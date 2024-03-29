#' @title Calculate multivariate O-statistics
#'
#' @description This function calculates a single O-statistic across multiple traits
#' by estimating hypervolumes for each species in multivariate trait space that
#' contain the trait values for all the individuals in each species. It uses a
#' stochastic estimation procedure implemented in the R package hypervolume. The
#' community-level O-statistics estimated from the hypervolume overlaps can be
#' evaluated against a null model.
#'
#' @param traits matrix of trait measurements. The number of rows in the matrix
#'   is the number of individuals,
#'   and the number of columns of the matrix is the number of traits.
#' @param plots a factor with length equal to nrow(traits) that indicates the
#'   community each individual belongs to.
#' @param sp a factor with length equal to nrow(traits) that indicates the taxon
#'   of each individual.
#' @param output specifies whether median or mean is calculated.
#' @param weight_type specifies weights to be used to calculate the median or mean.
#' @param run_null_model whether to run a null model (if \code{TRUE}) and evaluate the
#'  O-statistics against it, or simply return the raw O-statistics (if \code{FALSE}).
#'  Defaults to \code{TRUE}.
#' @param nperm the number of null model permutations to generate. Defaults to 99.
#' @param nullqs numeric vector of probabilities with values in [0,1] to set
#'   effect size quantiles. Defaults to \code{c(0.025, 0.975)}.
#' @param shuffle_weights If TRUE, shuffle weights given to pairwise overlaps
#'   within a community when generating null models.
#' @param swap_means If TRUE, swap means of body sizes within a community when
#'   generating null models.
#' @param random_seed User may supply a random seed to enable reproducibility
#'   of null model output. A warning is issued, and a random seed is generated
#'   based on the local time, if the user does not supply a seed.
#' @param hypervolume_args additional arguments to pass to \code{\link[hypervolume]{hypervolume}},
#'   such as \code{method}. If none are provided, default values are used.
#'   By default, \code{method = "gaussian"}.
#' @param hypervolume_set_args additional arguments to pass to \code{\link[hypervolume]{hypervolume_set}},
#'   such as \code{num.points.max}. If none are provided, default values are used.
#' @param verbose If \code{TRUE}, progress messages are displayed. Defaults to \code{FALSE}.
#'
#' @details This function calculates multivariate O-statistics and optionally evaluates them
#'   against a local null model. By default, it calculates the median of pairwise
#'   hypervolume overlaps, weighted by harmonic mean of species abundaces of the species
#'   pairs in each community. Two results are produced, one normalizing the area
#'   under all density functions to 1, the other making the area under all density
#'   functions proportional to the number of observations in that group.
#'
#'   Functions from the R package hypervolume by Blonder and colleagues (Blonder 2018) are
#'   used internally. The function \code{\link[hypervolume]{hypervolume}} stochastically
#'   estimates hypervolumes for each species in each community, and the function
#'   \code{\link[hypervolume]{hypervolume_set}} finds the proportional overlap
#'   between each pair of hypervolumes.
#'
#'   If \code{run_null_model} is \code{TRUE}, the O-statistics are evaluated relative
#'   to a null model. When both \code{shuffle_weights} and \code{swap_means} are \code{FALSE},
#'   null communities are generated by randomly assigning a taxon that is present in the community to
#'   each individual. If \code{shuffle_weights} is \code{TRUE}, species abundances
#'   are also randomly assigned to each species to weight the O-statistic for each
#'   null community. If \code{swap_means} is \code{TRUE}, instead of sampling individuals
#'   randomly, species means are sampled randomly among species, keeping the deviation
#'   of each individual from its species mean the same. After the null communities
#'   are generated, O-stats are calculated for each null community to compare with
#'   the observed O-stat.
#'
#'   Effect size statistics are calculated by z-transforming the O-statistics
#'   using the mean and standard deviation of the null distribution.
#'
#' @return The function returns a list containing four objects:
#' \item{overlaps_norm}{a matrix showing the O-statistic for each community,
#'   with the area under all density functions normalized to 1.}
#' \item{overlaps_unnorm}{a matrix showing O-stats calculated with the area under all density
#'   functions proportional to the number of observations in that group.}
#' \item{overlaps_norm_ses}{List of matrices of effect size statistics against a null model
#'   with the area under all density functions normalized to 1. \code{ses} contains the
#'   effect sizes (z-scores), \code{ses_lower} contains the effect size lower critical values
#'   for significance at the level determined by \code{nullqs}, and \code{ses_upper}
#'   contains the upper critical values. \code{raw_lower} and \code{raw_upper} are
#'   the critical values in raw units ranging from 0 to 1.}
#' \item{overlaps_unnorm_ses}{List of matrices of effect size statistics against a null model
#'   with the area under all density functions proportional to the number
#'   of observations in that group. Elements are as in \code{overlaps_norm_ses}.}
#'
#' @references Blonder, B. Hypervolume concepts in niche- and trait-based ecology.
#'   Ecography 41, 1441–1455 (2018). https://doi.org/10.1111/ecog.03187
#'
#' @seealso \code{\link{Ostats}} for univariate data.
#' @seealso \code{\link{Ostats_multivariate_plot}} for plotting multivariate trait overlap in
#' each community.
#'
#' @examples
#' \dontrun{
#' # overlap statistic between populations of pitcher plants at two different sites
#'
#' # Select two sites and three dimensions and scale data
#' site_index <- pitcher_traits[, 'site_id'] %in% c('FLK', 'MYR')
#' dat <- as.matrix(pitcher_traits[site_index,
#'                                c('rosette_diameter_1', 'pitcher_width', 'mouth_diameter')])
#' dat <- scale(dat, center = TRUE, scale = TRUE)
#'
#' # Here a low number is used for num.points.max for example purposes
#' Ostats_multi_example <- Ostats_multivariate(traits = dat,
#'                                             plots = factor(rep(1, nrow(dat))),
#'                                             sp = factor(pitcher_traits$site_id[site_index]),
#'                                             random_seed = 111,
#'                                             run_null_model = FALSE,
#'                                             hypervolume_args = list(method = 'box'),
#'                                             hypervolume_set_args = list(num.points.max = 100)
#' )
#' }
#'
#' @export
#'
Ostats_multivariate <- function(traits, plots, sp, output = "median", weight_type = "hmean", run_null_model = TRUE, nperm = 99, nullqs = c(0.025, 0.975), shuffle_weights = FALSE, swap_means = FALSE, random_seed = NULL, hypervolume_args = list(), hypervolume_set_args = list(), verbose = FALSE) {
  # Required input: a matrix called traits (nrows=n individuals, ncols=n traits),
  # a vector called plots which is a factor with length equal to nrow(traits),
  # a vector called sp which is a factor with length equal to nrow(traits),

  # warning and error messages to check the inputs
  if(is.numeric(traits) == FALSE) stop("traits must be a numeric matrix.")
  if(length(unique(sp)) == 1) warning("only one taxon is present; overlap cannot be calculated.")

  # If traits is a vector, coerce to a single column matrix.
  if(is.vector(traits)) traits <- matrix(data = traits, ncol = 1, dimnames = list(names(traits)))

  # If user did not supply a random seed, generate one and print a message.
  if (run_null_model && missing(random_seed)) {
    random_seed <- round(as.numeric(Sys.time()) %% 12345)
    message(paste("Note: argument random_seed was not supplied; setting seed to", random_seed))
  }

  # If the abundances of species are different, print a message.
  if (length(unique(table(sp))) > 1) message("Note: species abundances differ. Consider sampling equivalent numbers of individuals per species.")

  # Set random seed.
  set.seed(random_seed)

  # Declaration of data structures to hold the results
  # Data structures for observed O-Stats
  overlaps_norm <- matrix(nrow = nlevels(plots), ncol = 1)
  overlaps_unnorm <- matrix(nrow = nlevels(plots), ncol = 1)

  # Data structures for null O-Stats
  overlaps_norm_null <- array(dim = c(nlevels(plots), 1, nperm))
  overlaps_unnorm_null <- array(dim = c(nlevels(plots), 1, nperm))

  # Name rows of the outputs (in multivariate case we do not name columns after traits because there is only one column in output).
  dimnames(overlaps_norm) <- list(levels(plots))
  dimnames(overlaps_unnorm) <- list(levels(plots))

  # Calculation of observed O-Stats

  if (verbose) {
    message('Calculating observed local O-stats for each community . . .')
    pb <- utils::txtProgressBar(min = 0, max = nlevels(plots), style = 3)
  }

  for (s in 1:nlevels(plots)) {
    overlap_norm_s <- try(community_overlap(traits = traits[plots == levels(plots)[s], ], sp = sp[plots == levels(plots)[s]], output = output, weight_type = weight_type, normal = TRUE, density_args = hypervolume_args, hypervolume_set_args = hypervolume_set_args), TRUE)
    overlaps_norm[s, 1] <- if (inherits(overlap_norm_s, 'try-error')) NA else overlap_norm_s
    overlap_unnorm_s <- try(community_overlap(traits = traits[plots == levels(plots)[s], ], sp = sp[plots == levels(plots)[s]], output = output, weight_type = weight_type, normal = FALSE, density_args = hypervolume_args, hypervolume_set_args = hypervolume_set_args), TRUE)
    overlaps_unnorm[s, 1] <- if (inherits(overlap_unnorm_s, 'try-error')) NA else overlap_unnorm_s

    if (verbose) utils::setTxtProgressBar(pb, s)
  }

  if (verbose) close(pb)

  if (run_null_model) {

    if (verbose) {
      message('Calculating null distributions of O-stats . . . ')
      pb <- utils::txtProgressBar(min = 0, max = nperm, style = 3)
    }

    # Null model generation and calculation of null O-Stats

    # Local null model: generation and calculation done in the same loop
    for (i in 1:nperm) {
      for (s in 1:nlevels(plots)) {

        if (!swap_means) {
          overlap_norm_si <- try(community_overlap(traits = traits[plots == levels(plots)[s], ], sp = sample(sp[plots == levels(plots)[s]]), output = output, weight_type = weight_type, normal = TRUE, randomize_weights = shuffle_weights, density_args = hypervolume_args, hypervolume_set_args = hypervolume_set_args), TRUE)
          overlap_unnorm_si <- try(community_overlap(traits = traits[plots == levels(plots)[s], ], sp = sample(sp[plots == levels(plots)[s]]), output = output, weight_type = weight_type, normal = FALSE, randomize_weights = shuffle_weights, density_args = hypervolume_args, hypervolume_set_args = hypervolume_set_args), TRUE)
        } else {
          traits_s <- traits[plots==levels(plots)[s], ]
          sp_s <- sp[plots==levels(plots)[s]]

          traitmeans <- tapply(traits_s,sp_s,mean)
          traitdeviations <- traits_s-traitmeans[sp_s]

          # Sort the trait means out randomly.
          traitmeans_null <- sample(traitmeans)
          sp_null <- rep(names(traitmeans_null), table(sp_s))
          traits_null <- traitdeviations + traitmeans_null[sp_null]
          overlap_norm_si <- try(community_overlap(traits = traits_null, sp = sp_null, output = output, weight_type = weight_type, normal = TRUE, randomize_weights = shuffle_weights, density_args = hypervolume_args, hypervolume_set_args = hypervolume_set_args), TRUE)
          overlap_unnorm_si <- try(community_overlap(traits = traits_null, sp = sp_null, output = output, weight_type = weight_type, normal = FALSE, randomize_weights = shuffle_weights, density_args = hypervolume_args, hypervolume_set_args = hypervolume_set_args), TRUE)
        }

        overlaps_norm_null[s, 1, i] <- if (inherits(overlap_norm_si, 'try-error')) NA else overlap_norm_si

        overlaps_unnorm_null[s, 1, i] <- if (inherits(overlap_unnorm_si, 'try-error')) NA else overlap_unnorm_si

      }
      if (verbose) utils::setTxtProgressBar(pb, i)
    }

    if (verbose) close(pb)

    # Extract quantiles to get standardized effect sizes for the overlap stats
    overlaps_norm_ses <- get_ses(overlaps_norm, overlaps_norm_null, nullqs)
    overlaps_unnorm_ses <- get_ses(overlaps_unnorm, overlaps_unnorm_null, nullqs)
    list(overlaps_norm=overlaps_norm, overlaps_unnorm=overlaps_unnorm,
         overlaps_norm_ses=overlaps_norm_ses, overlaps_unnorm_ses=overlaps_unnorm_ses)

  } else {
    list(overlaps_norm=overlaps_norm, overlaps_unnorm=overlaps_unnorm)
  }

}
