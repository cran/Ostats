## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "png", 
  dev.args = list(type = "cairo-png")
)

## ----setup--------------------------------------------------------------------
library(Ostats)
data(pitcher_traits)

## -----------------------------------------------------------------------------
set.seed(1)
idx <- sort(sample(nrow(pitcher_traits), 10, replace = FALSE))

knitr::kable(pitcher_traits[idx,])

## -----------------------------------------------------------------------------
pitcher_traits <- pitcher_traits[complete.cases(pitcher_traits), ]

pitcher_sites <- as.factor(pitcher_traits$site_id)

round(cor(pitcher_traits[, -(1:2)]), 2)

## -----------------------------------------------------------------------------
traits_to_use <- c("rosette_diameter_1", "pitcher_width", "mouth_diameter", "lip_thickness")

pitcher_trait_matrix <- as.matrix(pitcher_traits[, traits_to_use])

round(cor(pitcher_trait_matrix), 2)

## -----------------------------------------------------------------------------
apply(pitcher_trait_matrix, 2, range)

pitcher_trait_matrix_scaled <- scale(pitcher_trait_matrix, center = TRUE, scale = TRUE)

round(apply(pitcher_trait_matrix_scaled, 2, range), 2)

## ----univariate O-stats, message = FALSE, results = 'hide'--------------------
pitcher_univariate <- Ostats(traits = pitcher_trait_matrix_scaled,
                             plots = factor(rep(1, nrow(pitcher_trait_matrix_scaled))),
                             sp = pitcher_sites,
                             run_null_model = FALSE
)

## ----multivariate O-stats, message = FALSE, results = 'hide'------------------
pitcher_multivariate <- Ostats_multivariate(traits = pitcher_trait_matrix_scaled,
                                            plots = factor(rep(1, nrow(pitcher_trait_matrix_scaled))),
                                            sp = pitcher_sites,
                                            random_seed = 333,
                                            run_null_model = FALSE, 
                                            hypervolume_args = list(method = 'box'),
                                            hypervolume_set_args = list(num.points.max = 1000)
)

## -----------------------------------------------------------------------------
pitcher_univariate$overlaps_norm

pitcher_multivariate$overlaps_norm

## ----univariate null, message = FALSE, results = 'hide'-----------------------
pitcher_univariate_withnull <- Ostats(traits = pitcher_trait_matrix_scaled,
                                      plots = factor(rep(1, nrow(pitcher_trait_matrix_scaled))),
                                      sp = pitcher_sites,
                                      random_seed = 666,
                                      run_null_model = TRUE,
                                      nperm = 50
)

## ----multivariate null, message = FALSE, results = 'hide'---------------------
pitcher_multivariate_withnull <- Ostats_multivariate(traits = pitcher_trait_matrix_scaled,
                                            plots = factor(rep(1, nrow(pitcher_trait_matrix_scaled))),
                                            sp = pitcher_sites,
                                            random_seed = 555,
                                            run_null_model = TRUE,
                                            nperm = 50,
                                            hypervolume_args = list(method = 'box'),
                                            hypervolume_set_args = list(num.points.max = 1000)
)

## ---- echo = FALSE------------------------------------------------------------
uni_result <- with(pitcher_univariate_withnull, rbind(overlaps_norm, 
                                                      overlaps_norm_ses$raw_lower,
                                                      overlaps_norm_ses$raw_upper))
multi_result <- with(pitcher_multivariate_withnull, c(overlaps_norm,
                                                      overlaps_norm_ses$raw_lower,
                                                      overlaps_norm_ses$raw_upper))
knitr::kable(data.frame(Ostat = c('observed', 'null 2.5%', 'null 97.5%'),
                        cbind(uni_result, multi_result)),
             row.names = FALSE,
             col.names = c('O-statistic', 'rosette diameter', 'pitcher width', 'mouth diameter', 'lip thickness', 'multivariate'),
             digits = 3)

## ----default plot, message = FALSE, fig.height = 9, fig.width = 9-------------
Ostats_multivariate_plot(plots = factor(rep(1, nrow(pitcher_trait_matrix))),
                         sp = pitcher_sites,
                         traits = pitcher_trait_matrix,
                         contour_level = 0.0001,
                         overlap_dat = pitcher_multivariate
)

## ----custom plot, message = FALSE, fig.height = 9, fig.width = 9--------------
Ostats_multivariate_plot(plots = factor(rep(1, nrow(pitcher_trait_matrix))),
                         sp = pitcher_sites,
                         traits = pitcher_trait_matrix,
                         contour_level = 0.0001,
                         overlap_dat = pitcher_multivariate,
                         colorvalues = rainbow(length(unique(pitcher_sites))),
                         axis_expansion = 0.05,
                         plot_points = FALSE
)

