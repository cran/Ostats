## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "png", 
  dev.args = list(type = "cairo-png")
)

## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github('NEON-biodiversity/Ostats')

## -----------------------------------------------------------------------------
# Load the Ostats package. 
library(Ostats)

dat <- small_mammal_data[small_mammal_data$siteID %in% c('HARV', 'JORN'), 
                         c('siteID', 'taxonID', 'weight')]
dat <- dat[!is.na(dat$weight), ]
dat$log_weight <- log10(dat$weight)

## -----------------------------------------------------------------------------
do.call(rbind, lapply(split(dat, interaction(dat$siteID, dat$taxonID), drop = TRUE), 
                      function(x) x[1,]))

## ----echo = T, results='hide'-------------------------------------------------
 Ostats_example <- Ostats(traits = as.matrix(dat[,'log_weight', drop = FALSE]),
                    sp = factor(dat$taxonID),
                    plots = factor(dat$siteID),
                    random_seed = 517)

## ----eval=FALSE---------------------------------------------------------------
#  Ostats_example2 <- Ostats(traits = as.matrix(dat[,'log_weight', drop = FALSE]),
#                      sp = factor(dat$taxonID),
#                      plots = factor(dat$siteID),
#                      density_args=list(bw = 'nrd0', adjust = 2, n=200),
#                      random_seed = 518)
#  

## -----------------------------------------------------------------------------
Ostats_example$overlaps_norm

## -----------------------------------------------------------------------------
Ostats_example$overlaps_unnorm

## -----------------------------------------------------------------------------
# View normalized and non-normalized standardized effect size outputs from null model analysis
Ostats_example$overlaps_norm_ses
Ostats_example$overlaps_unnorm_ses

## -----------------------------------------------------------------------------
head(ant_data)

## ----echo = T, results='hide'-------------------------------------------------
# Calculate overlap statistics for hourly data using the ant_data dataset
circular_example <- Ostats(traits = as.matrix(ant_data[, 'time', drop = FALSE]),
                    sp = factor(ant_data$species),
                    plots = factor(ant_data$chamber),
                    discrete = TRUE,
                    circular = TRUE,
                    unique_values = 0:23,
                    random_seed = 519)

## -----------------------------------------------------------------------------
circular_example$overlaps_norm
circular_example$overlaps_unnorm
circular_example$overlaps_norm_ses
circular_example$overlaps_unnorm_ses

## ----out.width="80%", warning=FALSE, error=FALSE, fig.align="center", fig.height=4.5, fig.width=7----
siteID <- small_mammal_data$siteID
taxonID <- small_mammal_data$taxonID
trait <- log10(small_mammal_data$weight)

sites2use<- c('HARV','JORN') 

Ostats_plot(plots = siteID, 
            sp = taxonID, 
            traits = trait, 
            overlap_dat = small_mammal_Ostats, 
            use_plots = sites2use, 
            name_x = 'log10 Body Weight (g)', 
            means = TRUE)


