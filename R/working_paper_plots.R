require(r4ss)
require(dplyr)
devtools::install_github("mkapur/kaputils")
## if prompted don't update anything
# library(kaputils)

## identify directory that has executed models in it
rootdir <- "C:/Users/Maia Kapur/Dropbox/UW/coursework/FISH-555/stm_mods/wp_test"
## create list of subdirs in this directory, you can use 'grep' if needed
mods <- list.dirs(rootdir, recursive = FALSE)[!grepl('plots',list.dirs(rootdir, recursive = FALSE))]
if(!exists(paste0(rootdir,"/plots"))) dir.create(paste0(rootdir,"/plots"))

## generate all r4ss comparison plots on suite of models (recommend n < 10)
## update covar and ncol as needed
mod.sum <- lapply(mods,SS_output, covar = FALSE) %>% SSsummarize(.)
SSplotComparisons(mod.sum, print = T, plotdir = paste0(rootdir,"/plots"))


## **kaputils** generate CSV for post-hoc analyses ----
## will save to rootdir/results
## You pass a vector of fleetnames to subset for extraction or leave as "all"
kaputils::extractResults(
  rootdir,
  terminal_year = 2017,
  pattern = 'Model',
  subpattern = NA,
  writeTables = TRUE,
  FleetName = 'All'
)

## **kaputils** generate kobe with multiple end-points ----
## will save to rootdir/plots
## this runs way faster if you pre-execute extractResults and reference the management_quantities.csv
## you must specify the name of the biomass column (varies by SS run)
kaputils::plotKobe_compare(rootdir,
                           kobe.type = 'ISC',
                           mq_csv = paste0(rootdir,"/results/management_quantities.csv"),
                           b.name = "SPB_SSBMSY",
                           f.name = 'F_FMSY',
                           pattern = 'Model',
                           subpattern = NA,
                           saveplot = T,
                           plotloc = paste0(rootdir,"/plots/"),
                           doLegend = T)
