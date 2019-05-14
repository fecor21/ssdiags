require(r4ss)
require(dplyr)
require(ggplot2)
# devtools::install_github("mkapur/kaputils")
## if prompted don't update anything
# library(kaputils)
require(gtools)

## identify directory that has executed models in it
rootdir <- "C:/Users/mkapur/Dropbox/UW/coursework/FISH-555/SA_Meeting_Final Runs-20190513T235227Z-001/SA_Meeting_Final Runs/Sensitivity_Analysis"## identify directory that has executed models in it

# rootdir <- "C:/Users/MKapur/Dropbox/UW/coursework/FISH-555/stm_mods/wp_test"
## create list of subdirs in this directory, you can use 'grep' if needed
mods <- list.dirs(rootdir, recursive = FALSE)[!grepl('plots|results',list.dirs(rootdir, recursive = FALSE))]
modnums <- sort(as.numeric(gsub(".*_", "\\1", mods)))
if(!exists(paste0(rootdir,"/plots"))) dir.create(paste0(rootdir,"/plots"))

## generate ALL r4ss comparison plots on suite of models (recommend n < 10)
## update covar and ncol as needed
mod.sum <- lapply(mixedsort(mods),SS_output, covar = FALSE) %>% SSsummarize(.)
SSplotComparisons(mod.sum, print = T, legendorder = c(modnums), plotdir = paste0(rootdir,"/plots"))

## **kaputils** generate CSV for post-hoc analyses ----
## will save to rootdir/results
## You pass a vector of fleetnames to subset for extraction or leave as "all"
kaputils:::extractResults(
  rootdir,
  terminal_year = 2017,
  pattern = 'Model',
  writeTables = TRUE,
  FleetName = 'All'
)

## Plotting using SPRSeries.csv generated above ----
sprs <- read.csv(paste0(rootdir,"/results/SPRseries.csv"))
## plot SPB over time for all models ----
ggplot(sprs, aes(x = Yr, y = SSB, col = MOD)) +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        legend.position = c(0.9,0.9)) +
  scale_y_continuous(limits = c(0,1.1*max(sprs$SSB))) +
  scale_color_manual(values = c('dodgerblue2','grey22')) +
  geom_line(lwd = 1.1, alpha = 0.5) +
  geom_point(col = 'black') +
  labs(x = 'Year', y = 'Spawning Stock Biomass', color = "")
ggsave(plot =last_plot(),
       file = paste0(rootdir,"/plots/ssb_custom.png"),
       width = 6, height = 4, unit = 'in',dpi = 420)

pars <- data.frame(mod.sum[8])
## plot recdevs for all models, with line ----
recdevs <- pars[grep('RecrDev',pars$pars.Label),] %>%
  select(pars.model1, pars.model2, pars.Yr) %>%
  plyr::rename(c("pars.model1" = basename(mods[1]),"pars.model2" = basename(mods[2]))) %>%
  reshape2::melt(id = 'pars.Yr')

ggplot(recdevs, aes(x =  pars.Yr, y = value, col = variable)) +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        legend.position = c(0.9,0.9)) +
  geom_hline(yintercept = 0, col = 'red') +
  scale_color_manual(values = c('dodgerblue2','grey22')) +
  geom_line(lwd = 1.1, alpha = 0.5) +
  geom_point() +
  labs(x = 'Year', y = 'Recruitment Deviates', color = "")
ggsave(plot =last_plot(),
       file = paste0(rootdir,"/plots/recdevs_custom.png"),
       width = 6, height = 4, unit = 'in',dpi = 420)



## **kaputils** generate kobe with multiple end-points ----
## will save to rootdir/plots
## this runs way faster if you pre-execute extractResults and reference the management_quantities.csv
## you must specify the name of the biomass column (varies by SS run)
kaputils:::plotKobe_compare(rootdir,
                           kobe.type = 'ISC',
                           axes.limits = c(2,2),
                           mq_csv = paste0(rootdir,"/results/management_quantities.csv"),
                           b.name = "SPB_SSBMSY",
                           f.name = 'F_FMSY',
                           pattern = 'Model',
                           subpattern = NA,
                           saveplot = T,
                           plotloc = paste0(rootdir,"/plots/"),
                           doLegend = T)

## **kaputils** extract ML and write csv to results/ ----
francisdat <- kaputils::SSMethod.TA1.8.MK(mod, type = 'len',fleet = 1:5, pick.gender = 0:3, plotit = T) %>% as.data.frame()
francisdat$Fleet <- mod$FleetNames[francisdat$Fleet] ## name the actual fleets
write.csv(francisdat,"results/francisDat.csv",row.names=F) ## save it
