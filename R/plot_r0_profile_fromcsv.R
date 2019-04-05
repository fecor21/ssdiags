#rm(list=ls())

library(r4ss)
library(colorRamps)
library(ggplot2)
library(dplyr); library(reshape2); require(gridExtra)
# setwd("C:/Users/felip/Dropbox/MLS_2019_1994/R0_Profile_Model_382")
setwd("C:/Users/mkapur/Documents/GitHub/ssdiags")
df <- read.csv("./R/r0profile_20190328_0858.csv")

profile.label <- expression(log(italic(R)[0]))
fleetmatch <-which(grepl("F.*_", names(df)))
survmatch <- which(grepl("S0.*_", names(df)))

## catch likelihoods -- all cols after "ALL.2"
catchmatch  <- grep('ALL.2',names(df)):length(names(df))

tomatch <- c(which(grepl("F.*_", names(df))),
             which(grepl("S0.*_", names(df))))
FleetNames <- names(df)[tomatch][!is.na(names(df)[tomatch])]


## Totally Raw Plotting Reboot -- sorry
plist <- list()
## R0 color by component ---
plist[[1]] <- df[,1:5] %>% melt(id = c('SR_LN.R0.')) %>%
ggplot(., aes(x = SR_LN.R0., y = value, color = variable,pch = variable)) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.8,0.8))+
  geom_line(lwd = 1.1) +
  geom_point() +
  scale_color_manual(values=c('black',rainbow(3))) +
  labs(x = profile.label, pch = '', color = '',
       y = 'Change in Log-Likelihood')
ggsave(plot = last_plot(),  file = paste0("PinerPlot.tiff"),
       width = 4, height = 6, units = 'in', dpi = 480)
## R0 color by fleet ---
plist[[2]] <- cbind(df$Label, df$SR_LN.R0.,df[,survmatch],df$ALL) %>%
  filter(df$Label == 'Surv_like') %>%
  # select(-c("df$Label") )%>%
  plyr::rename(c("df$SR_LN.R0." = "SR_LN.R0.", "df$ALL"='ALL')) %>%
  melt(id = c('SR_LN.R0.')) %>%
  ggplot(., aes(x = SR_LN.R0., y = value, color = variable,pch = variable)) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.5,0.8))+
  geom_line(lwd = 1.1) +
  geom_point() +
  scale_color_manual(values=c(rainbow(length(survmatch)),'black')) +
  labs(x = profile.label, pch = '', color = '',
       y = 'Change in Log-Likelihood',
       title = 'Changes in Index Likelihood by Fleet')

ggsave(plot = last_plot(),  file = paste0("Index Likelihood by Fleet.tiff"),
       width = 4, height = 6, units = 'in', dpi = 480)

## LengthLike color by fleet ----
## these are denoted by ".1"

plist[[3]] <- cbind(df$Label.1, df$SR_LN.R0.,df[,fleetmatch],df$ALL.1) %>%
  filter(df$Label.1 == 'Length_like') %>%
  select(-"df$Label.1") %>%
  plyr::rename(c("df$SR_LN.R0." = "SR_LN.R0.", "df$ALL.1"='ALL.1')) %>%
  melt(id = c('SR_LN.R0.')) %>%
  ggplot(., aes(x = SR_LN.R0., y = value, color = variable)) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.5,0.6),
        legend.background = element_blank())+
  geom_line(lwd = 1.1) +
  scale_color_manual(values=c(rainbow(length(fleetmatch)),'black')) +
  labs(x = profile.label,  color = '',
       y = 'Change in Log-Likelihood',
       title = 'Changes in Length_Like by Fleet')
ggsave(plot = last_plot(),  file = paste0("Length_Like by Fleet.tiff"),
       width = 4, height = 6, units = 'in', dpi = 480)

## Changes in catchlike by fleet ----
## denoted by ".2"
plist[[4]] <-cbind(df$Label.2, df$SR_LN.R0.,df[,catchmatch],df$ALL.2) %>%
  filter(df$Label.2 == 'Catch_like') %>%
  select(-"df$Label.2") %>%
  plyr::rename(c("df$SR_LN.R0." = "SR_LN.R0.", "df$ALL.2"='ALL.2')) %>%
  melt(id = c('SR_LN.R0.')) %>%
  ggplot(., aes(x = SR_LN.R0., y = value, color = variable,pch = variable)) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.5,0.8),
        legend.background = element_blank())+
  geom_line(lwd = 1.1) +
  geom_point() +
  scale_color_manual(values=c('black', rainbow(length(catchmatch)))) +
  labs(x = "log(R0)", pch = '', color = '',
       y = 'Change in Log-Likelihood',
       title = 'Changes in Catch Likelihood by Fleet')
ggsave(plot = last_plot(),  file = paste0("Catch Likelihood by Fleet.tiff"),
       width = 4, height = 6, units = 'in', dpi = 480)


## save it ----
lay <- cbind(c(1,2),
             c(3,4))
grid.arrange(grobs = plist, layout_matrix = lay) %>%
  ggsave(plot = .,  file = paste0("all_ikelihoods_from_csv.tiff), width = 8, height = 12, units = 'in', dpi = 480)


