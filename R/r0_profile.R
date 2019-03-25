#' r0_profile
#' \code{r0_profile} Conduct R0 profiling and generate plot
#' @param moddir the location of the model
#' @param plot do you want to create a profile plot?
#' @param parm.min start value for profiling
#' @param parm.max stop value for profiling
#' @param parm.step increment for increasing parm


#' @seealso \code{\link[r4ss]}

r0_profile <- function(moddir, plot = TRUE,
                       parm.min = 4.5, parm.max = 6.5,
                       parm.step = 0.1){
  wd <- paste0(moddir,"/R0_profile/")
  if(!exists(wd)) dir.create(wd)
  setwd(wd)
  #set working directory
  # wd=c("C:/Users/felip/Dropbox/MLS_2019_1994/R0_profile")
  # setwd(wd2)
  ## devtools::install_github("r4ss/r4ss") # to update r4ss
  ## load r4ss
  # library(r4ss)

  ### Running R0 Profile:
  ## 1. Make a folder called R0_Prof
  ## 2. Inside put the complete model into a folder called "orig".
  ## 3. in the control file set the phase for the R0 parameter to -1
  ## 4. in the starter file make sure the option to use the par file is 1
  ## 5. update the working directories in run_R0_parallel.R and plot_r0_profile.r
  ## 6. at the end of this file, ensure the location of plot_r0_profile.r is correct
  ## 7. in the R0_Prof folder, make a folder call 00_mle, put the files from orig into 00_mle and run it manually

  # rm(list=ls())

  # library('foreach')
  #library('doMC') # Comment out for windows
  # library('doSNOW') # Uncomment for Windows
  # setwd("C:/Users/felip/Dropbox/MLS_2019_1994/R0_profile")
  # parm.min <- 4.5
  # parm.max <-6.5
  # parm.step <- 0.1
  parmstr.parfile <- '# SR_parm\\[1]:' # Note that you need to add double backslash for escape character for grep
  parfile <- 'ss.par'
  ssdir.orig <- 'orig'

  numcpus <- 4
  #runss.str <- './SS324ab.bin -nohess -nox' # Comment out for windows
  runss.str <- 'ss.exe -nohess -nox' # Uncomment for Windows

  origwd <- moddir

  parm.vec <- seq(parm.min, parm.max, parm.step)
  numdir <- length(parm.vec)

  for (ii in 1:numdir) {
    dir.name <- paste(sprintf('%02d',ii),sprintf('%.2f',parm.vec[ii]),sep='_')
    #	system(paste('cp -r', ssdir.orig, dir.name, sep=' ')) # Comment out for windows
    system(paste('xcopy ', ssdir.orig, ' ', dir.name, '\\* ', '/E', sep='')) # Uncomment for Windows

    parfile.infile <- paste(dir.name,'/',parfile, sep='')
    conn <- file(parfile.infile, open='r')
    parfile.intxt <- readLines(conn)
    close(conn)
    parfile.outtxt <- parfile.intxt
    wantedline <- grep(parmstr.parfile,parfile.intxt)
    parfile.outtxt[wantedline+1] <-  parm.vec[ii]
    conn <- file(parfile.infile, open='w')
    writeLines(parfile.outtxt, conn)
    close(conn)
  }

  #registerDoMC(numcpus) # Comment out for windows
  cl<-makeCluster(numcpus) # Uncomment for Windows
  registerDoSNOW(cl) # Uncomment for Windows

  foreach(ii=1:numdir) %dopar% {
    dir.name <- paste(sprintf('%02d',ii),sprintf('%.2f',parm.vec[ii]),sep='_')
    setwd(paste(origwd,'/',dir.name,sep=''))
    print(paste(origwd,'/',dir.name,sep=''))
    system(runss.str)
    setwd(origwd)
  }

  stopCluster(cl) # Uncomment for Windows

  setwd(origwd)

  #setwd(paste(origwd,'/00_mle',sep=''))
  #system(runss.str)
  #setwd(origwd)

  ##make sure
  # source('C:/Users/felip/Dropbox/MLS_2019_1994/R0_profile/plot_r0_profile.r')
  #rm(list=ls())

  # library(r4ss)
  # library(colorRamps)
  # setwd("C:/Users/felip/Dropbox/MLS_2019_1994/R0_profile")
  ### Functions ####

  ##################

  showplot_yn <- F
  pdf_yn <- T
  png_yn <- T
  csv_yn <- T
  mainfolder <- './'
  pdf.filename <- paste('r0profile',format(Sys.time(), "%Y%m%d_%H%M.pdf"),sep='_')
  png.filename <- paste('r0profile',format(Sys.time(), "%Y%m%d_%H%M"),sep='_')
  csv.filename <- paste('r0profile',format(Sys.time(), "%Y%m%d_%H%M.csv"),sep='_')

  #mainlike_components <- c('TOTAL','Survey','Length_comp','Age_comp','SizeFreq','Recruitment')
  mainlike_components <- c('TOTAL','Survey','Length_comp','Recruitment')
  #fleetlike_components <- c('Surv_like','Length_like','SizeFreq_like:_1','SizeFreq_like:_2','SizeFreq_like:_3','Age_like')
  #fleetlike_components_labels <- c('survey likelihood','2 cm bin likelihood', '7 cm bin likelihood','2 cm bin - Age-0 likelihood', '7 cm bin - Age-0 likelihood','Cond-Age-at-len likelihood')
  fleetlike_components <- c('Surv_like','Length_like','Catch_like')
  fleetlike_components_labels <- c('index likelihood','Length_like','Catch Likelihood')
  ssdirpattern <- '^[0-9]{2}'
  profile.string <- 'R0'
  profile.label <- expression(log(italic(R)[0]))

  setwd(mainfolder)
  dirvec <- dir(pattern=ssdirpattern)
  SSreps <- SSgetoutput(dirvec=dirvec,getcovar=F, ncols=1000,forecast=FALSE)
  summaryoutput <- SSsummarize(SSreps[c(1,3:17)])
  lbf  <- summaryoutput$likelihoods_by_fleet
  FleetNames <- summaryoutput$FleetNames[[1]]


  sizefreqlike_component <- (grep('Survey',fleetlike_components))
  if (length(sizefreqlike_component) > 0) {
    for (ii in 1:length(sizefreqlike_component)) {
      sizefreqlike_label <- fleetlike_components[sizefreqlike_component[ii]]
      sizefreqlambda_label <-  sub('like','lambda',sizefreqlike_label)

      sizefreqlambda <- summaryoutput$likelihoods_by_fleet[summaryoutput$likelihoods_by_fleet$Label == sizefreqlambda_label, 4:ncol(summaryoutput$likelihoods_by_fleet)]
      sizefreqlike <- summaryoutput$likelihoods_by_fleet[summaryoutput$likelihoods_by_fleet$Label == sizefreqlike_label, 4:ncol(summaryoutput$likelihoods_by_fleet)] * sizefreqlambda
      summaryoutput$likelihoods_by_fleet[summaryoutput$likelihoods_by_fleet$Label == sizefreqlike_label, names(summaryoutput$likelihoods_by_fleet)=='ALL'] <- rowSums(sizefreqlike, na.rm=T)
    }
  }

  plotstuff <- function(){
    SSplotProfile(summaryoutput,plot=T,print=F,profile.string='R0',profile.label=expression(log(italic(R)[0])),components=mainlike_components,component.labels=mainlike_components,col=c('black',blue2green2red(length(mainlike_components)-1)),legendloc='top')
    for (ii in 1:length(fleetlike_components)) {
      minlike_byfleet <- apply(as.matrix(lbf[(which(lbf$Label %in% fleetlike_components[ii])), colnames(lbf) %in% FleetNames]),2,min)
      maxlike_byfleet <- apply(as.matrix(lbf[(which(lbf$Label %in% fleetlike_components[ii])), colnames(lbf) %in% FleetNames]),2,max)
      difflike_byfleet <- maxlike_byfleet - minlike_byfleet
      lambdas_byfleet <- colMeans(as.matrix(lbf[(which(lbf$Label %in% fleetlike_components[ii]))-1, colnames(lbf) %in% FleetNames]))
      wantedFleets <-  which(lambdas_byfleet > 0)
      ymax <- 1.3*max(difflike_byfleet[wantedFleets])
      PinerPlot(summaryoutput,plot=T,print=F,profile.string='R0',component=fleetlike_components[ii],fleets=wantedFleets,col=c('black',blue2green2red(length(wantedFleets))),main=paste('Changes in',fleetlike_components_labels[ii],'by fleet'),ymax=ymax,legendloc='top', minfraction = 0.0001)
    }
  }

  printstuff <- function() {
    like.table <- list()
    like.table[[1]] <- SSplotProfile(summaryoutput,plot=F,print=F,profile.string='R0',components=mainlike_components,component.labels=mainlike_components)
    for (ii in 1:length(fleetlike_components)) {
      like.table[[ii+1]] <- PinerPlot(summaryoutput,plot=F,print=F,profile.string='R0',component=fleetlike_components[ii])
    }
    return(like.table)
  }

  if (showplot_yn) plotstuff()

  if (pdf_yn) {
    pdf(file=pdf.filename)
    plotstuff()
    dev.off()
  }

  if (png_yn) {
    png(file=paste(png.filename,'_%d.png',sep=''))
    plotstuff()
    dev.off()
  }
  if (csv_yn) {
    out <- printstuff()
    write.table(out, file=csv.filename, sep=',', row.names=F)
  }






}
