#' runsSizeComp
#' \code{runsSizeComp} Do runs on size comps (mean length) using standardized residuals
#' @param summaryoutput an object generated via SS_output
#' @param standardized Should the test be performed using Unstandardized (0), Standardized (1) or both types of residuals (2)
#' @param type inputs to SSmethod.TA1.8
#' @param fleet inputs to SSmethod.TA1.8
#' @seealso \code{\link[r4ss]}

runsSizeComp <- function(summaryoutput,
                         standardized = c(0,1,2)[3],
                         type = 'len',
                         fleet = 1:nfleet(summaryoutput)){

  # source("./R/getTA1.R") ## load SSMethod.TA1.8.FC function
  francisdat <- getTA1(summaryoutput, type = 'len', fleet = fleet, plotit = F) %>% as.data.frame()
  francisdat$Fleet <- summaryoutput$FleetNames[francisdat$Fleet] ## name the actual fleets
  # write.csv(francisdat,"francisDat.csv",row.names=F) ## save it

  # load("smabase.rdata",verbose=T)
  summaryoutput$mnlen <- francisdat
  # save (smabase, file = 'smabase.rdata')


  if(!exists(paste0(getwd(),"./plots/"))) dir.create(paste0(getwd(),"./plots/"))

  std <- function(summaryoutput){
    par.save = par
    # Extract residual for by scenario and index
    d = summaryoutput$mnlen
    d$scenario = factor("s1")
    names(d)
    scenarios = (levels(d$scenario))
    d$residual = d$Std.res# log(d$Obs)-log(d$Exp)
    d$Name = d$Fleet
    agg = aggregate(residual~Yr+Name,d,mean)
    #combine
    idd = paste0(agg[,2])
    yall = 1:length(idd)
    iall = levels(factor(idd))
    yrs = agg[,1]

    Par = list(mfrow=c(ceiling(length(scenarios)/2),1),mar = c(1.2, 1.2, 1., 0.1),oma=c(0.5, 0.5, 0, 6), mgp =c(2.5,1,0), tck = -0.02,cex=0.8)
    png(file = paste0("./plots/runstest_MeanSize_std.png"), width =7.5, height = 3.5,
        res = 420, units = "in")
    par(Par)
    for(j in 1:length(scenarios)){
      ds = d[d$scenario==scenarios[j],]
      jdd = paste0(ds$Yr,".",ds$Name)
      yj = which(idd %in% jdd)
      ylim = c(-max(abs(d$residual)),max(abs(d$residual)))
      plot(yall,yall,type="n",xlab="Year",xlim=c(1,max(yall)),ylim=ylim,axes=F,xaxs = "i",yaxs="i")
      axis(1,at=yall,labels=yrs,cex.axis=0.6,mgp=c(0.1,0.,0))
      axis(2,cex.axis=0.6,mgp=c(0.5,0.0,0))
      title(main=paste(ds$scenario)[1], mgp=c(0.1,0,0),cex.main=0.8,line=0.2)
      abline(v=1)

      #for(i in 1:length(indices)){}
      i = 1
      ni = length(unique(ds$Name))
      indices = levels(factor(ds$Name))


      for(i in 1:ni){
        di = ds[ds$Name==indices[i],]
        yi = yall[which(agg$Name==indices[i])]
        get_runs = runs.sig3(di$residual)
        lims = get_runs$sig3lim
        polygon(c(yi,rev(yi)),c(rep(ylim[1],nrow(di)),rev(rep(ylim[2],nrow(di)))),col=ifelse(get_runs$p.runs<0.05,rgb(1,0,0,0.5),rgb(0,1,0,0.5)),border=0)
        polygon(c(yi,rev(yi)),c(rep(lims[1],nrow(di)),rev(rep(lims[2],nrow(di)))),col=grey(0.5,0.5),border=0)
        lines(yi,di$residual)
        points(yi,di$residual,pch=21,bg=ifelse(abs(di$residual)>abs(lims[1]),2,0),cex=0.7)
        text(mean(yi),ylim[2]*0.9,which(indices[i]==iall),cex=0.8)

      }
      if(j==1){  legend(par('usr')[2], par('usr')[4]-1, bty='n', xpd=NA,
                        c(paste0(1:length(iall),": ",iall)),cex=0.7)}
      if(j==1){legend(par('usr')[2]+2, par('usr')[4], bty='n', xpd=NA,
                      c("Passed","Failed"),pch=15,col=c(rgb(0,1,0,0.5),rgb(1,0,0,0.5)),pt.cex=2,cex=0.7)}


      abline(h=0,lty=2)
      abline(h=max(abs(d$residual)))
      for(i in 1:length(iall)){
        abline(v=0.5+max(which(agg$Name==levels(factor(agg$Name))[i])))
      }
      mtext(paste("Years"), side=1, outer=TRUE, at=0.5,line=-0.3,cex=.8)
      mtext(paste("Std. Residuals"), side=2, outer=TRUE, at=0.5,line=-0.3,cex=0.8)
    }

    dev.off()
    par=par.save


    par.save = par
    # Extract residual for by scenario and index
    # d = summaryoutput$mnlen
    # d$scenario = factor("s1")
    # names(d)
    # scenarios = (levels(d$scenario))
    # d$residual = d$Std.res# log(d$Obs)-log(d$Exp)
    # d$Name = d$Fleet
    # agg = aggregate(residual~Yr+Name,d,mean)
    # #combine
    # idd = paste0(agg[,2])
    # yall = 1:length(idd)
    # iall = levels(factor(idd))
    # yrs = agg[,1]
    #
    # Par = list(mfrow=c(ceiling(length(scenarios)/2),1),mar = c(1.2, 1.2, 1., 0.1),oma=c(0.5, 0.5, 0, 6), mgp =c(2.5,1,0), tck = -0.02,cex=0.8)
    # png(file = paste0("./plots/runstest_MeanSize_Std.png"), width =7.5, height = 3.5,
    #     res = 420, units = "in")
    # par(Par)
    # for(j in 1:length(scenarios)){
    #   ds = d[d$scenario==scenarios[j],]
    #   jdd = paste0(ds$Yr,".",ds$Name)
    #   yj = which(idd %in% jdd)
    #   ylim = c(-max(abs(d$residual)),max(abs(d$residual)))
    #   plot(yall,yall,type="n",xlab="Year",xlim=c(1,max(yall)),ylim=ylim,axes=F,xaxs = "i",yaxs="i")
    #   axis(1,at=yall,labels=yrs,cex.axis=0.6,mgp=c(0.1,0.,0))
    #   axis(2,cex.axis=0.6,mgp=c(0.5,0.0,0))
    #   title(main=paste(ds$scenario)[1], mgp=c(0.1,0,0),cex.main=0.8,line=0.2)
    #   abline(v=1)
    #
    #   #for(i in 1:length(indices)){}
    #   i = 1
    #   ni = length(unique(ds$Name))
    #   indices = levels(factor(ds$Name))
    #
    #
    #   for(i in 1:ni){
    #     di = ds[ds$Name==indices[i],]
    #     yi = yall[which(agg$Name==indices[i])]
    #     get_runs = runs.sig3(di$residual)
    #     lims = get_runs$sig3lim
    #     polygon(c(yi,rev(yi)),c(rep(ylim[1],nrow(di)),rev(rep(ylim[2],nrow(di)))),col=ifelse(get_runs$p.runs<0.05,rgb(1,0,0,0.5),rgb(0,1,0,0.5)),border=0)
    #     polygon(c(yi,rev(yi)),c(rep(lims[1],nrow(di)),rev(rep(lims[2],nrow(di)))),col=grey(0.5,0.5),border=0)
    #     lines(yi,di$residual)
    #     points(yi,di$residual,pch=21,bg=ifelse(abs(di$residual)>abs(lims[1]),2,0),cex=0.7)
    #     text(mean(yi),ylim[2]*0.9,which(indices[i]==iall),cex=0.8)
    #
    #   }
    #   if(j==1){  legend(par('usr')[2], par('usr')[4]-1, bty='n', xpd=NA,
    #                     c(paste0(1:length(iall),": ",iall)),cex=0.7)}
    #   if(j==1){legend(par('usr')[2]+2, par('usr')[4], bty='n', xpd=NA,
    #                   c("Passed","Failed"),pch=15,col=c(rgb(0,1,0,0.5),rgb(1,0,0,0.5)),pt.cex=2,cex=0.7)}
    #
    #
    #   abline(h=0,lty=2)
    #   abline(h=max(abs(d$residual)))
    #   for(i in 1:length(iall)){
    #     abline(v=0.5+max(which(agg$Name==levels(factor(agg$Name))[i])))
    #   }
    #   mtext(paste("Years"), side=1, outer=TRUE, at=0.5,line=-0.3,cex=.8)
    #   mtext(paste("Std. Residuals"), side=2, outer=TRUE, at=0.5,line=-0.3,cex=0.8)
    # }
    #
    # dev.off()
    # par=par.save
  } ## end std

  #-----------------------------------------
  #  Do runs on size comps (mean length) using unstandardized residuals
  #-----------------------------------------

  #smabase$mnlen

  unstd <- function(summaryoutput){
    par.save = par
    # Extract residual for by scenario and index
    d = summaryoutput$mnlen
    d$scenario = factor("s1")
    names(d)
    scenarios = (levels(d$scenario))
    d$residual =  log(d$Obsmn)-log(d$Expmn)
    d$Name = d$Fleet
    agg = aggregate(residual~Yr+Name,d,mean)
    #combine
    idd = paste0(agg[,2])
    yall = 1:length(idd)
    iall = levels(factor(idd))
    yrs = agg[,1]

    Par = list(mfrow=c(ceiling(length(scenarios)/2),1),mar = c(1.2, 1.2, 1., 0.1),oma=c(0.5, 0.5, 0, 6), mgp =c(2.5,1,0), tck = -0.02,cex=0.8)
    png(file = paste0("./plots/runstest_MeanSize_UnstdResid.png"), width =7.5, height = 3.5,
        res = 420, units = "in")
    par(Par)
    for(j in 1:length(scenarios)){
      ds = d[d$scenario==scenarios[j],]
      jdd = paste0(ds$Yr,".",ds$Name)
      yj = which(idd %in% jdd)
      ylim = c(-max(abs(d$residual)),max(abs(d$residual)))
      plot(yall,yall,type="n",xlab="Year",xlim=c(1,max(yall)),ylim=ylim,axes=F,xaxs = "i",yaxs="i")
      axis(1,at=yall,labels=yrs,cex.axis=0.6,mgp=c(0.1,0.,0))
      axis(2,cex.axis=0.6,mgp=c(0.5,0.0,0))
      title(main=paste(ds$scenario)[1], mgp=c(0.1,0,0),cex.main=0.8,line=0.2)
      abline(v=1)

      #for(i in 1:length(indices)){}
      i = 1
      ni = length(unique(ds$Name))
      indices = levels(factor(ds$Name))


      for(i in 1:ni){
        di = ds[ds$Name==indices[i],]
        yi = yall[which(agg$Name==indices[i])]
        get_runs = runs.sig3(di$residual)
        lims = get_runs$sig3lim
        polygon(c(yi,rev(yi)),c(rep(ylim[1],nrow(di)),rev(rep(ylim[2],nrow(di)))),col=ifelse(get_runs$p.runs<0.05,rgb(1,0,0,0.5),rgb(0,1,0,0.5)),border=0)
        polygon(c(yi,rev(yi)),c(rep(lims[1],nrow(di)),rev(rep(lims[2],nrow(di)))),col=grey(0.5,0.5),border=0)
        lines(yi,di$residual)
        points(yi,di$residual,pch=21,bg=ifelse(abs(di$residual)>abs(lims[1]),2,0),cex=0.7)
        text(mean(yi),ylim[2]*0.9,which(indices[i]==iall),cex=0.8)

      }
      if(j==1){  legend(par('usr')[2], par('usr')[4]-1, bty='n', xpd=NA,
                        c(paste0(1:length(iall),": ",iall)),cex=0.7)}
      if(j==1){legend(par('usr')[2]+2, par('usr')[4], bty='n', xpd=NA,
                      c("Passed","Failed"),pch=15,col=c(rgb(0,1,0,0.5),rgb(1,0,0,0.5)),pt.cex=2,cex=0.7)}


      abline(h=0,lty=2)
      abline(h=max(abs(d$residual)))
      for(i in 1:length(iall)){
        abline(v=0.5+max(which(agg$Name==levels(factor(agg$Name))[i])))
      }
      mtext(paste("Years"), side=1, outer=TRUE, at=0.5,line=-0.3,cex=.8)
      mtext(paste("Residuals"), side=2, outer=TRUE, at=0.5,line=-0.3,cex=0.8)
    }

    dev.off()
    par=par.save
  } ## end unstd

  if(standardized == 1) std(summaryoutput)
  if(standardized == 0) unstd(summaryoutput)
  if(standardized == 2) std(summaryoutput); unstd(summaryoutput)

}


