#' produceRuns
#' \code{produceRuns} Do runs for ...
#' @param summaryoutput an object generated via SS_output
#' @seealso \code{\link[r4ss]}

produceRuns <- function(summaryoutput){
  # source("./R/runsSig3.R")
  if(!exists(paste0(getwd(),"./plots/"))) dir.create(paste0(getwd(),"./plots/"))
  #---------------------
  # Index color palette
  #---------------------
  # jabba.colors = as.character(c('#e6194b', "#3cb44b", "#ffe119",
  #                               "#0082c8","#f58231", "#911eb4",
  #                               "#46f0f0", "#f032e6", "#d2f53c",
  #                               "#fabebe", "#008080","#e6beff", "#aa6e28",rainbow(12)[seq(1,12,3)],rainbow(12)[seq(2,12,3)],rainbow(12)[seq(3,12,3)]))
  #
  # cols = jabba.colors

  par.save <- par
  # Extract residual for by scenario and index
  d <- summaryoutput$cpue
  d$scenario <- factor("s1")
  names(d)
  scenarios <- (levels(d$scenario))
  d$residual <- log(d$Obs)-log(d$Exp)
  agg <- aggregate(residual~Yr+Fleet_name,d,mean)
  #combine
  idd <- paste0(agg[,2])
  yall <- 1:length(idd)
  iall <- levels(factor(idd))
  yrs <- agg[,1]

  Par = list(mfrow=c(ceiling(length(scenarios)/2),1),mar = c(1.2, 1.2, 1., 0.1),oma=c(0, 0, 0, 6), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)
  png(file = paste0("./plots/runstest.png"), width = 6, height = 3,
      res = 420, units = "in")
  par(Par)
  for(j in 1:length(scenarios)){
    ds <- d[d$scenario == scenarios[j],]
    jdd <- paste0(ds$Yr,".",ds$Fleet_name)
    yj <- which(idd %in% jdd)
    ylim <- c(-max(abs(d$residual)),max(abs(d$residual)))
    plot(
      yall,
      yall,
      type = "n",
      xlab = "Year",
      xlim = c(1, max(yall)),
      ylim = ylim,
      axes = F,
      xaxs = "i",
      yaxs = "i"
    )
    axis(1,at=yall,labels=yrs,cex.axis=0.6,mgp=c(0.1,0.,0))
    axis(2,cex.axis=0.6,mgp=c(0.5,0.0,0))
    title(main=paste(ds$scenario)[1], mgp=c(0.1,0,0),cex.main=0.8,line=0.2)
    abline(v=1)

    #for(i in 1:length(indices)){}
    i = 1
    ni = length(unique(ds$Fleet_name))
    indices = levels(factor(ds$Fleet_name))


    for(i in 1:ni){
      di = ds[ds$Fleet_name==indices[i],]
      yi = yall[which(agg$Fleet_name==indices[i])]
      get_runs = runs.sig3(di$residual)
      lims = get_runs$sig3lim
      polygon(c(yi,rev(yi)),c(rep(ylim[1],nrow(di)),rev(rep(ylim[2],nrow(di)))),col=ifelse(get_runs$p.runs<0.05,rgb(1,0,0,0.5),rgb(0,1,0,0.5)),border=0)
      polygon(c(yi,rev(yi)),c(rep(lims[1],nrow(di)),rev(rep(lims[2],nrow(di)))),col=grey(0.5,0.5),border=0)
      lines(yi,di$residual)
      points(yi,di$residual,pch=21,bg=ifelse(abs(di$residual)>abs(lims[1]),2,0),cex=0.7)
      text(mean(yi),ylim[2]*0.9,which(indices[i]==iall),cex=0.8)

    }
    if(j==1){  legend(par('usr')[2]-4, par('usr')[4]-1, bty='n', xpd=NA,
                      c(paste0(1:length(iall),": ",iall)),cex=0.7)}
    if(j==1){legend(par('usr')[2]+2, par('usr')[4], bty='n', xpd=NA,
                    c("Passed","Failed"),pch=15,col=c(rgb(0,1,0,0.5),rgb(1,0,0,0.5)),pt.cex=2,cex=0.7)}


    abline(h=0,lty=2)
    abline(h=max(abs(d$residual)))
    for(i in 1:length(iall)){
      abline(v=0.5+max(which(agg$Fleet_name==levels(factor(agg$Fleet_name))[i])))
    }
  }

  dev.off()
  par=par.save

  #---------------------------------------
  # Make JABBA-Residual Plot
  #---------------------------------------

  resids = dcast(d,Yr~Fleet_name,value.var="residual")
  # JABBA-residual plot
  Par = list(mfrow=c(1,1),mar = c(3.5, 3.5, 0.1, 0.1), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)
  png(file = paste0("./plots/JABBAresiduals.png"), width = 5, height = 3.5,
      res = 420, units = "in")
  par(Par)
  Resids = t(resids[,-1])
  Yr = resids[,1]
  n.indices = nrow(Resids)
  n.years = length(Yr)
  plot(Yr,Yr,type = "n",ylim=ifelse(rep(max(Resids,na.rm = T),2)>0.9,range(1.2*Resids,na.rm = T),range(c(-1.3,1.2))),xlim=range(Yr),ylab="log residuals",xlab="Year")
  boxplot(as.matrix(Resids),add=TRUE,at=c(Yr),xaxt="n",col=grey(0.8,0.5),notch=FALSE,outline = FALSE)
  abline(h=0,lty=2)

  positions=runif(nrow(Resids),-0.2,0.2)

  for(i in 1:n.indices){
    for(t in 1:n.years){
      lines(rep((Yr+positions[i])[t],2),c(0,Resids[i,t]),col=jabba.colors[i])}
    points(Yr+positions[i],Resids[i,],col=1,pch=21,bg=jabba.colors[i])}
  mean.res = apply(Resids,2,mean,na.rm =TRUE)
  smooth.res = predict(loess(mean.res~Yr),data.frame(Yr))
  lines(Yr,smooth.res,lwd=2)
  # get degree of freedom
  Nobs =length(as.numeric(Resids)[is.na(as.numeric(Resids))==FALSE])
  RMSE = round(100*sqrt(sum(Resids^2,na.rm =TRUE)/Nobs),1)

  legend('topright',c(paste0("RMSE = ",RMSE,"%")),bty="n")
  legend('bottomleft',c(paste(rownames(Resids)),"Loess"),bty="n",col=1,pt.cex=1.1,cex=0.75,pch=c(rep(21,n.indices),-1),pt.bg=c(jabba.colors,1),lwd=c(rep(-1,n.indices),2))

  dev.off()

  graphics.off()

}
