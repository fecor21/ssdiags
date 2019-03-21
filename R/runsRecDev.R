#' runsRecDev
#'
#' \code{runsRecDev} Do runs for recruitment devs
#' @param summaryoutput an object generated via SS_output
#' @seealso \code{\link[r4ss]}


runsRecDev <- function(summaryoutput){
  ss.rec = summaryoutput$recruit
  rec.est = ss.rec[is.na(ss.rec$dev)==F,]
  
  
  Par = list(mfrow=c(1,1),mar = c(3.5, 3.5, 0.1, 0.1), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)
  
  png(file = paste0("RecDev_",assessment,".png"), width = 5, height = 3.5, 
      res = 200, units = "in")
  par(Par)
  years=rec.est$year
  get_runs = runs.sig3(rec.est$dev)
  xlim =range(ss.rec$year) 
  ylim = c(min(-0.22,abs(rec.est$dev)*-1.1),max(0.22,abs(rec.est$dev)*1.1))#range(proc.dev)*1.1
  
  
  cord.x <- c(years,rev(years))
  cord.y <- c(rep(get_runs$sig3lim[1],length(years)),rev(rep(get_runs$sig3lim[2],length(years))))
  # Process Error
  plot(years,years,ylab="Recruiment Deviates",xlab="Year",ylim=ylim,xlim=xlim,type="n")
  polygon(cord.x,cord.y,col='grey',border=0,lty=2)
  ss.rec$dev[is.na(ss.rec$dev)] = 0
  lines(ss.rec$year,ss.rec$dev,lwd=2)
  abline(h=0,lty=2)
  lines(years,rec.est$dev,lwd=2,col=4)
  outliers = rec.est[rec.est$dev<get_runs$sig3lim[1] |rec.est$dev>get_runs$sig3lim[2],]
  points(outliers$year,outliers$dev,col=2,pch=16)
  pvalue = round(get_runs$p.runs,3)
  
  legend("topright",c(paste0("runs.p ", ifelse(pvalue<0.05,"< 0.05",paste0(" = ",pvalue)))),lwd=c(-1,1,2),col=c(0,1,4),bty="n",y.intersp = -0.2)
  legend("topleft",c("Fixed","Estimated"),lwd=c(2,2),col=c(1,4),bty="n")
  
  dev.off()
}
