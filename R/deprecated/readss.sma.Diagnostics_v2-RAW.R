

library(r4ss)
library(tseries)
library(reshape2)
setwd("C:/Users/felip/Documents/Mako_Atlantic/BH")
#smabase=SS_output(dir="C:/Users/felip/Documents/Mako_Atlantic/BH",covar=F)
#save(smabase,file="smabase.rdata")
assessment = "SMA"

load("smabase.rdata",verbose=T)

names(smabase)
smabase$cpue
# GENERALIZE and call your object ssall
ssall = smabase 

#---------------------
# Index color palette
#---------------------
jabba.colors = as.character(c('#e6194b', "#3cb44b", "#ffe119",
                              "#0082c8","#f58231", "#911eb4",
                              "#46f0f0", "#f032e6", "#d2f53c",
                              "#fabebe", "#008080","#e6beff", "#aa6e28",rainbow(12)[seq(1,12,3)],rainbow(12)[seq(2,12,3)],rainbow(12)[seq(3,12,3)]))

cols = jabba.colors

#-------------------------------------------------
# Function to do runs.test and 3 x sigma limits  
#------------------------------------------------

runs.sig3 <- function(x,type="resid") {
  if(type=="resid"){mu = 0}else{mu = mean(x, na.rm = TRUE)} 
  # Average moving range
  mr  <- abs(diff(x - mu))
  amr <- mean(mr, na.rm = TRUE)
  # Upper limit for moving ranges
  ulmr <- 3.267 * amr
  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  # Calculate standard deviation, Montgomery, 6.33
  stdev <- amr / 1.128
  # Calculate control limits
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev
  if(nlevels(factor(sign(x)))>1){ pvalue = round(runs.test(factor(sign(x)))$p.value,3)} else {
    pvalue = 0.001  
  }
  
  return(list(sig3lim=c(lcl,ucl),p.runs= pvalue))
}



#-------------------------------------------------------------------
# Produce runs.tests
#------------------------------------------------------------------

par.save = par
# Extract residual for by scenario and index 
d = ssall$cpue
d$scenario = factor("s1")
names(d)
scenarios = (levels(d$scenario))
d$residual = log(d$Obs)-log(d$Exp)
agg = aggregate(residual~Yr+Name,d,mean)
#combine
idd = paste0(agg[,2])
yall = 1:length(idd)
iall = levels(factor(idd))
yrs = agg[,1]

Par = list(mfrow=c(ceiling(length(scenarios)/2),1),mar = c(1.2, 1.2, 1., 0.1),oma=c(0, 0, 0, 6), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)
png(file = paste0("runstest_",assessment,".png"), width = 6, height = 3, 
    res = 200, units = "in")
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
  if(j==1){  legend(par('usr')[2]-4, par('usr')[4]-1, bty='n', xpd=NA,
                    c(paste0(1:length(iall),": ",iall)),cex=0.7)}
  if(j==1){legend(par('usr')[2]+2, par('usr')[4], bty='n', xpd=NA,
                  c("Passed","Failed"),pch=15,col=c(rgb(0,1,0,0.5),rgb(1,0,0,0.5)),pt.cex=2,cex=0.7)}
  
  
  abline(h=0,lty=2)
  abline(h=max(abs(d$residual)))
  for(i in 1:length(iall)){
    abline(v=0.5+max(which(agg$Name==levels(factor(agg$Name))[i])))
  }
}

dev.off()
par=par.save

#---------------------------------------
# Make JABBA-Residual Plot
#---------------------------------------

resids = dcast(d,Yr~Name,value.var="residual")
# JABBA-residual plot
Par = list(mfrow=c(1,1),mar = c(3.5, 3.5, 0.1, 0.1), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)
png(file = paste0("JABBAresiduals_",assessment,".png"), width = 5, height = 3.5, 
    res = 200, units = "in")
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




#----------------------------------------
# Do runs for recruitment devs
#----------------------------------------

ss.rec = ssall$recruit
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

#-----------------------------------------
#  Do runs on size comps (mean length) using standardized residuals
#-----------------------------------------

#smabase$mnlen


ar.save = par
# Extract residual for by scenario and index 
d = ssall$mnlen
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
png(file = paste0("runstest_MeanSize_",assessment,".png"), width =7.5, height = 3.5, 
    res = 200, units = "in")
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


#-----------------------------------------
#  Do runs on size comps (mean length) using unstandardized residuals
#-----------------------------------------

#smabase$mnlen


ar.save = par
# Extract residual for by scenario and index 
d = ssall$mnlen
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
png(file = paste0("runstest_MeanSizeResid_",assessment,".png"), width =7.5, height = 3.5, 
    res = 200, units = "in")
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

