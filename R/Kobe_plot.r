# ISC Kobe plot 
 
 library(r4ss) 
 
 model_dir <- "C:/Users/felip/Dropbox/MLS_April_2019/ZI_CPUE/Model_715"    #<----------------
 model <- SS_output(model_dir,ncols = 500)
 
 wd=c("C:/Users/felip/Dropbox/MLS_April_2019/ZI_CPUE/Model_715")#NoSeas")

 setwd(wd)
 base.model<-SS_output(getwd())
 # Kobe plot layout setting
 x_max = 4.0
 x_min = 0.0
 y_max = 4.0
 y_min = 0
   
 rnames <- base.model$derived_quants$Label

 index_SSB_MSY = which(rnames==paste("SSB_MSY",sep=""))
 index_Fstd_MSY = which(rnames==paste("Fstd_MSY",sep=""))
 index_SPR_MSY= which(rnames==paste("SPR_MSY",sep=""))
 
 year_vec = min(base.model$sprseries$Yr):max(base.model$sprseries$Yr)

 SSB_MSY_est = base.model$derived_quants[index_SSB_MSY:index_SSB_MSY,2]
 Fstd_MSY_est = base.model$derived_quants[index_Fstd_MSY:index_Fstd_MSY,2]
 SPR_MSY_est = base.model$derived_quants[index_SPR_MSY:index_SPR_MSY,2]
 
 SSBratio = base.model$sprseries$SSB/SSB_MSY_est
 Fratio = base.model$sprseries$F_report/Fstd_MSY_est
 SPRratio = base.model$sprseries$SPR/SPR_MSY_est
 
 ### Fration terminal year * sqrt((sd terminal year/F terminal year)^2+(sd Fmsy/Fmsy)^2)
 ### Fration terminal year * sqrt((sd terminal year/B terminal year)^2+(sd Bmsy/Bmsy)^2)
  ## F1-10
 #Fstd<-Fratio[42]*sqrt((0.011509/0.0743)^2+(0.003036/0.169564)^2)

 #SSBstd<-SSBratio[42]*sqrt((5000.23/29593.2)^2+(638.781/15723.1)^2)
 
 ## F3-10
 #Fstd<-Fratio[42]*sqrt((0.018106/0.131934)^2+(0.003103/0.339866)^2)
 #SSBstd<-SSBratio[42]*sqrt((5000.23/29593.2)^2+(638.781/15723.1)^2)
 
 
 
 #Fratio_95<-Fratio[42]+1.96*Fstd
 #Fratio_05<-Fratio[42]-1.96*Fstd
 #SSBratio_95<-SSBratio[42]+1.96*SSBstd
 #SSBratio_05<-SSBratio[42]-1.96*SSBstd
 
 
png("KobePlot.png",height=5,width = 5, units="in",res=300) 
 plot(c(x_min,x_max),c(y_min,y_max),type="n", ylab="", xlab="")
 mtext(side=1, expression(SSB/SSB[MSY]),line=2.5, cex=1)  
 mtext(side=2, expression(F/F[MSY]),line=2.5, cex=1)  
 
 polygon(c(x_min,1,1,x_min), c(1,1,y_min,y_min),col="khaki1")
 polygon(c(1,x_max,x_max,1), c(1,1,y_min,y_min),col="palegreen")
 polygon(c(x_min,1,1,x_min), c(1,1,y_max,y_max),col="salmon")
 polygon(c(1,x_max,x_max,1), c(1,1,y_max,y_max),col="khaki1")
 

 points(SSBratio,Fratio,type="o",bg="black",pch=21,col="black",cex=1.2)
 
 points(SSBratio[1],Fratio[1],type="o",bg="white",pch=21,col="white",cex=1.2)
 points(SSBratio[length(year_vec)-1],Fratio[length(year_vec)-1],type="o",bg="orange",pch=21,col="orange",cex=1.2)
 #points(SSBratio[length(year_vec)],Fratio[length(year_vec)],type="o",bg="blue",pch=21,col="blue",cex=1.2)
 
 
 
 #SSBratio 
 #Fratio 0.693362522	1.034111657

 
 
 points(c(SSBratio[length(year_vec)-1],SSBratio[length(year_vec)-1])
 ,c(Fratio_05,Fratio_95)
 ,type="l",lwd=2,lty=3)
 
 
# points(c(SSBratio_95,SSBratio_05)
# ,c(Fratio[length(year_vec)-1],Fratio[length(year_vec)-1])
# ,type="l",lwd=2,lty=3)
 
 points(SSBratio[length(year_vec)-1],Fratio[length(year_vec)-1],type="o",bg="orange",pch=21,col="orange",cex=1.2)
 
 
  #text(SSBratio[c(1,42)],Fratio[c(1,42)],labels=year_vec[c(1,42)],cex = 1,adj = c(-0.4,1))
  #text(SSBratio[19],Fratio[19],labels=year_vec[19],cex = 1, pos=3)
  
 dev.off()
 # legend
 # legend(x=x_max-1,y=y_max, 
 #        legend=c(min(year_vec),max(year_vec)-1),
 #        col=c(1), lwd=1, lty=c(1,1), 
 #        pch=c(NA,NA),border=NULL,box.lty=0,bg=NULL)
 # 
 # legend(x=x_max-1,y=y_max, 
 #        legend=c(min(year_vec),max(year_vec)-1),
 #        col=c("white","orange"), lwd=1, lty=c(0,0), 
 #        pch=c(19,19),fill=NULL)
 # 
 # 
