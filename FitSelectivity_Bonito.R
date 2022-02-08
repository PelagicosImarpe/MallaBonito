dir.car <- "D:/Elmer QS/Selectividad/Bonito"
setwd(dir.car)

year <- 2019
mesh <- 3.81 # 1 1/2 pulg 
catch <- read.table("catch_2019.csv", sep = ",", header = T)

# if(all == T)
allCatch <- data.frame(Size = catch$Size , catch_abundace = rowSums(catch[,2:ncol(catch)]))
allCatch$rel <- all.catch$catch_abundace / sum(all.catch$catch_abundace)
allCatch$cum <- cumsum(allCatch$rel)

plot(allCatch$Size , allCatch$cum, type = "p", pch = 16, col = 4)

# if(all == F)

date <- names(catch)

par(mfrow = c(3,2))
for(u in 2:ncol(catch)){
  dataCatch <- catch[, c(1, u)]
  dataCatch$rel <- dataCatch[,2] / sum(dataCatch[,2])
  dataCatch$cum <- cumsum(dataCatch$rel)
  plot(dataCatch$Size , dataCatch$cum, type = "p", pch =16, col = 4,  main = paste(year, date[u], sep = "-"))
  print(paste(year, date[u], sep = "-"))
}

plot(allCatch$Size , allCatch$cum, type = "p", pch = 16, col = 4)

################################################################################
#Desde aquí inicia el ajuste de parametros del modelo logistico (L50 y L95)

paramIni <- c(par1=log(55),par2=log(60))
# paramIni <- c(par1=-50,par2=1, par3 = 1)

CalcLogLikeLogisticAll<- function(par,Data= allCatch){
  par1 <- exp(par[1])
  par2 <- exp(par[2])
  # par3 <- par[3]
    # for(i in 1:1){
    tmp<- Data
    #Calculate the probabilities 
    sel <- 1/(1+ exp(-log(19)*(tmp$Size-par1)/par2))
    #Predice el stock vulnerable y lo relativiza (0-1)
    # Pred <- sel*tmp$SurveyAbundance
    Probs <- sel
    #relativiza las captura (0-1000)
    Nstand <-tmp$cum*1000
    # Nstand2 <- floor(tmp$CatchAbundance)
    # Nstand3 <- sum(tmp$CatchAbundance) * floor(tmp$CatchAbundance)/sum(floor(tmp$CatchAbundance))
    #Establece un distribución multinomial con Nstand y Probs
    NegLogLikSurvey <- -dmultinom(x=Nstand, prob=Probs, log = TRUE)
    # NegLogLikSurvey <- -dnorm(x=Nstand, prob=Probs, log = TRUE)
    
    #función de verosimilitud
  NegLogLik <- NegLogLikSurvey
  return(NegLogLik)
}

#################################################################################
#par1 (L50) y par2(L95-L50) optimizado - DataLong2
results<-optim(par=paramIni,CalcLogLikeLogisticAll,Data= allCatch )
results

exp(results$par)

###############################################################################
#Luego se establece la función de modelado (L50 y L95) con los parametros optimos
fitData<-function(Data=DataLong, res=results) 
{
	par1 <- exp(res$par[1])
	par2 <- exp(res$par[2])
	Surveys <- unique(Data$Survey)
	
	for(i in 1:length(Surveys))
	{
		tmp<- Data[Data$Survey == Surveys[i],]
		#Calculate the probabilities 
		sel<- 1/(1+ exp(-log(19)*(tmp$Size-par1)/par2))
		Pred <- sel*tmp$SurveyAbundance

		relSurvey <- tmp$SurveyAbundance/sum(tmp$SurveyAbundance)
		relCatchObs <- tmp$CatchAbundance/sum(tmp$CatchAbundance)
		relCatchPred <- Pred/sum(Pred)

		lg<-dmultinom(x=relCatchObs, prob=relCatchPred, log = TRUE)
		
		plot(tmp$Size,relCatchObs,col="black",type="n",ylim=c(0,1.5*max(c(relSurvey,tmp$relCatchObs,relCatchPred))),main=tmp$Key[1],xlim=c(5,20),pch=15)
		polygon(x=c(tmp$Size,rev(tmp$Size)), y=c(relCatchObs,rep(0,length(tmp$Size))),col="orange1",border="bisque2")
		points(tmp$Size,relCatchPred,pch=15,col="blue",type="l", lwd =2)
		abline(v = 12, col="black", lwd=0.5, lty=2)
	  
		# lines(tmp$Size,relSurvey,col="red")
		}

}	


#Luego, L50 y L95
param<-exp(results$par)
names(param)<-NULL
select <-c(L50=param[1], L95=param[2]+param[1])

par1 <- exp(results$par)[1]
par2 <- exp(results$par)[2]
sel <- 1/(1+ exp(-log(19)*(tmp$Size-par1)/par2))

dev.off()
plot(allCatch$Size , allCatch$cum, type = "p", pch = 16, col = 4)
lines(tmp$Size, sel, col = 2, type = "l")
segments(x0 = par1, y0 = 0, x1 = par1, y1 = 0.5, col = 2)









