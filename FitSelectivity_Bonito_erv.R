allCatch <- read.table("tallas1967-2019.csv", sep = ",", header = T, check.names = F)

################################################################################
#Desde aqu? inicia el ajuste de parametros del modelo logistico (L50 y L95)

paramIni <- c(par1=log(55),par2=log(60))

CalcLogLikeLogisticAll<- function(par,Data= allCatch){
  par1 <- exp(par[1])
  par2 <- exp(par[2])
  
  tmp<- Data
  #Calculate the probabilities 
  sel <- 1/(1+ exp(-log(19)*(tmp$Size-par1)/par2))
  #Predice el stock vulnerable y lo relativiza (0-1)
  Probs <- sel
  #relativiza las captura (0-1000)
  Nstand <-tmp$cum*1000
  #Establece un distribuci?n multinomial con Nstand y Probs
  NegLogLikSurvey <- -dmultinom(x=Nstand, prob=Probs, log = TRUE)
  
  #funci?n de verosimilitud
  NegLogLik <- NegLogLikSurvey
  return(NegLogLik)
}

output <- data.frame()
par(mfcol=c(11,4), mar=c(0,0,0,0), oma=c(4,2,2,2))
filter <- c("1989")
for(year in names(allCatch)[!(names(allCatch) %in% filter)][-1]){
  allCatch_y <- data.frame(Size = allCatch$`LONG H (cm)`, catch_abundace = allCatch[,year])
  allCatch_y$rel <- allCatch_y$catch_abundace / sum(allCatch_y$catch_abundace, na.rm = T)
  miss <- is.na(allCatch_y$rel)
  allCatch_y$rel[miss] <- 0
  cs <- cumsum(allCatch_y$rel)
  cs[miss] <- NA
  allCatch_y$cum <- cumsum(allCatch_y$rel)
  
  #################################################################################
  #par1 (L50) y par2(L95-L50) optimizado - DataLong2
  results<-optim(par=paramIni,CalcLogLikeLogisticAll,Data= allCatch_y )
  # print(results)
  
  #Luego, L50 y L95
  param<-exp(results$par)
  names(param)<-NULL
  select <-c(L50=param[1], L95=param[2]+param[1])
  
  output <- rbind(output, c(year = as.numeric(year), L50=param[1], L95=param[2]+param[1]))
  
  par1 <- exp(results$par)[1]
  par2 <- exp(results$par)[2]
  sel <- 1/(1+ exp(-log(19)*(allCatch_y$Size-par1)/par2))
  
  plot(allCatch_y$Size , allCatch_y$cum, type = "p", pch = 16, col = 4)
  lines(allCatch_y$Size, sel, col = 2, type = "l")
  segments(x0 = par1, y0 = 0, x1 = par1, y1 = 0.5, col = 1)
  mtext(year, line = -1.5, adj=0.05, font = 2)
  
}
names(output) <- c("year", "L50", "L95")

output
write.csv(output, file = "L50.csv")
plot(output$year, output$L50, type="l", ylim=c(0,1.1*max(output$L50)), xlab="años", ylab="L50")
points(output$year, output$L50)


boxplot(output$L50)
hist(output$L50, n=4)
summary(output$L50)


################################################################


par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(4,2,2,2))
filter <- c("1989")
year_start <- 1967
year_end <- 2019
all_years <- names(allCatch)[!(names(allCatch) %in% filter)][-1]
all_years <- all_years[which((all_years>=year_start) & (all_years<=year_end))]

allCatch_t <- data.frame(Size = allCatch$`LONG H (cm)`, catch_abundace = rowSums(allCatch[,all_years], na.rm = TRUE))
allCatch_t$rel <- allCatch_t$catch_abundace / sum(allCatch_t$catch_abundace, na.rm = T)
miss <- is.na(allCatch_t$rel)
allCatch_t$rel[miss] <- 0
cs <- cumsum(allCatch_t$rel)
cs[miss] <- NA
allCatch_t$cum <- cumsum(allCatch_t$rel)

#################################################################################
#par1 (L50) y par2(L95-L50) optimizado - DataLong2
results<-optim(par=paramIni,CalcLogLikeLogisticAll,Data= allCatch_t)
# print(results)

#Luego, L50 y L95
param<-exp(results$par)
names(param)<-NULL
select <- c(L50=param[1], L95=param[2]+param[1])

output <- c(L50=param[1], L95=param[2]+param[1])

par1 <- exp(results$par)[1]
par2 <- exp(results$par)[2]
sel <- 1/(1+ exp(-log(19)*(allCatch_y$Size-par1)/par2))

plot(allCatch_t$Size , allCatch_t$cum, type = "p", pch = 16, col = 4)
lines(allCatch_t$Size, sel, col = 2, type = "l")
segments(x0 = par1, y0 = 0, x1 = par1, y1 = 0.5, col = 2)

names(output) <- c("L50", "L95")



















###############################################################################
#Luego se establece la funci?n de modelado (L50 y L95) con los parametros optimos
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









