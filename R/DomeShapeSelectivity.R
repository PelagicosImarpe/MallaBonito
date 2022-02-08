paramIni <- c(35, 37, 50, 20,-5, -5)

CalcLogLikeDomeShapeAll<- function(par,Data= allCatch){
  
  p1 = paramIni[1] #
  p2 = paramIni[2] #
  p3 = paramIni[3] # 
  p4 = paramIni[4] # 
  p5 = paramIni[5] #
  p6 = paramIni[6] #
  
  L <- seq(1,80,1)
  
  nL <- length(L)
  comp1 <- 1/(1 + exp(-p5))
  comp2 <- exp((-(L - p1)^2)/p3)
  comp3 <- exp((-(L[1] - p1)^2)/p3)
  asc <- comp1 + (1 - comp1) * ((comp2 - comp3)/(1 - comp3))
  comp4 <- 1/(1 + exp(-p6))
  comp5 <- exp((-(L - p2)^2)/p4)
  comp6 <- exp((-(L[nL] - p2)^2)/(p4 - 1))
  dsc <- 1 + (comp4 - 1) * ((comp5 - 1)/(comp6 - 1))
  J1 <- 1/(1 + exp(-(20 * (L - p1)/(1 + abs(L - p1)))))
  J2 <- 1/(1 + exp(-20 * ((L - p2)/(1 + abs(L - p2)))))
  
  tmp<- Data
  #Calculate the probabilities 
  sel <- (asc * (1 - J1)) + J1 * (1 - J2 + dsc * J2)
  #sel <- 1/(1+ exp(-log(19)*(tmp$Size-par1)/par2))
  #Predice el stock vulnerable y lo relativiza (0-1)
  Probs <- sel
  #relativiza las captura (0-1000)
  Nstand <-tmp$rel
  #Establece un distribuci?n multinomial con Nstand y Probs
  NegLogLikSurvey <- -dmultinom(x=Nstand, prob=Probs, log = TRUE)
  
  #funci?n de verosimilitud
  NegLogLik <- NegLogLikSurvey
  return(NegLogLik)
}
