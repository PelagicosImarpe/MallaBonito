library("TropFishR")

data <- read.csv("tallas1967-2019.csv", check.names = FALSE)
head(data)

data[is.na(data)] <- 0

bonito <- NULL
bonito$midLengths <- data$`LONG H (cm)`
bonito$Linf <- 73.3
bonito$K <- 0.33
bonito$t0 <- -0.79
bonito$catch <- data$`2018`

output <- catchCurve(param = bonito, calc_ogive = TRUE)#, reg_int = c(49,60))#43-52,52-62,48-57
plot(output, plot_selec = TRUE)

c(L50 = output$L50, L75 = output$L75, L95 = output$L95)



##############3

dat <- read.csv("L50-TropFishR.csv")
plot(dat$year, dat$L50, type="l", ylim=c(0,1.1*max(dat$L50)), xlab="años", ylab="L50")
points(dat$year, dat$L50)


###########################################


library("TropFishR")

allCatch <- read.csv("tallas1967-2019.csv", check.names = FALSE)
head(allCatch)

allCatch[is.na(allCatch)] <- 0

filter <- c("1989")
year_start <- 1968
year_end <- 2019
all_years <- names(allCatch)[!(names(allCatch) %in% filter)][-1]
all_years <- all_years[which((all_years>=year_start) & (all_years<=year_end))]
allCatch <- allCatch[,c("LONG H (cm)", all_years)]

for(year in names(allCatch)[-1]){
  bonito <- NULL
  bonito$midLengths <- allCatch$`LONG H (cm)`
  bonito$Linf <- 73.3
  bonito$K <- 0.33
  bonito$t0 <- -0.79
  bonito$catch <- allCatch[,year]
  
  # which.max(bonito$catch)
  
  output <- catchCurve(param = bonito, calc_ogive = TRUE)#, reg_int = c(49,60)
  # plot(output, plot_selec = TRUE)
  
  print(c(L50 = output$L50, L75 = output$L75, L95 = output$L95))
  
}





############################################################



library("TropFishR")

allCatch <- read.csv("tallas1967-2019.csv", check.names = FALSE)
head(allCatch)

allCatch[is.na(allCatch)] <- 0

filter <- c("1989")
year_start <- 1967
year_end <- 2019
all_years <- names(allCatch)[!(names(allCatch) %in% filter)][-1]
all_years <- all_years[which((all_years>=year_start) & (all_years<=year_end))]

allCatch <- allCatch[!(names(allCatch) %in% filter)]
allCatch$Total <- rowSums(allCatch[,all_years], na.rm = TRUE)

bonito <- NULL
bonito$midLengths <- allCatch$`LONG H (cm)`
bonito$Linf <- 73.3
bonito$K <- 0.33
bonito$t0 <- -0.79
bonito$catch <- allCatch$Total

# output <- catchCurve(param = bonito, calc_ogive = TRUE)
output <- catchCurve(param = bonito, calc_ogive = TRUE, reg_int = c(50,61))#50-63
# plot(output, plot_selec = TRUE)

c(L50 = output$L50, L75 = output$L75, L95 = output$L95)
