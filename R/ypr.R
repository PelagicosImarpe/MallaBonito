library("TropFishR")

data <- read.csv("tallas1967-2019.csv", check.names = FALSE)
head(data)

data[is.na(data)] <- 0

bonito <- NULL
bonito$Linf <- 73.3
bonito$K <- 0.33
bonito$t0 <- -0.79
bonito$M <- 1.5*bonito$K
# bonito$FM
bonito$a <- 0.00501
bonito$b <- 3.29219
bonito$tr <- 0.5
bonito$midLengths <- data$`LONG H (cm)`

# select.list <- list(selecType = 'trawl_ogive', L50 = 51.95, L75 = 54.48)

output <- predict_mod(bonito, tc_change = c(0.4,1,2,3,4,5), FM_change = seq(0,4.25,0.1),
                      # s_list = select.list, 
                      type = 'ypr', plot = T)

# output <- predict_mod(bonito, FM_change = seq(0,6,0.1),
                      # tc_change = seq(0.2,1,0.2), type = 'ypr')
par(oma = c(0,0,2,2), mar = c(4,4,8,3), col=1:9)
plot(output)
source("plot_erv.predict_mod.R")
plot_erv.predict_mod(output, type = 'ypr')


output <- predict_mod(bonito, tc_change = c(0.4,1,1.5,2,2.5,3,4,5), FM_change = seq(0,4.25,0.1),
                      # s_list = select.list, 
                      type = 'ypr', plot = T)
source("plot_erv.predict_mod_v2.R")
plot_erv.predict_mod(output, type = 'ypr')

(VBGF(bonito, t = c(1.7,1.8,1.9,2,2.1,2.2,2.3,2.4))/4.7*2/3*10)+4

output <- predict_mod(bonito, tc_change = c(1.7,1.8,1.9,2,2.1,2.2,2.3,2.4), FM_change = seq(0,4.25,0.1),
                      # s_list = select.list, 
                      type = 'ypr', plot = T)
source("plot_erv.predict_mod_v2.R")
plot_erv.predict_mod(output, type = 'ypr')

# output$list_Lc_runs[[1]]

# (0.04 / 0.040525833)
 # -0.001558686  0.040525833

 # -0.009197424  1.038815286

# factor <- 
px <- output$list_Lc_runs[[1]]$FM
py <- output$list_Lc_runs[[1]]$Y_R.rel /0.03896715
offset_text <- py[length(py)] * 0.05
offset_x <- py[length(px)] * 0.1
text(x = px[length(px)] - offset_x, y = py[length(py)] + offset_text, labels = "edad 1", col=2)

px <- output$list_Lc_runs[[2]]$FM
py <- output$list_Lc_runs[[2]]$Y_R.rel /0.03896715
offset_text <- py[length(py)] * 0.05
offset_x <- py[length(px)] * 0.1
text(x = px[length(px)] - offset_x, y = py[length(py)] + offset_text, labels = "edad 1", col=2)
px <- output$list_Lc_runs[[3]]$FM
py <- output$list_Lc_runs[[3]]$Y_R.rel /0.0387
offset_text <- py[length(py)] * 0.05
offset_x <- py[length(px)] * 0.1
text(x = px[length(px)] - offset_x, y = py[length(py)] + offset_text, labels = "edad 1", col=2)
px <- output$list_Lc_runs[[4]]$FM
py <- output$list_Lc_runs[[4]]$Y_R.rel /0.0387
offset_text <- py[length(py)] * 0.05
offset_x <- py[length(px)] * 0.1
text(x = px[length(px)] - offset_x, y = py[length(py)] + offset_text, labels = "edad 1", col=2)
px <- output$list_Lc_runs[[5]]$FM
py <- output$list_Lc_runs[[5]]$Y_R.rel /0.0387
offset_text <- py[length(py)] * 0.05
offset_x <- py[length(px)] * 0.1
text(x = px[length(px)] - offset_x, y = py[length(py)] + offset_text, labels = "edad 1", col=2)
px <- output$list_Lc_runs[[1]]$FM
py <- output$list_Lc_runs[[1]]$Y_R.rel /0.0387
offset_text <- py[length(py)] * 0.05
offset_x <- py[length(px)] * 0.1
text(x = px[length(px)] - offset_x, y = py[length(py)] + offset_text, labels = "edad 1", col=2)
px <- output$list_Lc_runs[[1]]$FM
py <- output$list_Lc_runs[[1]]$Y_R.rel /0.0387
offset_text <- py[length(py)] * 0.05
offset_x <- py[length(px)] * 0.1
text(x = px[length(px)] - offset_x, y = py[length(py)] + offset_text, labels = "edad 1", col=2)

VBGF(bonito, t = 1:5)

VBGF(bonito, L = c(26.8, 28.5, 52.5, 57))
VBGF(bonito, t = c(0.7, 3.77))

####################################################3


#______________________________________
# Yiel Per Recruit (YPR) / Beverton and Holt's model
#______________________________________
# age structured data
# Nemipterus marginatus
threadfin <- list(Winf = 286, K = 0.37, t0 = -0.2, M = 1.1, tr = 0.4)

predict_mod(threadfin, FM_change = seq(0,6,0.1),
            tc_change = seq(0.2,1,0.2), type = 'ypr')  #where it is maximal  = MSY

# Leiognathus spendens (Pauly, 1980)
ponyfish <- list(Winf = 64, K = 1, t0 = -0.2, M = 1.8, tr = 0.2)

predict_mod(ponyfish, tc_change = c(0.2,0.3,1.0), type = 'ypr', plot=TRUE)

#______________________________________
# length structured data
# Xiphias gladius (Berkeley and Houde, 1980)
swordfish <- list(Linf = 309, K = 0.0949, M = 0.18,
                  a = 0.0003, b = 3, Lr = 90)

select.list <- list(selecType = 'trawl_ogive', L50 = 120, L75 = 132)
#swordfish$midLengths <- seq(60,300,5)

output <- predict_mod(param = swordfish, Lc_change = c(100,118,150,180),
                      s_list = select.list, type = 'ypr', Lmin = 90, Lincr = 8)
plot(output)

data(hake)
hake$Lr <- 35
select.list <- list(selecType = 'trawl_ogive', L50 = 50, L75 = 54)
output <- predict_mod(param = hake, FM_change = seq(0,3,0.05),
                      Lc_change = seq(30,70,1), s_list = select.list,
                      type = 'ypr', plot = FALSE, curr.Lc = 50, curr.E = 0.73)
plot(output, type = "Isopleth", xaxis1 = "FM", yaxis1 = "Y_R.rel", mark = TRUE)

output <- predict_mod(param = hake, E_change = seq(0,1,0.1),
                      Lc_change = seq(2,120,2), #s_list = select.list,
                      type = 'ypr', plot = FALSE)
plot(output, type = "Isopleth", xaxis1 = "E", yaxis1 = "B_R")


