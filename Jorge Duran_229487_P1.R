#*** Jorge Eduardo Durán Vásquez***
#*** Case Studies SS 2022 ***
#*** Project 1: One-Quarter-Ahead Forecasts of US GDP Growth A Vector Autoregression Approach 

###########***Upload and processing data***####################################

# Upload the data set
Data <- read.table(file = "2022-02.csv", header = TRUE, sep = ',')

# Pre-processing the data
testdata <-Data[3:254,]
NewData <- data.frame(Data$sasdate,Data$GDPC1,Data$CUMFNS,Data$UNRATESTx,Data$CPIAUCSL,Data$FEDFUNDS,Data$M1REAL,Data$S.P.500)

#Save the data without the first to rows
finaldataset <- NewData[3:254,]
class(finaldataset)

# Transforming Growth Gross Domestic Product (GDP) 
GDPgrowth = (diff(finaldataset$Data.GDPC1,1)/finaldataset$Data.GDPC1[1:251])*100

#Transforming Consumer Price Index (CPIAUCSL)
CPIAUCSLgrowth = (diff(finaldataset$Data.CPIAUCSL,1)/finaldataset$Data.CPIAUCSL[1:251])*100

# Transforming M1 money stock (M1REAL)
M1REALgrowth = (diff(finaldataset$Data.M1REAL,1)/finaldataset$Data.M1REAL[1:251])*100

#Transforming to Growth S&P 500 (S.P.500)
S.P.500growth = (diff(finaldataset$Data.S.P.500,1)/finaldataset$Data.S.P.500[1:251])*100

plottisdata <- data.frame(GDPgrowth,
                          finaldataset$Data.CUMFNS[2:252],
                          finaldataset$Data.UNRATESTx[2:252],
                          CPIAUCSLgrowth,
                          finaldataset$Data.FEDFUNDS[2:252],
                          M1REALgrowth,
                          S.P.500growth) 

colnames(plottisdata) <- c('GDP_growth',
                           'CUMFNS',
                           'UNRATESTx',
                           'CPIAUCSL',
                           'FEDFUNDS',
                           'M1REAL_growth',
                           'S.P.500_growth')

# Change the format of the data to get time series data

tis_GDPC1 <- ts(plottisdata$GDP_growth, start= c(1959,2), frequency = 4, end=c(2021,4))
tis_CUMFNS<- ts(plottisdata$CUMFNS, start= c(1959,2), frequency = 4, end=c(2021,4))
tis_UNRATESTx <- ts(plottisdata$UNRATESTx, start= c(1959,2), frequency = 4, end=c(2021,4))
tis_CPIAUCSL <- ts(plottisdata$CPIAUCSL, start= c(1959,2), frequency = 4, end=c(2021,4))
tis_FEDFUNDS <- ts(plottisdata$FEDFUNDS, start= c(1959,2), frequency = 4, end=c(2021,4))
tis_M1REAL <- ts(plottisdata$M1REAL_growth, start= c(1959,2), frequency = 4, end=c(2021,4))
tis_S.P.500 <- ts(plottisdata$S.P.500_growth, start= c(1959,2), frequency = 4, end=c(2021,4))


###########***Summary of the data ***########################################################################

summary(finaldataset)

#*** Growth Gross Domestic Product (GDP) ***#

pdf(file = "Hist_GDP.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 5) # The height of the plot in inches

# Layout to split the screen, here we are creating one column and 2 rows in the visualization.
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8,3))
# Now we set up the margin of the first graph that will be in the in the Layout function
par(mar=c(0, 5, 1.1, 1))
# Draw the histogram of the variable
h1<- hist(GDPgrowth, breaks = 20, plot = FALSE)
h1$counts=(h1$counts)/sum(h1$counts) #Relative frequencies
plot(h1, main= "",xlim=c(-10,10), ylim=c(0,0.60), xaxt="n",
     col="dodgerblue3", ylab="Relative Frecuency",
     panel.first=grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd")))
# Now we set up the margin of the second graph that will be in the Layout function
par(mar=c(5, 5, 0, 1))
boxplot(GDPgrowth, horizontal = TRUE, frame=FALSE, ylim=c(-10,10), col= "goldenrod1", xlab ="GDP growth [%]",xaxt='n')
axis(1,at = seq(-10,10,2))

## Run dev.off() to create the PDF file
dev.off()

# Look for outliers
boxplot.stats(GDPgrowth)$out

#*** Manufacturing industry capacity utilization (CUMFNS) ***#

pdf(file = "Hist_CUMFNS.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 5) # The height of the plot in inches

# Layout to split the screen, here we are creating one column and 2 rows in the visualization.
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8,3))
# Now we set up the margin of the first graph that will be in the in the Layout function
par(mar=c(0, 5, 1.1, 1))
# Draw the histogram of the variable
h2<- hist(plottisdata$CUMFNS, breaks = 20, plot = FALSE)
h2$counts=(h2$counts)/sum(h2$counts) #Relative frequencies
plot(h2, main= "", xlim=c(60,95), ylim=c(0,0.12),
     col="dodgerblue3", ylab="Relative Frecuency", xaxt='n',
     panel.first=grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd")))
# Now we set up the margin of the second graph that will be in the Layout function
par(mar=c(5, 5, 0, 1))
boxplot(plottisdata$CUMFNS, horizontal = TRUE, frame=FALSE, ylim=c(60,95), col= "goldenrod1", xlab ="CUMFNS [%]",xaxt='n')
axis(1,at = seq(60,95,5))

## Run dev.off() to create the PDF file
dev.off()

# Look for outliers
boxplot.stats(plottisdata$CUMFNS)$out

#*** Unemployment rate less than 27 weeks (UNRATESTx) ***#

pdf(file = "Hist_UNRATESTx.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 5) # The height of the plot in inches

# Layout to split the screen, here we are creating one column and 2 rows in the visualization.
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8,3))
# Now we set up the margin of the first graph that will be in the in the Layout function
par(mar=c(0, 5, 1.1, 1))
# Draw the histogram of the variable
h3<- hist(plottisdata$UNRATESTx, breaks = 20, plot = FALSE)
h3$counts=(h3$counts)/sum(h3$counts) #Relative frequencies
plot(h3, main= "", xlim=c(2,14), ylim=c(0,0.25),
     col="dodgerblue3", ylab="Relative Frecuency", xaxt='n',
     panel.first=grid(NA, 5, col = "gray", lty = "dotted",lwd = par("lwd")))
# Now we set up the margin of the second graph that will be in the Layout function
par(mar=c(5, 5, 0, 1))
boxplot(plottisdata$UNRATESTx, horizontal = TRUE, frame=FALSE, ylim=c(2,14), col= "goldenrod1", xlab ="UNRATESTx [%]",xaxt='n')
axis(1,at = seq(2,14,2))

## Run dev.off() to create the PDF file
dev.off()

# Look for outliers
boxplot.stats(plottisdata$UNRATESTx)$out

#*** Consumer price index for all urban consumers  (CPIAUCSL) ***#

pdf(file = "Hist_CPIAUCSL.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 5) # The height of the plot in inches

# Layout to split the screen, here we are creating one column and 2 rows in the visualization.
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8,3))
# Now we set up the margin of the first graph that will be in the in the Layout function
par(mar=c(0, 5, 1.1, 1))
# Draw the histogram of the variable
h4<- hist(plottisdata$CPIAUCSL, breaks = 20, plot = FALSE)
h4$counts=(h4$counts)/sum(h4$counts) #Relative frequencies
plot(h4, main= "", xlim=c(-4,6), ylim=c(0,0.5),
     col="dodgerblue3", ylab="Relative Frecuency", xaxt='n',
     panel.first=grid(NA, 5, col = "gray", lty = "dotted",lwd = par("lwd")))
# Now we set up the margin of the second graph that will be in the Layout function
par(mar=c(5, 5, 0, 1))
boxplot(plottisdata$CPIAUCSL, horizontal = TRUE, frame=FALSE, ylim=c(-4,6), col= "goldenrod1", xlab ="CPIAUCSL growth [%]",xaxt='n')
axis(1,at = seq(-4,6,2))

## Run dev.off() to create the PDF file
dev.off()

# Look for outliers
boxplot.stats(plottisdata$CPIAUCSL)$out

#*** Effective Federal Funds rate (FEDFUNDS) ***#

pdf(file = "Hist_FEDFUNDS.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 5) # The height of the plot in inches

# Layout to split the screen, here we are creating one column and 2 rows in the visualization.
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8,3))
# Now we set up the margin of the first graph that will be in the in the Layout function
par(mar=c(0, 5, 1.1, 1))
# Draw the histogram of the variable
h5<- hist(plottisdata$FEDFUNDS, breaks = 20, plot = FALSE)
h5$counts=(h5$counts)/sum(h5$counts) #Relative frequencies
plot(h5, main= "", xlim=c(-5,20), ylim=c(0,0.20),
     col="dodgerblue3", ylab="Relative Frecuency", xaxt='n',
     panel.first=grid(NA, 5, col = "gray", lty = "dotted",lwd = par("lwd")))
# Now we set up the margin of the second graph that will be in the Layout function
par(mar=c(5, 5, 0, 1))
boxplot(plottisdata$FEDFUNDS, horizontal = TRUE, frame=FALSE, ylim=c(-5,20), col= "goldenrod1", xlab ="FEDFUNDS [%]",xaxt='n')
axis(1,at = seq(-5,20,5))

## Run dev.off() to create the PDF file
dev.off()

# Look for outliers
boxplot.stats(plottisdata$FEDFUNDS)$out

#*** Real M1 money stock (M1REAL) ***#

pdf(file = "Hist_M1REAL.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 5) # The height of the plot in inches

# Layout to split the screen, here we are creating one column and 2 rows in the visualization.
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8,3))
# Now we set up the margin of the first graph that will be in the in the Layout function
par(mar=c(0, 5, 1.1, 1))
# Draw the histogram of the variable
h6<- hist(plottisdata$M1REAL_growth, breaks = 100, plot = FALSE)
h6$counts=(h6$counts)/sum(h6$counts) #Relative frequencies
plot(h6, main= "", 
     col="dodgerblue3", ylab="Relative Frecuency", xaxt='n', xlim=c(-20,220),
     panel.first=grid(NA, 5, col = "gray", lty = "dotted",lwd = par("lwd")))
# Now we set up the margin of the second graph that will be in the Layout function
par(mar=c(5, 5, 0, 1))
boxplot(plottisdata$M1REAL_growth, horizontal = TRUE, frame=FALSE, ylim=c(-20,220), col= "goldenrod1", xlab ="M1REAL growth [%]", xaxt='n')
axis(1,at = seq(-20,220,30))

## Run dev.off() to create the PDF file
dev.off()

# Look for outliers
boxplot.stats(plottisdata$M1REAL_growth)$out

#*** Standard & Poor's 500 common stock price index ***#

pdf(file = "Hist_S.P.500.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 5) # The height of the plot in inches

# Layout to split the screen, here we are creating one column and 2 rows in the visualization.
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8,3))
# Now we set up the margin of the first graph that will be in the in the Layout function
par(mar=c(0, 5, 1.1, 1))
# Draw the histogram of the variable
h7<- hist(plottisdata$S.P.500_growth, breaks = 20, plot = FALSE)
h7$counts=(h7$counts)/sum(h7$counts) #Relative frequencies
plot(h7, main= "", xlim=c(-30,30), ylim=c(0,0.20),
     col="dodgerblue3", ylab="Relative Frecuency", xaxt='n',
     panel.first=grid(NA, 5, col = "gray", lty = "dotted",lwd = par("lwd")))
# Now we set up the margin of the second graph that will be in the Layout function
par(mar=c(5, 5, 0, 1))
boxplot(plottisdata$S.P.500_growth, horizontal = TRUE, frame=FALSE, ylim=c(-30,30), col= "goldenrod1", xlab ="S&P 500 growth [%]", xaxt='n')
axis(1,at = seq(-30,30,10))

## Run dev.off() to create the PDF file
dev.off()

# Look for outliers
boxplot.stats(plottisdata$S.P.500_growth)$out

###########***Part a Ploting Time Series***############################################

# Without transformation 
par(mfrow=c(4,2),mar=c(3, 5, 1.1, 1),mgp=c(2,1,0))
plot(c(1:252),finaldataset$Data.GDPC1, type = 'l', xlab="Time", ylab="GDP")
plot(c(1:252),finaldataset$Data.CUMFNS, type = 'l', xlab="Time", ylab="Capacity Utilization M.")
plot(c(1:252),finaldataset$Data.UNRATESTx, type = 'l', xlab="Time", ylab="Unemployment Rate")
plot(c(1:252),finaldataset$Data.CPIAUCSL, type = 'l', xlab="Time", ylab="Consumer Price Index ")
plot(c(1:252),finaldataset$Data.FEDFUNDS, type = 'l', xlab="Time", ylab="Federal Funds Rate (Percent)")
plot(c(1:252),finaldataset$Data.M1REAL, type = 'l', xlab="Time", ylab="Real M1 Money Stock")
plot(c(1:252),finaldataset$Data.S.P.500, type = 'l', xlab="Time", ylab="S&P 500")


# With transformation 

pdf(file = "Time_Series.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 6) # The height of the plot in inches

par(mfrow=c(4,2),mar=c(3, 5, 1.1, 1),mgp=c(2,1,0))
ts.plot(tis_GDPC1, ylab="GDP Growth [%]", xlab="Years")
ts.plot(tis_CUMFNS, ylab="CUMFNS [%]", xlab="Years")
ts.plot(tis_UNRATESTx, ylab="UNRATESTx [%]", xlab="Years")
ts.plot(tis_CPIAUCSL, ylab="Inflation [%]", xlab="Years")
ts.plot(tis_FEDFUNDS, ylab="FEDFUNDS [%]", xlab="Years")
ts.plot(tis_M1REAL, ylab="M1REAL Growth [%]", xlab="Years")
ts.plot(tis_S.P.500, ylab="S&P 500 Growth [%]", xlab="Years")

## Run dev.off() to create the PDF file
dev.off()

###########***Part b AR(1) model***############################################

#We create a vector to store the predicted values
Forecastingtis_AR= c()

for (i in 1:(length(GDPgrowth)-2)){
  
  ones_AR = rep(1,(1+i))
  yt1_AR = GDPgrowth[1:(1+i)]
  
  z_AR = rbind(ones_AR,yt1_AR)
  Y_AR = rbind(GDPgrowth[2:(2+i)])
        
  C_AR = solve(z_AR%*%t(z_AR))
  B_AR = Y_AR%*%t(z_AR)
  A_AR = B_AR%*%C_AR
  
  pred_AR= A_AR[1,1] +
          (A_AR[1,2]*GDPgrowth[2+i])
    
    Forecastingtis_AR <-  append(Forecastingtis_AR,pred_AR)
  
}

GDPgrowth_graph_AR1 <- ts(GDPgrowth[-c(1:3)], start= c(1960,1), frequency = 4, end=c(2021,4))
Forecastingtis_AR1 <- ts(Forecastingtis_AR, start= c(1960,1), frequency = 4, end=c(2021,4))

GDPgrowth_AR_graph <- GDPgrowth[-c(1:3)]
 
pdf(file = "AR_1_GDP.pdf",   # The directory you want to save the file in
     width = 8, # The width of the plot in inches
     height = 5) # The height of the plot in inches

par(mfrow=c(1,1))
ts.plot(GDPgrowth_graph_AR1, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]",lwd = 2, ylim=c(-15,10))
points(Forecastingtis_AR1, col= 'red', type = 'l', lty= 1,lwd = 2)
 legend("topleft", legend=c("GDP growth","AR(1) forecast GDP growth"),
        col=c("black", "red"), lty=c("solid","solid"),lwd = c(2,2),cex=0.8)

## Run dev.off() to create the PDF file
dev.off()

forecasting_error_AR = (sqrt((1/length(GDPgrowth_AR_graph))*sum((GDPgrowth_AR_graph-Forecastingtis_AR[-c(249)])^2)))
forecasting_error_AR
# forecasting_error_AR1 = 1.206266

###########***Part c VAR(1) model***#########################################

#Estimation of the parameters with all available information

Forecasting_var= c()

for (i in 1:(length(GDPgrowth)-8)){

  ones = rep(1,(7+i))
  yt1 = GDPgrowth[1:(7+i)]
  yt2 = finaldataset$Data.CUMFNS[2:(8+i)]
  yt3 = finaldataset$Data.UNRATESTx[2:(8+i)]
  yt4 = CPIAUCSLgrowth[1:(7+i)]
  yt5 = finaldataset$Data.FEDFUNDS[2:(8+i)]
  yt6 = M1REALgrowth[1:(7+i)]
  yt7 = S.P.500growth[1:(7+i)]

  z = rbind(ones,yt1,yt2,yt3,yt4,yt5,yt6,yt7)
  Y = rbind(GDPgrowth[2:(8+i)],
            finaldataset$Data.CUMFNS[3:(9+i)],
            finaldataset$Data.UNRATESTx[3:(9+i)],
            CPIAUCSLgrowth[2:(8+i)],
            finaldataset$Data.FEDFUNDS[3:(9+i)],
            M1REALgrowth[2:(8+i)],
            S.P.500growth[2:(8+i)])
  
  C = solve(z%*%t(z))
  B = Y%*%t(z)
  A = B%*%C

  pred= A[1,1] +
       (A[1,2]*GDPgrowth[8+i])+
       (A[1,3]*finaldataset$Data.CUMFNS[9+i])+
       (A[1,4]*finaldataset$Data.UNRATESTx[9+i])+
       (A[1,5]*CPIAUCSLgrowth[8+i])+
       (A[1,6]*finaldataset$Data.FEDFUNDS[9+i])+
       (A[1,7]*M1REALgrowth[8+i])+
       (A[1,8]*S.P.500growth[8+i])

  Forecasting_var <-  append(Forecasting_var,pred)
  
}

Resd_1 = Y- (A%*%z)

GDPgrowth_graph_VAR1 <- ts(GDPgrowth[-c(1:9)], start= c(1961,3), frequency = 4, end=c(2021,4))
Forecastingtis_VAR1 <- ts(Forecasting_var[-c(243)], start= c(1961,3), frequency = 4, end=c(2021,4))

GDPgrowth_VAR_graph <- GDPgrowth[-c(1:9)]

pdf(file = "VAR_1_GDP.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 5) # The height of the plot in inches

par(mfrow=c(1,1))
ts.plot(GDPgrowth_graph_AR1, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]",lwd = 2, ylim=c(-15,10))
points(Forecastingtis_VAR1, col= 'blue', type = 'l', lty=1,lwd = 2)
points(Forecastingtis_AR1, col= 'red', type = 'l', lty=1,lwd = 2)
legend("topleft", legend=c("GDP growth","AR(1) forecast GDP growth","VAR(1) forecast GDP growth"),
       col=c("black", "red",'blue'), lty=c("solid","solid","solid"),lwd = c(2,2,2),cex=0.8)

## Run dev.off() to create the PDF file
dev.off()

forecasting_error_VAR = (sqrt((1/length(GDPgrowth_VAR_graph))*sum((GDPgrowth_VAR_graph-Forecasting_var[-c(243)])^2)))
forecasting_error_VAR
# forecasting_error_VAR = 1.980594

###########***Part d Granger causality***##############

#we estimate the variance of the error 

var_P1_1= solve(z%*%t(z))
var_P1_2= ((Resd_1%*%t(Resd_1))/(250-(7*1)-1))
var_P1 = kronecker(var_P1_1,var_P1_2)

# T-test statistic for GDP

t_test_GDP= A[1,2]/sqrt(var_P1[8,8])
t_test_GDP

#t_test_GDP 1.448

# For a large sample size it follows a normal distribution
# at the level of significance alpha = 0.05 we have that for a 
# 2 sided test the critical value is (+/-) 1.96
# because 1.448 < 1.96 we do not reject the null hypothesis, which indicates
# that the parameter estimation for lag 1 GDP could be 0.

t_test_CUMFNS= A[1,3]/sqrt(var_P1[15,15])
t_test_CUMFNS

# t_test_CUMFNS = 4.456844

t_test_UNRATESTx= A[1,4]/sqrt(var_P1[22,22])
t_test_UNRATESTx

# t_test_UNRATESTx = 4.03169

t_test_CPIAUCSL= A[1,5]/sqrt(var_P1[29,29])
t_test_CPIAUCSL

# t_test_CPIAUCSL = -1.607346 

t_test_FEDFUNDS= A[1,6]/sqrt(var_P1[36,36])
t_test_FEDFUNDS

# t_test_FEDFUNDS =  -2.013595

t_test_M1REAL= A[1,7]/sqrt(var_P1[43,43])
t_test_M1REAL

# t_test_M1REAL = 4.618165 

t_test_S.P.500= A[1,8]/sqrt(var_P1[50,50])
t_test_S.P.500

# t_test_S.P.500 = 3.453106

###########***Part e AIC***#########################################
# The forecasting error for VAR(1) was calculated in part c
#AIC For p=1

Forecasting_var_AIC_P1= c()

for (i in 1:(length(GDPgrowth)-11)){
  
  ones_AIC_P1 = rep(1,(7+i))
  yt1_AIC_P1 = GDPgrowth[4:(10+i)]
  yt2_AIC_P1 = finaldataset$Data.CUMFNS[5:(11+i)]
  yt3_AIC_P1 = finaldataset$Data.UNRATESTx[5:(11+i)]
  yt4_AIC_P1 = CPIAUCSLgrowth[4:(10+i)]
  yt5_AIC_P1 = finaldataset$Data.FEDFUNDS[5:(11+i)]
  yt6_AIC_P1 = M1REALgrowth[4:(10+i)]
  yt7_AIC_P1 = S.P.500growth[4:(10+i)]
  
  z_AIC_P1 = rbind(ones_AIC_P1,
            yt1_AIC_P1,
            yt2_AIC_P1,
            yt3_AIC_P1,
            yt4_AIC_P1,
            yt5_AIC_P1,
            yt6_AIC_P1,
            yt7_AIC_P1)
  
  Y_AIC_P1 = rbind(GDPgrowth[5:(11+i)],
            finaldataset$Data.CUMFNS[6:(12+i)],
            finaldataset$Data.UNRATESTx[6:(12+i)],
            CPIAUCSLgrowth[5:(11+i)],
            finaldataset$Data.FEDFUNDS[6:(12+i)],
            M1REALgrowth[5:(11+i)],
            S.P.500growth[5:(11+i)])
  
  C_AIC_P1 = solve(z_AIC_P1%*%t(z_AIC_P1))
  B_AIC_P1 = Y_AIC_P1%*%t(z_AIC_P1)
  A_AIC_P1 = B_AIC_P1%*%C_AIC_P1
  
  pred_AIC_P1= A_AIC_P1[1,1] +
    (A_AIC_P1[1,2]*GDPgrowth[12+i])+
    (A_AIC_P1[1,3]*finaldataset$Data.CUMFNS[13+i])+
    (A_AIC_P1[1,4]*finaldataset$Data.UNRATESTx[13+i])+
    (A_AIC_P1[1,5]*CPIAUCSLgrowth[12+i])+
    (A_AIC_P1[1,6]*finaldataset$Data.FEDFUNDS[13+i])+
    (A_AIC_P1[1,7]*M1REALgrowth[12+i])+
    (A_AIC_P1[1,8]*S.P.500growth[12+i])
  
  Forecasting_var_AIC_P1 <-  append(Forecasting_var_AIC_P1,pred_AIC_P1)
  
}


Resd_1_AIC_P1 = Y_AIC_P1- (A_AIC_P1%*%z_AIC_P1)
Cov_resM1_T_AIC_P1 = (Resd_1_AIC_P1%*%t(Resd_1_AIC_P1))/247
AIC_P1= log(det(Cov_resM1_T_AIC_P1)) + ((2/247)*(1*(7^2)+7))

AIC_P1
#AIC_P1 = 2.053082

#For p=2, first we calculate the forecasting error with all available information

Forecasting_var_P2= c()

for (i in 1:(length(GDPgrowth)-16)){
  
  ones_P2 = rep(1,(14+i))
  yt1_P2 = GDPgrowth[2:(15+i)]
  yt2_P2 = finaldataset$Data.CUMFNS[3:(16+i)]
  yt3_P2 = finaldataset$Data.UNRATESTx[3:(16+i)]
  yt4_P2 = CPIAUCSLgrowth[2:(15+i)]
  yt5_P2 = finaldataset$Data.FEDFUNDS[3:(16+i)]
  yt6_P2 = M1REALgrowth[2:(15+i)]
  yt7_P2 = S.P.500growth[2:(15+i)]
  yt8_P2 = GDPgrowth[1:(14+i)]
  yt9_P2 = finaldataset$Data.CUMFNS[2:(15+i)]
  yt10_P2 = finaldataset$Data.UNRATESTx[2:(15+i)]
  yt11_P2 = CPIAUCSLgrowth[1:(14+i)]
  yt12_P2 = finaldataset$Data.FEDFUNDS[2:(15+i)]
  yt13_P2 = M1REALgrowth[1:(14+i)]
  yt14_P2 = S.P.500growth[1:(14+i)]
  
  z_P2 = rbind(ones_P2,
               yt1_P2,
               yt2_P2,
               yt3_P2,
               yt4_P2,
               yt5_P2,
               yt6_P2,
               yt7_P2,
               yt8_P2,
               yt9_P2,
               yt10_P2,
               yt11_P2,
               yt12_P2,
               yt13_P2,
               yt14_P2)
  
  Y_P2 = rbind(GDPgrowth[3:(16+i)],
               finaldataset$Data.CUMFNS[4:(17+i)],
               finaldataset$Data.UNRATESTx[4:(17+i)],
               CPIAUCSLgrowth[3:(16+i)],
               finaldataset$Data.FEDFUNDS[4:(17+i)],
               M1REALgrowth[3:(16+i)],
               S.P.500growth[3:(16+i)])
  
  C_P2 = solve(z_P2%*%t(z_P2))
  B_P2 = Y_P2%*%t(z_P2)
  A_P2 = B_P2%*%C_P2
  
  pred_P2= A_P2[1,1] +
    (A_P2[1,2]*GDPgrowth[16+i])+
    (A_P2[1,3]*finaldataset$Data.CUMFNS[17+i])+
    (A_P2[1,4]*finaldataset$Data.UNRATESTx[17+i])+
    (A_P2[1,5]*CPIAUCSLgrowth[16+i])+
    (A_P2[1,6]*finaldataset$Data.FEDFUNDS[17+i])+
    (A_P2[1,7]*M1REALgrowth[16+i])+
    (A_P2[1,8]*S.P.500growth[16+i])+
    (A_P2[1,9]*GDPgrowth[15+i])+
    (A_P2[1,10]*finaldataset$Data.CUMFNS[16+i])+
    (A_P2[1,11]*finaldataset$Data.UNRATESTx[16+i])+
    (A_P2[1,12]*CPIAUCSLgrowth[15+i])+
    (A_P2[1,13]*finaldataset$Data.FEDFUNDS[16+i])+
    (A_P2[1,14]*M1REALgrowth[15+i])+
    (A_P2[1,15]*S.P.500growth[15+i])
  
  Forecasting_var_P2 <-  append(Forecasting_var_P2,pred_P2)

}

GDPgrowth_VAR_graph_P2 <- GDPgrowth[-c(1:17)]

par(mfrow=c(1,1))
ts.plot(GDPgrowth_VAR_graph_P2, type = 'l', xlab="Time", ylab="GDP Growth")
points(Forecasting_var_P2[-c(249)], col= 'red', type = 'l', lty=5)

forecasting_error_VAR_P2 = (sqrt((1/length(GDPgrowth_VAR_graph_P2))*sum((GDPgrowth_VAR_graph_P2-Forecasting_var_P2[-c(235)])^2)))
forecasting_error_VAR_P2

# forecasting_error_VAR = 3.003182

# AIC for P=2

Forecasting_var_AIC_P2= c()

for (i in 1:(length(GDPgrowth)-18)){
  
  ones_AIC_P2 = rep(1,(14+i))
  yt1_AIC_P2 = GDPgrowth[4:(17+i)]
  yt2_AIC_P2 = finaldataset$Data.CUMFNS[5:(18+i)]
  yt3_AIC_P2 = finaldataset$Data.UNRATESTx[5:(18+i)]
  yt4_AIC_P2 = CPIAUCSLgrowth[4:(17+i)]
  yt5_AIC_P2 = finaldataset$Data.FEDFUNDS[5:(18+i)]
  yt6_AIC_P2 = M1REALgrowth[4:(17+i)]
  yt7_AIC_P2 = S.P.500growth[4:(17+i)]
  yt8_AIC_P2 = GDPgrowth[3:(16+i)]
  yt9_AIC_P2 = finaldataset$Data.CUMFNS[4:(17+i)]
  yt10_AIC_P2 = finaldataset$Data.UNRATESTx[4:(17+i)]
  yt11_AIC_P2 = CPIAUCSLgrowth[3:(16+i)]
  yt12_AIC_P2 = finaldataset$Data.FEDFUNDS[4:(17+i)]
  yt13_AIC_P2 = M1REALgrowth[3:(16+i)]
  yt14_AIC_P2 = S.P.500growth[3:(16+i)]
  
  z_AIC_P2 = rbind(ones_AIC_P2,
                   yt1_AIC_P2,
                   yt2_AIC_P2,
                   yt3_AIC_P2,
                   yt4_AIC_P2,
                   yt5_AIC_P2,
                   yt6_AIC_P2,
                   yt7_AIC_P2,
                   yt8_AIC_P2,
                   yt9_AIC_P2,
                   yt10_AIC_P2,
                   yt11_AIC_P2,
                   yt12_AIC_P2,
                   yt13_AIC_P2,
                   yt14_AIC_P2)
  
  Y_AIC_P2 = rbind(GDPgrowth[5:(18+i)],
                   finaldataset$Data.CUMFNS[6:(19+i)],
                   finaldataset$Data.UNRATESTx[6:(19+i)],
                   CPIAUCSLgrowth[5:(18+i)],
                   finaldataset$Data.FEDFUNDS[6:(19+i)],
                   M1REALgrowth[5:(18+i)],
                   S.P.500growth[5:(18+i)])
  
  C_AIC_P2 = solve(z_AIC_P2%*%t(z_AIC_P2))
  B_AIC_P2 = Y_AIC_P2%*%t(z_AIC_P2)
  A_AIC_P2 = B_AIC_P2%*%C_AIC_P2
  
  pred_AIC_P2= A_AIC_P2[1,1] +
               (A_AIC_P2[1,2]*GDPgrowth[18+i])+
              (A_AIC_P2[1,3]*finaldataset$Data.CUMFNS[19+i])+
              (A_AIC_P2[1,4]*finaldataset$Data.UNRATESTx[19+i])+
              (A_AIC_P2[1,5]*CPIAUCSLgrowth[18+i])+
              (A_AIC_P2[1,6]*finaldataset$Data.FEDFUNDS[19+i])+
              (A_AIC_P2[1,7]*M1REALgrowth[18+i])+
              (A_AIC_P2[1,8]*S.P.500growth[18+i])+
              (A_AIC_P2[1,9]*GDPgrowth[17+i])+
              (A_AIC_P2[1,10]*finaldataset$Data.CUMFNS[18+i])+
              (A_AIC_P2[1,11]*finaldataset$Data.UNRATESTx[18+i])+
              (A_AIC_P2[1,12]*CPIAUCSLgrowth[17+i])+
              (A_AIC_P2[1,13]*finaldataset$Data.FEDFUNDS[18+i])+
              (A_AIC_P2[1,14]*M1REALgrowth[17+i])+
              (A_AIC_P2[1,15]*S.P.500growth[17+i])
  
  Forecasting_var_AIC_P2 <-  append(Forecasting_var_AIC_P2,pred_AIC_P2)
  
}


Resd_1_AIC_P2 = Y_AIC_P2 - (A_AIC_P2%*%z_AIC_P2)
Cov_resM1_T_AIC_P2 = (Resd_1_AIC_P2%*%t(Resd_1_AIC_P2))/247
AIC_P2= log(det(Cov_resM1_T_AIC_P2)) + ((2/247)*(2*(7^2)+7))

AIC_P2

#AIC_P2 = 1.407417

#For p=3, first we calculate the forecasting error with all available information

Forecasting_var_P3= c()

for (i in 1:(length(GDPgrowth)-24)){
  
  ones_P3 = rep(1,(21+i))
  yt1_P3 = GDPgrowth[3:(23+i)]
  yt2_P3 = finaldataset$Data.CUMFNS[4:(24+i)]
  yt3_P3 = finaldataset$Data.UNRATESTx[4:(24+i)]
  yt4_P3 = CPIAUCSLgrowth[3:(23+i)]
  yt5_P3 = finaldataset$Data.FEDFUNDS[4:(24+i)]
  yt6_P3 = M1REALgrowth[3:(23+i)]
  yt7_P3 = S.P.500growth[3:(23+i)]
  yt8_P3 = GDPgrowth[2:(22+i)]
  yt9_P3 = finaldataset$Data.CUMFNS[3:(23+i)]
  yt10_P3 = finaldataset$Data.UNRATESTx[3:(23+i)]
  yt11_P3 = CPIAUCSLgrowth[2:(22+i)]
  yt12_P3 = finaldataset$Data.FEDFUNDS[3:(23+i)]
  yt13_P3 = M1REALgrowth[2:(22+i)]
  yt14_P3 = S.P.500growth[2:(22+i)]
  yt15_P3 = GDPgrowth[1:(21+i)]
  yt16_P3 = finaldataset$Data.CUMFNS[2:(22+i)]
  yt17_P3 = finaldataset$Data.UNRATESTx[2:(22+i)]
  yt18_P3 = CPIAUCSLgrowth[1:(21+i)]
  yt19_P3 = finaldataset$Data.FEDFUNDS[2:(22+i)]
  yt20_P3 = M1REALgrowth[1:(21+i)]
  yt21_P3 = S.P.500growth[1:(21+i)]

  z_P3 = rbind(ones_P3,
               yt1_P3,
               yt2_P3,
               yt3_P3,
               yt4_P3,
               yt5_P3,
               yt6_P3,
               yt7_P3,
               yt8_P3,
               yt9_P3,
               yt10_P3,
               yt11_P3,
               yt12_P3,
               yt13_P3,
               yt14_P3,
               yt15_P3,
               yt16_P3,
               yt17_P3,
               yt18_P3,
               yt19_P3,
               yt20_P3,
               yt21_P3)
  
  Y_P3 = rbind(GDPgrowth[4:(24+i)],
               finaldataset$Data.CUMFNS[5:(25+i)],
               finaldataset$Data.UNRATESTx[5:(25+i)],
               CPIAUCSLgrowth[4:(24+i)],
               finaldataset$Data.FEDFUNDS[5:(25+i)],
               M1REALgrowth[4:(24+i)],
               S.P.500growth[4:(24+i)])
  
  C_P3 = solve(z_P3%*%t(z_P3))
  B_P3 = Y_P3%*%t(z_P3)
  A_P3 = B_P3%*%C_P3
  
  pred_P3= A_P3[1,1] +
    (A_P3[1,2]*GDPgrowth[24+i])+
    (A_P3[1,3]*finaldataset$Data.CUMFNS[25+i])+
    (A_P3[1,4]*finaldataset$Data.UNRATESTx[25+i])+
    (A_P3[1,5]*CPIAUCSLgrowth[24+i])+
    (A_P3[1,6]*finaldataset$Data.FEDFUNDS[25+i])+
    (A_P3[1,7]*M1REALgrowth[24+i])+
    (A_P3[1,8]*S.P.500growth[24+i])+
    (A_P3[1,9]*GDPgrowth[23+i])+
    (A_P3[1,10]*finaldataset$Data.CUMFNS[24+i])+
    (A_P3[1,11]*finaldataset$Data.UNRATESTx[24+i])+
    (A_P3[1,12]*CPIAUCSLgrowth[23+i])+
    (A_P3[1,13]*finaldataset$Data.FEDFUNDS[24+i])+
    (A_P3[1,14]*M1REALgrowth[23+i])+
    (A_P3[1,15]*S.P.500growth[23+i])+
    (A_P3[1,16]*GDPgrowth[22+i])+
    (A_P3[1,17]*finaldataset$Data.CUMFNS[23+i])+
    (A_P3[1,18]*finaldataset$Data.UNRATESTx[23+i])+
    (A_P3[1,19]*CPIAUCSLgrowth[22+i])+
    (A_P3[1,20]*finaldataset$Data.FEDFUNDS[23+i])+
    (A_P3[1,21]*M1REALgrowth[22+i])+
    (A_P3[1,22]*S.P.500growth[22+i])

  Forecasting_var_P3 <- append(Forecasting_var_P3,pred_P3)

}


GDPgrowth_graph_VAR3 <- ts(GDPgrowth[-c(1:25)], start= c(1965,3), frequency = 4, end=c(2021,4))
Forecastingtis_VAR3 <- ts(Forecasting_var_P3[-c(227)], start= c(1965,3), frequency = 4, end=c(2021,4))
Forecasting_VAR1_P3 <- ts(Forecasting_var[17:242], start= c(1965,3), frequency = 4, end=c(2021,4))
Forecastingtis_P3 <- ts(Forecastingtis_AR1[23:248], start= c(1965,3), frequency = 4, end=c(2021,4))

GDPgrowth_VAR_graph_P3 <- GDPgrowth[-c(1:25)]

pdf(file = "VAR_3_GDP.pdf",   # The directory you want to save the file in
    width = 15, # The width of the plot in inches
    height = 10) # The height of the plot in inches


par(mfrow=c(1,1))
ts.plot(GDPgrowth_graph_VAR3, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]", ylim=c(-35,15),lwd = 2)
points(Forecastingtis_VAR3, col= 'magenta2', type = 'l', lty=1,lwd = 2)
points(Forecasting_VAR1_P3, col= 'blue', type = 'l', lty=1,lwd = 2)
points(Forecastingtis_P3, col= 'red', type = 'l', lty=1,lwd = 2)
legend("bottomleft", legend=c("GDP growth","AR(1) forecast GDP growth","VAR(1) forecast GDP growth","VAR(3) forecast GDP growth"),
       col=c("black", "red",'blue','magenta2'), lty=c("solid","solid","solid","solid"),lwd = c(3,3,3,3), cex = 1.5)

## Run dev.off() to create the PDF file
dev.off()

forecasting_error_VAR_P3= (sqrt((1/length(GDPgrowth_VAR_graph_P3))*sum((GDPgrowth_VAR_graph_P3-Forecasting_var_P3[-c(227)])^2)))
forecasting_error_VAR_P3

# forecasting_error_VAR = 3.87826

# AIC for P=3

Forecasting_var_AIC_P3= c()

for (i in 1:(length(GDPgrowth)-25)){
  
  ones_AIC_P3 = rep(1,(21+i))
  yt1_AIC_P3 = GDPgrowth[4:(24+i)]
  yt2_AIC_P3 = finaldataset$Data.CUMFNS[5:(25+i)]
  yt3_AIC_P3 = finaldataset$Data.UNRATESTx[5:(25+i)]
  yt4_AIC_P3 = CPIAUCSLgrowth[4:(24+i)]
  yt5_AIC_P3 = finaldataset$Data.FEDFUNDS[5:(25+i)]
  yt6_AIC_P3 = M1REALgrowth[4:(24+i)]
  yt7_AIC_P3 = S.P.500growth[4:(24+i)]
  yt8_AIC_P3 = GDPgrowth[3:(23+i)]
  yt9_AIC_P3 = finaldataset$Data.CUMFNS[4:(24+i)]
  yt10_AIC_P3 = finaldataset$Data.UNRATESTx[4:(24+i)]
  yt11_AIC_P3 = CPIAUCSLgrowth[3:(23+i)]
  yt12_AIC_P3 = finaldataset$Data.FEDFUNDS[4:(24+i)]
  yt13_AIC_P3 = M1REALgrowth[3:(23+i)]
  yt14_AIC_P3 = S.P.500growth[3:(23+i)]
  yt15_AIC_P3 = GDPgrowth[2:(22+i)]
  yt16_AIC_P3 = finaldataset$Data.CUMFNS[3:(23+i)]
  yt17_AIC_P3 = finaldataset$Data.UNRATESTx[3:(23+i)]
  yt18_AIC_P3 = CPIAUCSLgrowth[2:(22+i)]
  yt19_AIC_P3 = finaldataset$Data.FEDFUNDS[3:(23+i)]
  yt20_AIC_P3 = M1REALgrowth[2:(22+i)]
  yt21_AIC_P3 = S.P.500growth[2:(22+i)]
  
  z_AIC_P3 = rbind(ones_AIC_P3,
                   yt1_AIC_P3,
                   yt2_AIC_P3,
                   yt3_AIC_P3,
                   yt4_AIC_P3,
                   yt5_AIC_P3,
                   yt6_AIC_P3,
                   yt7_AIC_P3,
                   yt8_AIC_P3,
                   yt9_AIC_P3,
                   yt10_AIC_P3,
                   yt11_AIC_P3,
                   yt12_AIC_P3,
                   yt13_AIC_P3,
                   yt14_AIC_P3,
                   yt15_AIC_P3,
                   yt16_AIC_P3,
                   yt17_AIC_P3,
                   yt18_AIC_P3,
                   yt19_AIC_P3,
                   yt20_AIC_P3,
                   yt21_AIC_P3)
  
  Y_AIC_P3 = rbind(GDPgrowth[5:(25+i)],
                   finaldataset$Data.CUMFNS[6:(26+i)],
                   finaldataset$Data.UNRATESTx[6:(26+i)],
                   CPIAUCSLgrowth[5:(25+i)],
                   finaldataset$Data.FEDFUNDS[6:(26+i)],
                   M1REALgrowth[5:(25+i)],
                   S.P.500growth[5:(25+i)])
  
  C_AIC_P3 = solve(z_AIC_P3%*%t(z_AIC_P3))
  B_AIC_P3 = Y_AIC_P3%*%t(z_AIC_P3)
  A_AIC_P3 = B_AIC_P3%*%C_AIC_P3
  
  pred_AIC_P3= A_AIC_P3[1,1] +
    (A_AIC_P3[1,2]*GDPgrowth[25+i])+
    (A_AIC_P3[1,3]*finaldataset$Data.CUMFNS[26+i])+
    (A_AIC_P3[1,4]*finaldataset$Data.UNRATESTx[26+i])+
    (A_AIC_P3[1,5]*CPIAUCSLgrowth[25+i])+
    (A_AIC_P3[1,6]*finaldataset$Data.FEDFUNDS[26+i])+
    (A_AIC_P3[1,7]*M1REALgrowth[25+i])+
    (A_AIC_P3[1,8]*S.P.500growth[25+i])+
    (A_AIC_P3[1,9]*GDPgrowth[24+i])+
    (A_AIC_P3[1,10]*finaldataset$Data.CUMFNS[25+i])+
    (A_AIC_P3[1,11]*finaldataset$Data.UNRATESTx[25+i])+
    (A_AIC_P3[1,12]*CPIAUCSLgrowth[24+i])+
    (A_AIC_P3[1,13]*finaldataset$Data.FEDFUNDS[25+i])+
    (A_AIC_P3[1,14]*M1REALgrowth[24+i])+
    (A_AIC_P3[1,15]*S.P.500growth[24+i])+
    (A_AIC_P3[1,16]*GDPgrowth[23+i])+
    (A_AIC_P3[1,17]*finaldataset$Data.CUMFNS[24+i])+
    (A_AIC_P3[1,18]*finaldataset$Data.UNRATESTx[24+i])+
    (A_AIC_P3[1,19]*CPIAUCSLgrowth[23+i])+
    (A_AIC_P3[1,20]*finaldataset$Data.FEDFUNDS[24+i])+
    (A_AIC_P3[1,21]*M1REALgrowth[23+i])+
    (A_AIC_P3[1,22]*S.P.500growth[23+i])
  
  Forecasting_var_AIC_P3 <-  append(Forecasting_var_AIC_P3,pred_AIC_P3)
  
}


Resd_1_AIC_P3 = Y_AIC_P3 - (A_AIC_P3%*%z_AIC_P3)
Cov_resM1_T_AIC_P3 = (Resd_1_AIC_P3%*%t(Resd_1_AIC_P3))/247
AIC_P3= log(det(Cov_resM1_T_AIC_P3)) + ((2/247)*(3*(7^2)+7))

AIC_P3

#AIC_P3 = 1.327928


#For p=4

Forecasting_var_P4= c()

for (i in 1:(length(GDPgrowth)-32)){
  
  ones_P4 = rep(1,(28+i))
  yt1_P4 = GDPgrowth[4:(31+i)]
  yt2_P4 = finaldataset$Data.CUMFNS[5:(32+i)]
  yt3_P4 = finaldataset$Data.UNRATESTx[5:(32+i)]
  yt4_P4 = CPIAUCSLgrowth[4:(31+i)]
  yt5_P4 = finaldataset$Data.FEDFUNDS[5:(32+i)]
  yt6_P4 = M1REALgrowth[4:(31+i)]
  yt7_P4 = S.P.500growth[4:(31+i)]
  yt8_P4 = GDPgrowth[3:(30+i)]
  yt9_P4 = finaldataset$Data.CUMFNS[4:(31+i)]
  yt10_P4 = finaldataset$Data.UNRATESTx[4:(31+i)]
  yt11_P4 = CPIAUCSLgrowth[3:(30+i)]
  yt12_P4 = finaldataset$Data.FEDFUNDS[4:(31+i)]
  yt13_P4 = M1REALgrowth[3:(30+i)]
  yt14_P4 = S.P.500growth[3:(30+i)]
  yt15_P4 = GDPgrowth[2:(29+i)]
  yt16_P4 = finaldataset$Data.CUMFNS[3:(30+i)]
  yt17_P4 = finaldataset$Data.UNRATESTx[3:(30+i)]
  yt18_P4 = CPIAUCSLgrowth[2:(29+i)]
  yt19_P4 = finaldataset$Data.FEDFUNDS[3:(30+i)]
  yt20_P4 = M1REALgrowth[2:(29+i)]
  yt21_P4 = S.P.500growth[2:(29+i)]
  yt22_P4 = GDPgrowth[1:(28+i)]
  yt23_P4 = finaldataset$Data.CUMFNS[2:(29+i)]
  yt24_P4 = finaldataset$Data.UNRATESTx[2:(29+i)]
  yt25_P4 = CPIAUCSLgrowth[1:(28+i)]
  yt26_P4 = finaldataset$Data.FEDFUNDS[2:(29+i)]
  yt27_P4 = M1REALgrowth[1:(28+i)]
  yt28_P4 = S.P.500growth[1:(28+i)]
  
  z_P4 = rbind(ones_P4,
               yt1_P4,
               yt2_P4,
               yt3_P4,
               yt4_P4,
               yt5_P4,
               yt6_P4,
               yt7_P4,
               yt8_P4,
               yt9_P4,
               yt10_P4,
               yt11_P4,
               yt12_P4,
               yt13_P4,
               yt14_P4,
               yt15_P4,
               yt16_P4,
               yt17_P4,
               yt18_P4,
               yt19_P4,
               yt20_P4,
               yt21_P4,
               yt22_P4,
               yt23_P4,
               yt24_P4,
               yt25_P4,
               yt26_P4,
               yt27_P4,
               yt28_P4)
  
  Y_P4 = rbind(GDPgrowth[5:(32+i)],
               finaldataset$Data.CUMFNS[6:(33+i)],
               finaldataset$Data.UNRATESTx[6:(33+i)],
               CPIAUCSLgrowth[5:(32+i)],
               finaldataset$Data.FEDFUNDS[6:(33+i)],
               M1REALgrowth[5:(32+i)],
               S.P.500growth[5:(32+i)])
  
  C_P4 = solve(z_P4%*%t(z_P4))
  B_P4 = Y_P4%*%t(z_P4)
  A_P4 = B_P4%*%C_P4
  
  pred_P4= A_P4[1,1] +
    (A_P4[1,2]*GDPgrowth[32+i])+
    (A_P4[1,3]*finaldataset$Data.CUMFNS[33+i])+
    (A_P4[1,4]*finaldataset$Data.UNRATESTx[33+i])+
    (A_P4[1,5]*CPIAUCSLgrowth[32+i])+
    (A_P4[1,6]*finaldataset$Data.FEDFUNDS[33+i])+
    (A_P4[1,7]*M1REALgrowth[32+i])+
    (A_P4[1,8]*S.P.500growth[32+i])+
    (A_P4[1,9]*GDPgrowth[31+i])+
    (A_P4[1,10]*finaldataset$Data.CUMFNS[32+i])+
    (A_P4[1,11]*finaldataset$Data.UNRATESTx[32+i])+
    (A_P4[1,12]*CPIAUCSLgrowth[31+i])+
    (A_P4[1,13]*finaldataset$Data.FEDFUNDS[32+i])+
    (A_P4[1,14]*M1REALgrowth[31+i])+
    (A_P4[1,15]*S.P.500growth[31+i])+
    (A_P4[1,16]*GDPgrowth[30+i])+
    (A_P4[1,17]*finaldataset$Data.CUMFNS[31+i])+
    (A_P4[1,18]*finaldataset$Data.UNRATESTx[31+i])+
    (A_P4[1,19]*CPIAUCSLgrowth[30+i])+
    (A_P4[1,20]*finaldataset$Data.FEDFUNDS[31+i])+
    (A_P4[1,21]*M1REALgrowth[30+i])+
    (A_P4[1,22]*S.P.500growth[30+i])+
    (A_P4[1,23]*GDPgrowth[29+i])+
    (A_P4[1,24]*finaldataset$Data.CUMFNS[30+i])+
    (A_P4[1,25]*finaldataset$Data.UNRATESTx[30+i])+
    (A_P4[1,26]*CPIAUCSLgrowth[29+i])+
    (A_P4[1,27]*finaldataset$Data.FEDFUNDS[30+i])+
    (A_P4[1,28]*M1REALgrowth[29+i])+
    (A_P4[1,29]*S.P.500growth[29+i])
  
  
  Forecasting_var_P4 <- append(Forecasting_var_P4,pred_P4)
  
}

Resd_2_P4= Y_P4-(A_P4%*%z_P4)

Cov_resM4_T = (Resd_2_P4%*%t(Resd_2_P4))/247

AIC_P4= log(det(Cov_resM4_T)) + ((2/247)*((4*(7^2))+7))

AIC_P4

#AIC_P4 = 1.442107

GDPgrowth_VAR_graph_P4 <- GDPgrowth[-c(1:33)]

par(mfrow=c(1,1))
ts.plot(GDPgrowth_VAR_graph_P4, type = 'l', xlab="Time", ylab="GDP Growth", ylim=c(-40,20))
points(Forecasting_var_P4, col= 'red', type = 'l', lty=5)
# points(Forecastingtis[7:248], col= 'blue', type = 'l', lty=5)

forecasting_error_VAR_P4= (sqrt((1/length(GDPgrowth_VAR_graph_P4))*sum((GDPgrowth_VAR_graph_P4-Forecasting_var_P4[-c(219)])^2)))
forecasting_error_VAR_P4

# forecasting_error_VAR = 4.099632


############ Model Comparison ######

GDPgrowth_AR_graph_65 <- GDPgrowth[-c(1:25)]

forecasting_error_AR_65 = (sqrt((1/length(GDPgrowth_AR_graph_65))*sum((GDPgrowth_AR_graph_65-Forecastingtis_AR[-c(1:22,249)])^2)))
forecasting_error_AR_65

forecasting_error_VAR_65 = (sqrt((1/length(GDPgrowth_AR_graph_65))*sum((GDPgrowth_AR_graph_65-Forecasting_var[-c(1:16,243)])^2)))
forecasting_error_VAR_65
