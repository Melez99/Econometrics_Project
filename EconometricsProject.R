###########################################################################################################################################################

##### Econometrics project ######

#Load libraries
library(AER)
library(MASS)
library(dynlm)
library(forecast)
library(readxl)
library(stargazer)
library(scales)
library(quantmod)
library(urca)
library(tseries)
library(vars)
library(ggplot2)
library(rmarkdown)        
library(tinytex) 

# set the working directory
setwd("C:/Users/cex/OneDrive/Desktop/Ariel")


#read database file
databaseproject <- read.csv("C:/Users/cex/OneDrive/Desktop/Ariel/Econometrics Assignment 21 data.csv")

# view data
ls(databaseproject)
head(databaseproject)
View(databaseproject)

#clean data 
databaseproject<-na.omit(databaseproject)


#Check how R has stored the data
str(data)

#Format date column
databaseproject$Year <- as.Date(paste(databaseproject$Year,12,31,sep="-"))

#Adjust column names
colnames(databaseproject)[1]<-"Year"

#Average hourly earnings
attach(databaseproject)
databaseproject$AHW_F <- WBR_F.after.tax/E_F
databaseproject$AHW_M <- WBR_M..after.tax/E_M

#GPG
attach(databaseproject)
databaseproject$G_W_Gap <- (AHW_M - AHW_F)/AHW_M

#GDP
attach(databaseproject)
databaseproject$GDP <- ((AHW_M * E_M) + (AHW_F * E_F)) + PPR.after.tax

#Logs to linearise exponential trends
databaseproject$ln_GDP<-log(databaseproject$GDP)
databaseproject$ln_AHW_M<-log(databaseproject$AHW_M)
databaseproject$ln_E_M<-log(databaseproject$E_M)
databaseproject$ln_AHW_F<-log(databaseproject$AHW_F)
databaseproject$ln_E_F<-log(databaseproject$E_F)
databaseproject$ln_G_W_Gap<-log(databaseproject$G_W_Gap)
databaseproject$ln_PPR.after.tax<-log(databaseproject$PPR.after.tax)
databaseproject$ln_WBR_M<-log(databaseproject$WBR_M)

#Create time-series vector
ts.databaseproject<-ts(databaseproject[,2:15],start=c(1970),frequency = 1)

### GDP, AHW_M, AHW_F, GPG, E_F, and E_M plots ###

#GDP plot
attach(databaseproject)
year=ts(1970:2012)

GDP_plot=ts(GDP,start=c(1970),frequency=1)

plot(GDP_plot,
     col = "dark green",
     lwd = 2,
     ylab = "GDP",
     xlab = "Year",
     main = "U.K. Yearly Real GDP")

#GDP Growth log in p.p.
attach(databaseproject)
year=ts(1970:2012)
ln_GDP_plot=ts(ln_GDP,start=c(1970),frequency=1)
plot(ln_GDP_plot,
     col = "orange",
     lwd = 2,
     ylab = "GDP",
     xlab = "Year",
     main = "U.K. Yearly Real GDP Growth in p.p.")

#Average Hourly Wages comparison
attach(databaseproject)
plot(Year,
     ln_AHW_M,
     type="l",
     col = 4,
     ylim=c(1.7,3),
     ylab = "Average Hourly Wage in p.p.",
     xlab = "Year",
     main = "U.K.Average Hourly Wage for Males and Females in p.p.")
lines(Year,
      ln_AHW_F,
      type="l",
      col = 2)
legend("bottomright", legend=c("Males", "Females"),
       col=c("blue", "red"), lty=1, cex=0.7)


#Gender Wage Gap plot
attach(databaseproject)
GenderwageGap_plot=ts(G_W_Gap,start=c(1970),frequency=1)
plot(GenderwageGap_plot,
     col = "black",
     lwd = 2,
     ylab = "Relative Wage Ratio",
     xlab = "Year",
     main = "U.K.Relative Gender Wage Ratio")

#Gender Wage Gap plot in p.p.
attach(databaseproject)
ln_GenderwageGap_plot=ts(ln_G_W_Gap,start=c(1970),frequency=1)
plot(ln_GenderwageGap_plot,
     col = "black",
     lwd = 2,
     ylab = "Growth Rate in p.p.",
     xlab = "Year",
     main = "Relative Gender Wage Gap Growth Rate in p.p.")


#Average employment rate in hours by gender
attach(databaseproject)
plot(Year,
     E_M,
     type="l",
     col = "blue",
     ylim=c(19, 32),
     ylab = "Average Hours",
     xlab = "Year",
     main = "U.K.Average employment rate in hours by gender")
lines(Year,
      E_F,
      type="l",
      col = "red")
legend("bottomright", legend=c("Males", "Females"),
       col=c("blue", "red"), lty=1, cex=0.7)


### Regression analysis ###

##Regression on levels##
attach(databaseproject)
Model_1 <-lm(ln_GDP~ln_AHW_M+ln_G_W_Gap+ln_E_F+ln_WBR_M+ln_PPR.after.tax, data=databaseproject)
summary(Model_1)
head(fortify(Model_1))
residPlot<-ggplot(aes(x=.fitted, y=.resid), data=Model_1)+geom_line()+geom_hline(yintercept=0)+labs(x="Fitted Values", y="Residual")
residPlot

#Log of GDP variable
LogGDP<-log(GDP)

## Autocorrelation tests ##

# Durbin-Watson Test #
durbinWatsonTest(Model_1)
plot(Model_1$residuals)
plot.ts(Model_1$residuals)


## Unit root tests ##

# AIC Augmented Dickey Fuller (ADF) #

#LogGDP
ADF_test_trend_LogGDP_AIC<-ur.df(LogGDP, 
                                 type = "trend", 
                                 lags = 15, 
                                 selectlags = "AIC")
summary(ADF_test_trend_LogGDP_AIC)

#ln_AHW_M
ADF_test_trend_ln_AHW_M_AIC<-ur.df(ln_AHW_M, 
                                   type = "trend", 
                                   lags = 15, 
                                   selectlags = "AIC")
summary(ADF_test_trend_ln_AHW_M_AIC)

#ln_W_Gap
ADF_test_trend_GWGAP_AIC<-ur.df(ln_G_W_Gap, 
                                type = "trend", 
                                lags = 15, 
                                selectlags = "AIC")
summary(ADF_test_trend_GWGAP_AIC)

#ln_E_F
ADF_test_trend_ln_E_F_AIC<-ur.df(ln_E_F, 
                                 type = "trend", 
                                 lags = 15, 
                                 selectlags = "AIC")
summary(ADF_test_trend_ln_E_F_AIC)

#ln_WBR_M
ADF_test_trend_ln_WBR_M_AIC<-ur.df(ln_WBR_M, 
                                   type = "trend", 
                                   lags = 15, 
                                   selectlags = "AIC")
summary(ADF_test_trend_ln_WBR_M_AIC)

#ln_PPR.after.tax
ADF_test_trend_ln_PPR.after.tax_AIC<-ur.df(ln_PPR.after.tax, 
                                           type = "trend", 
                                           lags = 15, 
                                           selectlags = "AIC")
summary(ADF_test_trend_ln_PPR.after.tax_AIC)

# Phillips-Perron (PP) test #
pp.test(LogGDP,alternative="stationary",type="Z(t_alpha)")
pp.test(ln_AHW_M,alternative="stationary",type="Z(t_alpha)")
pp.test(ln_G_W_Gap,alternative="stationary",type="Z(t_alpha)")
pp.test(ln_E_F,alternative="stationary",type="Z(t_alpha)")
pp.test(ln_WBR_M,alternative="stationary",type="Z(t_alpha)")
pp.test(ln_PPR.after.tax,alternative="stationary",type="Z(t_alpha)")

## Cointegration tests ##

#Variables binded to test
dset <- cbind(ln_GDP, ln_AHW_M, ln_G_W_Gap, ln_E_F, ln_WBR_M, ln_PPR.after.tax)

#Lag Selection
lagselect <- VARselect(dset, lag.max = 15, type = "const")
lagselect$selection

# Johansen Testing (Trace) #
Jo_test_tr <- ca.jo(dset, type = "trace", ecdet = "const", K = 4)
summary(Jo_test_tr)

# Johansen Testing (MaxEigen) #
Jo_test_ei <- ca.jo(dset, type = "eigen", ecdet = "const", K = 4)
summary(Jo_test_ei)

# Engle-Granger test #

#regression between the variables of interest and store residuals
Step_1_reg<-lm (ln_GDP ~ ln_G_W_Gap)
summary(Step_1_reg)
resid_G_W_Gap_reg<-ts(residuals(Step_1_reg), start = 1970, frequency = 1)
plot(Step_1_reg$residuals)
plot.ts(Step_1_reg$residuals)

#Test Stationarity of the residuals
ADF_test_resid_AIC<-ur.df(resid_G_W_Gap_reg, 
                          type ="none", 
                          lags = 12, 
                          selectlags = "AIC")
summary(ADF_test_resid_AIC)

## Regression in first differences model ##
GDP_Diff<-diff(ln_GDP,lag=1)
AHW_M_Diff<-diff(ln_AHW_M,lag=1)
G_W_Gap_Diff<-diff(ln_G_W_Gap,lag=1)
E_F_Diff<-diff(ln_E_F,lag=1)
WBR_M_Diff<-diff(ln_WBR_M,lag=1)
PPR_Diff<-diff(ln_PPR.after.tax,lag=1)

regression_fo_diff<-lm(GDP_Diff ~ AHW_M_Diff + G_W_Gap_Diff + E_F_Diff + WBR_M_Diff + PPR_Diff)
summary(regression_fo_diff)

# diagnostic checks
#Autocorrelation
dwtest(regression_fo_diff)
acf(residuals(regression_fo_diff), plot=T)

#Heteroskedasticity
bptest(regression_fo_diff)

#Coefficients adjustments 
#Heteroskedasticity adjustment
coeftest(regression_fo_diff, vcov. = vcovHC, type = "HC1")

#Autocorrelation adjustment
coeftest(regression_fo_diff, vcov = NeweyWest, prewhite = F, adjust = T)

## The ARDL model ##
df2<-cbind(GDP_Diff, AHW_M_Diff, G_W_Gap_Diff, E_F_Diff, WBR_M_Diff, PPR_Diff)
Regr_ARDL<-dynlm(L(GDP_Diff)~L(GDP_Diff)+L(AHW_M_Diff)+L(G_W_Gap_Diff)+L(E_F_Diff)+L(WBR_M_Diff)+L(PPR_Diff))
summary(Regr_ARDL)
head(fortify(Regr_ARDL))
residPlot<-ggplot(aes(x=.fitted, y=.resid), data=(Regr_ARDL))+geom_line()+geom_hline(yintercept=0)+labs(x="Fitted Values", y="Residual")
residPlot

##EXTRACTION OF RELEVANT TABLES##
SIMPLEMRM<- Model_1
ARDL<- Regr_ARDL
REGFIRSTDIF<-regression_fo_diff


stargazer(SIMPLEMRM, ARDL, 
          type = "html",
          out="Regression tables.htm")
stargazer(REGFIRSTDIF, 
          type = "html",
          out="Regression first differences.htm")



######################################################################################################################################################