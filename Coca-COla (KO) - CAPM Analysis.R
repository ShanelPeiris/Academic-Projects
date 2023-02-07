# Clear Console and environment
rm(list=ls())
cat("\014")

#Scientific Notation
options("scipen"=99999, digits=3)

setwd("C:/Users/shane/OneDrive/UC Work files/Time series econometrics/Assignments/Homework 1")

#install.packages("dynlm")
library("quantmod")
library(dynlm)

data <- read.csv("APT2000.csv", header = TRUE)

# Transform the datafile into time series
dfts = ts(data,frequency=12,start=c(2000,9))
plot.ts(dfts[,2:10])

#Create inflation Rate
INF <- Delt(dfts[,4])*100
plot(INF)

#Estimate Regression of level model
reg = lm (KO ~ SANDP + INF + Industrialproduction + USTB3M + M1MONEYSUPPLY + CONSUMERCREDIT + BAAAAASPREAD , data = dfts)
summary(reg)

# Interpret the regression and give details
# We observe that SANDP is statistically significant and has a positive coefficient of 0.15 indicating
# We also note that Money supply is significant and positive indicating
# Consumer credit is statistically significant but the coefficient is negative
# Credit spread is also significant with a positive coefficient

# Determine the order of integration of each variable.
par(mar = c(1, 1, 1, 1))
source(file="intord.R")
intord(data$SANDP)
intord(data$CPI)
intord(data$Industrialproduction)
intord(data$USTB3M)
intord(data$M1MONEYSUPPLY)
intord(data$CONSUMERCREDIT)
intord(data$BAAAAASPREAD)
intord(data$KO) ### So we take Intord from the Original data and not the Time series?

# Create Excess Return for KO
RKO = Delt(dfts[,10]) # return of KO
RF = dfts[,6] # Risk Free rate
ERKO = RKO - RF # Excess return for KO

#Create Market Risk Premium
RSANDP = Delt(dfts[,3])
ERSANDP = RSANDP-RF
plot.ts(ERSANDP)

# Transform all other non stationary variables using first differencing
dKO <- diff(dfts[,10])
dSANDP <- diff(dfts[,3])
dCPI <- diff(dfts[,4])
dIndustrialproduction <- diff(dfts[,5])
dUSTB3M <- diff(dfts[,6])
dM1MONEYSUPPLY <- diff(dfts[,7])
dCONSUMERCREDIT <- diff(dfts[,8])
dBAAAAASPREAD <- diff(dfts[,9])

# Rerunning the Regression with First differenced variables
reg2 = dynlm (dKO ~  dSANDP + INF + dIndustrialproduction + dUSTB3M + dM1MONEYSUPPLY + dCONSUMERCREDIT + dBAAAAASPREAD)
summary(reg2)


#Estimate CAPM - one factor
res = lm(ERKO~ERSANDP)
summary(res)
