# Clear Console and environment
rm(list=ls())
cat("\014")

#Scientific Notation
options("scipen"=99999, digits=3)

setwd("C:/Users/shane/OneDrive/UC Work files/Time series econometrics/Assignments/Homework 1")

#install.packages("dynlm")
library("quantmod")
library(dynlm)
library(vtable)
library(stargazer)

data <- read.csv("Merged dataset Q2.csv", header = TRUE)

# Transform the datafile into time series
dfts = ts(data,frequency=12,start=c(2000,1))
plot.ts(dfts[,2:4])

# Summary Table using vtable 
Sumstats <- sumtable(data)

# Run regressions
reg1 = lm(data$SANDP_houseprice~data$Avg_milk_price , data = dfts)
summary(reg1)

reg2 = lm(data$SANDP_houseprice~data$PPR_Shiprepair ,  data = dfts)
summary(reg2)

reg3 = lm(data$Avg_milk_price~data$PPR_Shiprepair ,  data = dfts)
summary(reg3)

reg4 = lm(data$Avg_milk_price~data$SANDP_houseprice + data$PPR_Shiprepair)
summary(reg4)

# Determine if stationary or not
par(mar = c(1, 1, 1, 1))
source(file="intord.R")
intord(data$SANDP_houseprice)
intord(data$Avg_milk_price)
intord(data$PPR_Shiprepair)


# Transform all other non stationary variables using  differencing
dAvg_milk_price <- diff(dfts[,2])

dSANDP_houseprice <- diff(dfts[,3])
ddSANDP_houseprice <- diff(dSANDP_houseprice) # Second-differenced

dPPR_Shiprepair <- diff(dfts[,4])


# Run regressions again after differencing
d_reg1 = dynlm(ddSANDP_houseprice~dAvg_milk_price )
summary(d_reg1)

d_reg2 = dynlm(ddSANDP_houseprice~dPPR_Shiprepair ,  data = dfts)
summary(d_reg2)

d_reg3 = dynlm(dAvg_milk_price~dPPR_Shiprepair ,  data = dfts)
summary(d_reg3)

d_reg4 = dynlm(dAvg_milk_price~ddSANDP_houseprice + dPPR_Shiprepair)
summary(d_reg4)


# Compare the regressions
stargazer(reg1,d_reg1,reg2,d_reg2,reg3,d_reg3,reg4,d_reg4,type="text",align=TRUE,
          keep.stat=c("n","rsq","f","aic","bic"), no.space=FALSE,df=FALSE, 
          title="Time Seris Comparisson" ,
          column.labels =c("Y on X","Diff Y on X","Y on Z", "Diff Y on Z", "X on Z", "Diff X on Z", "Y on X & Z", "Diff Y on X & Z"),
          report="vcsp",notes="Standard errors in parentheses",
          digit.separator = "",digits=3)


### Special case

d_reg_special = dynlm(dSANDP_houseprice~dAvg_milk_price  ,  data = dfts)
summary(d_reg_special)





