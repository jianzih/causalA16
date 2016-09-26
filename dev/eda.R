# Explorative data analysis

library(causalA16)
library(data.table)
library(ggplot2)
library(texreg)
library(plm)

data1 <- load_dataset("Ross_full")
data2 <- load_dataset("Ross_5_year")

# selective decriptive statistics
attach(data2)
x <- data.frame(logCMRunicef,logIMRunicef,Polity_1,logDEMYRS_1,logGDPcap_1,logDen_1,GDPgrowth_1,logHIV_1)
desc.1 <- describe(x)

library(xtable)
xtable(desc.1,skew=F)

m5 <- plm(logCMRunicef ~ Polity_1 + logHIV_1 + logGDPcap_1 + logDen_1 + GDPgrowth_1, data=data2, index=c("id", "period"), model="within", effect = "twoways")
summary(m5)
