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

mt <- plm(logCMRunicef ~ Polity_1 + logHIV_1 + logGDPcap_1 + logDen_1 + GDPgrowth_1, data=data2, index=c("id", "period"), model="within", effect = "twoways")
summary(mt)


## Simple regression estimate

m1 <- lm(logCMRunicef ~ Polity_1 + logHIV_1 + logGDPcap_1 + logDen_1 + GDPgrowth_1, data=data2)
m2 <- lm(logCMRunicef ~ logHIV_1 + logGDPcap_1 + logDen_1 + GDPgrowth_1 + logDEMYRS_1, data=data2)
m3 <- lm(logCMRunicef ~ logGDPcap_1 + logDen_1 + GDPgrowth_1 + logDEMYRS_1, data=data2)
m4 <- lm(logIMRunicef ~ Polity_1 + logHIV_1 + logGDPcap_1 + logDen_1 + GDPgrowth_1, data=data2)
m5 <- lm(logIMRunicef ~ logHIV_1 + logGDPcap_1 + logDen_1 + GDPgrowth_1 + logDEMYRS_1, data=data2)
m6 <- lm(logIMRunicef ~ logGDPcap_1 + logDen_1 + GDPgrowth_1 + logDEMYRS_1, data=data2)

texreg(list(m1, m2, m3, m4, m5, m6))

##  Estimate with matching

library(Matching)

# drop missing data
data3 <- na.omit(data2)

# create a dummy as the treatment based on Polity_1
data3$treatment1[data3$Polity_1<=0] <- 0
data3$treatment1[data3$Polity_1>0] <- 1

# calculate the propensity score
ps <- glm(treatment1 ~ logHIV_1 + logGDPcap_1 + logDen_1 + GDPgrowth_1, data=data3, family = binomial)

ps1 <- predict(ps, data3, type = "response")

ma1 <- Match(data3$logCMRunicef, data3$treatment1,ps1)
ma2 <- Match(data3$logIMRunicef, data3$treatment1, ps1)


# create a dummy as the treatment based on logDemYRS_1 and replicate Matching
data3$treatment2[data3$logDEMYRS_1<=0] <- 0
data3$treatment2[data3$logDEMYRS_1>0] <- 1

ps <- glm(treatment2 ~ logHIV_1 + logGDPcap_1 + logDen_1 + GDPgrowth_1, data=data3, family = binomial)

ps2 <- predict(ps, data3, type = "response")

ma3 <- Match(data3$logCMRunicef, data3$treatment2,ps2)
ma4 <- Match(data3$logIMRunicef, data3$treatment2,ps2)

matchtable <- matrix(c(ma1$est, ma2$est, ma3$est, ma4$est, ma1$se, ma2$se, ma3$se, ma4$se, ma1$wnobs, ma2$wnobs, ma3$wnobs, ma4$wnobs), ncol = 3)
colnames(matchtable) <- c("Estimate", "SE", "Num. obs.")
xtable(matchtable)



# check balance
hist(data3$treatment1)
hist(data3$treatment2)







