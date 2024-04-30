# install.packages(c("ggplot2", "gridExtra"), dependencies = TRUE, repos = "https://cloud.r-project.org")

library(ggplot2)
library(gridExtra)
library(Hmisc)
library(MASS)

##### Read Data #####

data <- readxl::read_xlsx("/Users/julie/Downloads/Heineken.xlsx") # weekly data

##### Plot Data #####

#p1 <- ggplot(data, aes(x = Week, y = data$Awareness_Pct)) + geom_bar(stat = "identity") 
p1 <- ggplot(data, aes(x = Week, y = data$Awareness_Pct)) + geom_line() + geom_point(colour="blue", size = 2, shape = 21, fill="white") 
p2 <- ggplot(data, aes(x = Week, y = data$Volume_Sales_1000Liter)) + geom_line() + geom_point(colour="orange", size = 2, shape = 21, fill="white") 
p3 <- ggplot(data, aes(x = Week, y = data$ADV_OOH_Euros)) + geom_line() + geom_point(colour="red", size = 2, shape = 21, fill="white") 
p4 <- ggplot(data, aes(x = Week, y = data$ADV_SocialMedia_Euros)) + geom_line() + geom_point(colour="green", size = 2, shape = 21, fill="white") 


grid.arrange(p1,p2,p3,p4,  nrow = 3)  
theme_set(theme_bw())

## dependent variables
aware <- data$Awareness_Pct
sales <- data$Volume_Sales_1000Liter

## independent variable
OOH <- data$ADV_OOH_Euros
SocialMedia <- data$ADV_SocialMedia_Euros

##### Create Lag Variables #####
data$lag.Sales <- Lag(sales) 	
data$lag.Aware <- Lag(aware)


##### Mediator and DV Regressions #####
# Mediator regression (a-path)
m1 <- lm(aware ~ -1 + lag.Aware + OOH, data = data)

m2 <- lm(aware ~ -1 + lag.Aware + SocialMedia, data = data)

# DV regression (b-path and c-path)
m3 <- lm(sales ~ -1 + lag.Sales + OOH + aware + SocialMedia + data$temp_mean + data$covid_stringency_index , data = data)

## summary
summary(m1)
summary(m2)
summary(m3)


# a-path
a1.est <- m1$coeff[2] / (1 - m1$coeff[1])
a2.est <- m2$coeff[2] / (1 - m2$coeff[1])

# b-path
b.est <- m3$coeff[3] / (1 - m3$coeff[1])

# c-path
c1.est <- m3$coeff[2] / (1 - m3$coeff[1])
c2.est <- m3$coeff[4] / (1 - m3$coeff[1])

# Calculate the mediation effects
ab1 <- a1.est * b.est
ab2 <- a2.est * b.est

# Calculate the total effects
total1 <- c1.est + ab1
total2 <- c2.est + ab2

# empirical results
out.emp.res <- matrix(c(a1.est, a2.est, b.est, c1.est, c2.est, ab1, ab2, total1, total2),9,1)
colnames(out.emp.res) <- c("Estimates")
rownames(out.emp.res) <- c("OOH -> Aware", "SoicalMedia -> Aware", "Aware -> Sales","OOH -> Sales","SocialMedia -> Sales","OOH -> Aware -> Sales","SocialMedia -> Aware -> Sales", "Total1 (c + ab)", "Total2 (c + ab)")

print(round(out.emp.res, digits = 4))

####################################
# Monte Carlo Inference Starts Here
####################################

# create parameter vector and var-covar matrix of relevany coefficients from m1 and m2 models
m1.param <- matrix(coefficients(m1), nrow = 2, ncol = 1)
m1.vcov <- vcov(m1)

m2.param <- matrix(coefficients(m2), nrow = 2, ncol = 1)
m2.vcov <- vcov(m2)

m3.param <- matrix(coefficients(m3), nrow = 6, ncol = 1)
m3.vcov <- vcov(m3)

## Monte Carlo draws
n.MC <- 1000
draws.m1 <- mvrnorm(n.MC, m1.param, m1.vcov)
draws.m2 <- mvrnorm(n.MC, m2.param, m2.vcov)
draws.m3 <- mvrnorm(n.MC, m3.param, m3.vcov)

# a-path
a1.sim <- draws.m1[,2] / (1 - draws.m1[,1])
a2.sim <- draws.m2[,2] / (1 - draws.m1[,1])

# b-path
b.sim <- draws.m3[,3] / (1 - draws.m3[,1])

# c-path
c1.sim <- draws.m3[,2] / (1 - draws.m3[,1])
c2.sim <- draws.m3[,4] / (1 - draws.m3[,1])

# mediation effect a-path x b-path
ab1.sim <- a1.sim * b.sim 
ab2.sim <- a2.sim * b.sim 

# total effect = c + a*b
tot1.sim <- c1.sim + ab1.sim
tot2.sim <- c2.sim + ab2.sim

## Results: MC Estimates and CIs
a1.res <- quantile(a1.sim, probs = c(0.5, 0.025, 0.975))
a2.res <- quantile(a2.sim, probs = c(0.5, 0.025, 0.975))
b.res <- quantile(b.sim, probs = c(0.5, 0.025, 0.975))
c1.res <- quantile(c1.sim, probs = c(0.5, 0.025, 0.975))
c2.res <- quantile(c2.sim, probs = c(0.5, 0.025, 0.975))
ab1.res <- quantile(ab1.sim, probs = c(0.5, 0.025, 0.975))
ab2.res <- quantile(ab2.sim, probs = c(0.5, 0.025, 0.975))
total1.res <- quantile(tot1.sim, probs = c(0.5, 0.025, 0.975))
total2.res <- quantile(tot2.sim, probs = c(0.5, 0.025, 0.975))

out.mediation.res <- matrix(0,9,3)
out.mediation.res <- rbind(a1.res, a2.res, b.res,c1.res, c2.res, ab1.res, ab2.res, total1.res, total2.res)
colnames(out.mediation.res) <- c("Median", "2.5% CIs", "97.5% CIs")
rownames(out.mediation.res) <- c("OOH -> Aware", "Social -> Aware", "Aware -> Sales","OOH -> Sales","SocialMedia -> Sales", "OOH -> Aware -> Sales", "Social -> Aware -> Sales", "Total OOH (c + ab)", "Total Social (c + ab)")

print(round(out.mediation.res, digits = 4))

print(round(out.emp.res, digits = 4))



