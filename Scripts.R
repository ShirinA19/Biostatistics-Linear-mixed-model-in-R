##Simulation of mixed model

##Libraries
library(lme4)
install.packages("merTools")
library(merTools)

install.packages("ggeffects")

library(ggeffects)

library(lme4)
library(nlme)
library(dplyr)
library(car)
library(ggplot2)
library(ggpubr)
library(tidyverse)

set.seed(1234)  
Ngroups = 100
NperGroup = 3
N = Ngroups * NperGroup
groups = factor(rep(1:Ngroups, each = NperGroup))
u = rnorm(Ngroups, sd = .5)
e = rnorm(N, sd = .25)
x = rnorm(N)
y = 2 + .5 * x + u[groups] + e

d = data.frame(x, y, groups)

model = lmer(y ~ x + (1|groups), data=d)

ggplot(aes(x,y), data=d)+geom_point()

pred.mm.sim <- ggpredict(model, terms = c("x"))


(ggplot(pred.mm.sim) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = d,                      # adding the raw data (scaled values)
               aes(x = x, y = y, colour = y)) + 
    labs(x = "x", y = "y", 
         title = "predictions") + 
    theme_minimal()
)



##Malarai data
malaria.df<-read.csv("data file_cross sectional time points.csv", header=TRUE,sep = ";", quote = "\"'", dec = ".")

summary(malaria.df)

TLKb_mixed<-lmer(TLkb ~ age +symp + (1 | studyno), data = malaria.df)

summary(TLKb_mixed)

## To get confidence interval
confint(TLKb_mixed)

##estimation of the population effect for first five group
ranef(TLKb_mixed)$studyno %>% head(5)

coef(TLKb_mixed)$studyno %>% head(5)

install.packages("merTools")
library(merTools)
## for various model predictions
predictInterval(TLKb_mixed)

# mean, median and sd of the random effect estimates
REsim(TLKb_mixed)

## plot the interval estimates
plotREsim(REsim(TLKb_mixed))

#Prediction
predict(TLKb_mixed, re.form=NA)

predict(TLKb_mixed)

##Some plotting
plot(TLKb_mixed)
qqnorm(resid(TLKb_mixed))
qqline(resid(TLKb_mixed))

#Plotting model predictions

install.packages("ggeffects")

library(ggeffects)
##Prediction for the term age
pred.mm <- ggpredict(TLKb_mixed, terms = c("age"))
##Prediction for the term symp
pred.mm2 <- ggpredict(TLKb_mixed, terms = c("symp"))



## code to Plot the predictions for the term age

(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = malaria.df,                      # adding the raw data (scaled values)
               aes(x = age, y = TLkb, colour = year)) + 
    labs(x = "symptoms", y = "TLKb", 
         title = "predictions") + 
    theme_minimal()
)


### Plot the predictions for the term symp

(ggplot(pred.mm2) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = malaria.df,                      # adding the raw data (scaled values)
               aes(x = symp, y = TLkb, colour = year)) + 
    labs(x = "symptoms", y = "TLKb", 
         title = "predictions") + 
    theme_minimal()
)
