#clear out old stuff
rm(list=ls())

#load libraries
library(arm)
library(effects)
library(ggplot2)
library(car)
library(multcomp)
library(ezknitr)
library(R2jags)

#' Read in data for barrier lab study
dat <- read.csv("data/lab_barrier_dat.csv")
names(dat) <- tolower(names(dat)) #make variable names lower case
dat$barrier <- as.character(dat$barrier) #this and next line set barrier as an ordered factor so they plot in the right order
dat$barrier <- factor(dat$barrier, levels=unique(dat$barrier))
dat$cross <- as.factor(dat$cross)
dat$instar <- as.factor(dat$instar)
head(dat) #check first few rows
str(dat) #check variable type for the different variables


#' For this experiment, I was testing the effectiveness of different physical barriers in limiting
#' the movement of gypsy moth larvae. I used 4th ,5th, and 6th instars to see if there was an effect of
#' instar. The thought here being that each successive instar is larger and may be more able to 
#' over come certain barriers. The point of looking for barriers is to keep the larvae from entering
#' an area we don't want them to and pupating, emerging, mating, and laying eggs.
#' The barrier variable labels the different barriers tested, instar is what instar larvae was used
#' and cross is whether or not they crossed the barrier (1=cross, 0 = didn't cross). I plan to fit
#' a logistic regression to this data to evaluated the effectiveness of the different barriers
#' If the regression does not show a significant term for instar*barrier interaction, instar will be dropped

#' Plot data
ggplot(dat, aes(x=barrier, y=cross, colour=instar)) + theme_bw() + 
  geom_point(position = position_jitter(w=0.1, h=0.05), size=3) + ylab("Cross (Y/N)") + 
  scale_y_discrete(breaks=c(0,1), labels=c("No", "Yes")) + xlab("Barrier")

#' Logistic regression including instar
log_reg1 <- glm(cross ~ barrier * instar , data=dat, family="binomial")
summary(log_reg1)
op <- par()
par(mfrow=c(2,2))#Check residuals - not super useful with logistic regression, better to try and bin them
plot(log_reg1)
#binning values for residual checkin
x <- predict(log_reg1)
y <- resid(log_reg1)
par(op)
binnedplot(x,y)

#' Model including only barrier as a factor
log_reg2 <- glm(cross ~ barrier, data=dat, family="binomial")
summary(log_reg2) #check results
Anova(log_reg2)
log_reg2.5 <- glm(cross ~ barrier - 1, data=dat, family="binomial")
summary(log_reg2.5)

par(mfrow=c(2,2)) #set up image display to show 4 plots at once
plot(log_reg2) #Check residuals - not super useful with logistic regression, better to try and bin them
#binning values for residual checkin
x <- predict(log_reg2)
y <- resid(log_reg2)
par(op)
binnedplot(x,y)

effect("barrier", log_reg2)

#' Post Hoc comparisons?
posthoc <- glht(log_reg2, linfct = mcp(barrier = "Tukey"))
summary(posthoc)


#' Bayesian Analysis (not working)
#' Need to make a bunch of dummy variables
dummy<-model.matrix(log_reg2.5)[,1:6]
dat_jags <- cbind(dat, dummy)
jags.log <- function(){
  #likelihood
  for(i in 1:nobs){
    logit(p[i]) <- b0*barrierControl[i] + b1*barrierAsh[i] + b2*barrierDE[i] + b3*barrierTF[i] + 
      b4*barrierFluon[i] + b5*barrierMoat[i]
    y[i] ~ dbern(p[i])
  }
  
  #priors
  b0 ~ dnorm(0, 0.001)
  b1 ~ dnorm(0, 0.001)
  b2 ~ dnorm(0, 0.001)
  b3 ~ dnorm(0, 0.001)
  b4 ~ dnorm(0, 0.001)
  b5 ~ dnorm(0, 0.001)
}
#Gather Data

jags.dat <- list(nobs=nrow(dat_jags), barrierAsh=dat_jags$barrierAsh, barrierDE= dat_jags$barrierDE,
                 barrierTF = dat_jags$barrierTF, barrierFluon=dat_jags$barrierFluon,
                 barrierMoat=dat_jags$barrierMoat, y=as.numeric(dat_jags$cross), barrierControl=dat_jags$barrierControl)
params <- c("b0", "b1", "b2", "b3", "b4", "b5")
nc <- 3
nb <- 5000
ni <- 10000
nt <- 2
inits <- function(){
  list(b0 = 1,
       b1 = 1,
       b2 = 1,
       b3 = 1,
       b4 = 1,
       b5 = 1)
}
bayes.lr <- jags(jags.dat, parameters.to.save = params, model=jags.log, n.chains = nc, n.burnin = nb,
                 n.iter = ni, progress.bar="gui", inits=inits, n.thin = nt)

#'  Spun with:
#'  ezspin("homework/HW01_Template.R", out_dir = "output/HW01", fig_dir = "figures")