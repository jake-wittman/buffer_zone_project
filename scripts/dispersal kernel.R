#' ### Load libraries
library(fitdistrplus)

#' ### Import data
dat <- read.csv("data/summary_data.csv")
absence <- dat[dat$treat_id=="Absence",]
presence <- dat[dat$treat_id != "Absence",]

#' ### Plot empircal dispersal distribution
ggplot(dat, aes(x=displacement)) + stat_bin() + facet_grid(.~treat_id) +theme_bw() 


#' ### Use fitdistr to fit distribution
#' These distributions don't work with data that are 0s, so we remove them. We are really interested in calculating the probability
#' of moving a certain distance for insects that actually move anyway, so it seems okay.
#Fit a gamma distribution to the data. Probably don't need the "start" or "lower" arguments when you remove the 0s
fitgamma <-fitdist(absence$displacement[absence$displacement!=0], "gamma", start=list(shape=2, scale=1))
                  
plot(fitgamma) #produces 4 plots - empirical distribution with density function overlaid, QQplot showing fit in tails, PP plot showing
               #fit in center and a cumulitive density plot for both emprical and theoretical density
fitgamma #parameter estimates with SE

#Fit a Weibull distribution, comments largely same as for gamma
fitweibull <-fitdist(absence$displacement[absence$displacement !=0], "weibull", start=list(scale=15, shape=1))
plot(fitweibull)
fitweibull

#Fit a lognormal distribution, comments largely the same as for gamma
fitlognormal <- fitdist(absence$displacement[absence$displacement !=0], "lnorm")
plot(fitlognormal)
fitlognormal

#' We can produce these plots with all three theoretical distributions overlaid
{op <-par()
par(mfrow=c(2,2))
plot.legend <-c("Weibull", "Gamma", "Log Normal")
denscomp(list(fitweibull, fitgamma, fitlognormal), legendtext=plot.legend)
qqcomp(list(fitweibull, fitgamma, fitlognormal), legendtext=plot.legend)
cdfcomp(list(fitweibull, fitgamma, fitlognormal), legendtext=plot.legend)
ppcomp(list(fitweibull, fitgamma, fitlognormal), legendtext=plot.legend)
par(op)}

#' Let's look at the AIC for each of the three distribution
aic <- c(fitgamma$aic, fitweibull$aic, fitlognormal$aic)
aic
#' It appears that the gamma and weibull distributions are virtually identical in terms of AIC. The gamma distribution has
#' a higher peak and a slightly quicker decline, whereas the weibull has a slightly lower peak and declines slightly slower.
#' Both of these fit out in the tails much better than the log normal does.



bootgamma<-bootdist(fitgamma)
plot(bootgamma)
bootgamma$CI
