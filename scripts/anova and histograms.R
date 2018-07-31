#' This script is to compare distances moved between different treatments (absence vs presence, 
#' fed vs starved). There is also code for making histograms.

#'Library
library(car)
library(ggplot2)
#' Import data
dat2016 <-read.csv("data/summary_data_2016.csv", na="NA")
dat2017 <-read.csv("data/summary_data_2017.csv", na="NA")

#' ### Anova Comparisons
#' Total distance 2016
ggplot(data=dat2016, aes(total_path_distance)) + geom_histogram(fill="white", colour="black") + facet_grid(treat_id ~ .) +
  theme_bw() + theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


flm_2016_totdist <- glm((total_path_distance+1) ~ treat_id, data=dat2016, family=Gamma)
Anova(flm_2016_totdist)
summary(flm_2016_totdist)
plot(flm_2016_totdist)
#' There is a significant difference in total difference moved between the absence and presence of
#' host vegetation groups (F=73.361, df=1, 101, p=1.277e-13)

#' Displacement 2016
flm_2016_displacement <- lm(displacement ~ treat_id, data=dat2016)
Anova(flm_2016_displacement)
summary(flm_2016_totdist)
#' There is a signi