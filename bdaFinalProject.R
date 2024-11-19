# bdaFinalProject.R

# load/setup              ####
rm(list = ls())
data <- read.csv("data/fieldData2024ForBdaGenoSuspect.11192024.cvs")

# packages
library(rstan)
library(ggplot2)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# make some EDA plots     ####
ggplot(data) + geom_boxplot(aes(x=genoSuspect, y=FRN)) + ylab("Fruit Count")

ggplot(data) + geom_boxplot(aes(x=genoSuspect, y=H)) + ylab("plant height")
  # this is probably confounded alot by stage 
ggplot(data) + geom_boxplot(aes(x=Stg, y=H)) + ylab("plant height")
  # yup that is a big driver of that

ggplot(data) + geom_boxplot(aes(x=genoSuspect, y=W)) + ylab("plant width")
  # oh maybe this is not just a factor of stage 

hist(data[!data$FRN %in% 0, "FRN"])
  # ok so that is quite the tail we probably want to use a poisson???


# model plan              ####

# FRN ~ genoSuspect + (site + stage)
   # this notation is frequentist but my brain still works this way so pretend its not freq
  # site and stage pretend they are hirec 



