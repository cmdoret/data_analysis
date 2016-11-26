# In this script I try different ways to analyse the Arabidopsis dataset for assignment 2
# Cyril Matthey-Doret
# Thu Nov 24 11:01:16 2016 ------------------------------

setwd("/home/cyril/Documents/Master/sem_1/Data_analysis/reports/Arabidopsis/")
library(lme4);library(car);library(MASS);library(fits)
ara <- read.csv(file = "arabidopsis.csv")
poisson <- fitdistr(ara$total.fruits, "Poisson")
qqp(main="Poisson",ara$total.fruits, "pois", poisson$estimate)
qqp(main="Lognormal", ara$total.fruits,"lnorm")
