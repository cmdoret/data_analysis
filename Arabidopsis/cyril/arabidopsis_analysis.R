# In this script I try different ways to analyse the Arabidopsis dataset for assignment 2
# Cyril Matthey-Doret
# Thu Nov 24 11:01:16 2016 ------------------------------

#setwd("/home/cyril/Documents/Master/sem_1/Data_analysis/reports/Arabidopsis/")
setwd("/home/cyril/Documents/Master/sem_1/Data_analysis/Arabidopsis/")

library(lme4);library(car);library(MASS);library(fits)
ara <- read.csv(file = "arabidopsis.csv")
ara$gen <- as.factor(ara$gen)
ara$rack <- as.factor(ara$rack)
ara$nutrient <- as.factor(ara$nutrient)
poisson <- fitdistr(ara$total.fruits, "Poisson")
qqp(main="Poisson",ara$total.fruits, "pois", poisson$estimate)
qqp(main="Lognormal", ara$total.fruits,"lnorm")
gamma <- fitdistr(x = ara$total.fruits+1,"gamma")
qqp(ara$total.fruits,"gamma",shape=0.5)
qqp(ara$total.fruits+1,"gamma",shape=gamma$estimate[[1]],rate=gamma$estimate[[2]])

#=========================================================0

mod<-glmer(data=ara,(total.fruits+1) ~ 1+status+amd+nutrient+(1|rack)+(1|gen)+(1|popu),family=Gamma)

mod<-glmer(data=ara,(total.fruits+1) ~ status*amd*nutrient+(1|reg/popu/gen)+(1|reg)+(1|reg/popu),family=Gamma)

#glmmPQL(data=ara,(total.fruits+1) ~ status+amd+nutrient, ~1|rack+1|gen+1|popu, family = "Gamma")

overdisp_fun <- function(model) {
  ## number of variance parameters in an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1)/2
  }
  # The next two lines calculate the residual degrees of freedom
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf <- nrow(model.frame(model)) - model.df
  # extracts the Pearson residuals
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  # Generates a p-value. If less than 0.05, the data are overdispersed.
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

overdisp_fun(mod)


rowSums(xtabs(~popu+gen, ara)) # gen niché dans popu niché dans reg

ara$pop_reg <- with(ara, reg:factor(popu))
ara$pop_reg_gen <- with(ara, reg:factor(popu):factor(gen))
