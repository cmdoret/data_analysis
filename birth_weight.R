# First report for advanced data analysis. Birth weight dataset.
# Cyril Matthey-Doret
# Thu Nov  3 10:34:26 2016 ------------------------------

# Loading data:
setwd ("/home/cyril/Documents/Master/sem_1/Data_analysis/data/")
birth_weight<-read.table("birtweigth_study.txt", sep= " ")
row.names(birth_weight) <- NULL

#=======================================================
# WARNING: 
# LWT == POUNDS
# BWT == GRAMS
# Tranforming units
birth_weight$lwt <- birth_weight$lwt*0.45359237
birth_weight$bwt <- birth_weight$bwt/1000

#=======================================================
# Checking data:

shapiro.test(birth_weight$bwt)
qqnorm(birth_weight$bwt)
qqline(birth_weight$bwt)


#=======================================================

# Analysing which variable impacts birthweight

anova(lm(bwt ~ age+lwt+ht+smoke+ptd,birth_weight))
anova(lm(bwt ~ ht*smoke*lwt*ptd*age,birth_weight))

