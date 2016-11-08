# Analyse these data in order to identify variables affecting the birth weight and their possible interactions.

setwd(dir="/Users/Baboo19/Documents/R/")

dataframe <- read.csv("birtweigth_study.csv", sep="", header=TRUE)
row.names(dataframe) = NULL # reset row names : 1 -> 189


# response variable : Mother's weight before pregnancy (lwt)
# explanatory variables : Age of the mother (age), Smoking habits (smoke), Known hypertension (ht), Newborn weight (bwt), Preterm birth (ptd)


# linear regression : quantitatives VS qualitatives variables
# ANOVA : quantitatives VS quantitatives variables
# ANCOVA : mix of regression and ANOVA



### lwt : Mother's weight before pregnancy in POUNDS (lb) --> change to KILOGRAMS
### bwt : Newborn weight in GRAMS --> change to KILOGRAMS
# Conversion factor lb <-> kg : 1 lb = 0.45359237 kg

dataframe$lwt <- dataframe$lwt*0.45359237 # kg
dataframe$bwt <- dataframe$bwt/1000 # kg



#########################################
############ DATA STRUCTURE #############
#########################################

qqnorm(dataframe$bwt); qqline(dataframe$bwt) # QQnorm --> visualize if the data of the response variable is normally distributed
shapiro.test(dataframe$bwt) # Shapiro test to confirm the normal distribution of the data
# pvalue = 0.4383 > 0.05 >> normal distribution of the response variable



#########################################
################ ANCOVA #################
#########################################

anova(lm(bwt ~ age+lwt+smoke+ht+ptd, dataframe)) # ANOVA without interaction between explanatory variables
anova(lm(bwt ~ ht*smoke*lwt*ptd*age, dataframe)) # ANOVA with interaction between explanatory variables




#########################################
################# PLOTS #################
#########################################

library(ggplot2)
library(gridExtra)


names_df <- c(age = "Age of the mother [year]", lwt = "Mother's weight before pregnancy [kg]", smoke = "Smoking habits", 
              ht = "Known hypertension", bwt = "Newborn weight [kg]", ptd = "Preterm birth") # variables' name and units



# plots_df is a function to plot every explanatory variables VS response variable (bwt)
# plotslist create a list containing all plots
# count is a counter for the list
# if the explanatory variable is numeric, make a scatterplot
# if the explanatory variable is a factor, make a boxplot
# aes_string prevents erasing values at last iteration

plots_df <- function(df){
  plotslist <- list()
  count <- 1
  for (c in colnames(df)){
    if (c!= "bwt"){
      if(is.numeric(df[1,c])){
        plotslist[[count]] <- ggplot(data=df) +
          geom_point(aes_string(x = df[,c], y = df$bwt)) +
          theme_bw() + xlab(names_df[c]) + ylab("Newborn weight") +
          ggtitle(paste("Newborn weight [kg] VS", names_df[c]))
    }else{
      plotslist[[count]] <- ggplot(data=df) +
        geom_boxplot(aes_string(x = df[,c], y = df$bwt)) +
        theme_bw() + xlab(names_df[c]) + ylab("Newborn weight") +
        ggtitle(paste("Newborn weight [kg] VS", names_df[c]))
     }
    count <- count + 1
    }
  }
  return(plotslist)
}

plots_df(dataframe)


# Newborn weight VS Age of the mother
summary(dataframe$age)
cor.test(dataframe$age, dataframe$bwt)
# The linear dependance of both variables aren't correlated (cor = 0.0899)


# Newborn weight VS Mother's weight before pregnancy
summary(dataframe$lwt)
cor.test(dataframe$lwt, dataframe$bwt)
# The linear dependance of both variables aren't correlated (cor = 0.1858)


# Newborn weight VS Smoking habits
summary(dataframe$smoke)
summary(dataframe$bwt[dataframe$smoke=="SMOKE"])
summary(dataframe$bwt[dataframe$smoke=="NOSMOKE"])
# This figure shows that "Smoker mothers" have heavier babies


# Newborn weight VS Known hypertension
summary(dataframe$ht)
summary(dataframe$bwt[dataframe$ht=="HT"])
summary(dataframe$bwt[dataframe$ht=="NOHT"])
# This figure shows that "Mothers with known hypertension" have heavier babies
# It isn't logical biological result

  
# Newborn weight VS Preterm birth
summary(dataframe$ptd)
summary(dataframe$bwt[dataframe$ptd=="PPREM"])
summary(dataframe$bwt[dataframe$ptd=="NOPPREM"])
# This figure shows that "Preterm born children" are heavier than "Not preterm born children"
# It isn't logical biological result