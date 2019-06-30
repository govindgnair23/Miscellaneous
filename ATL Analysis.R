setwd('K:/CLE05/AML_MUSIGMA/RM&A/ATL Analysis Enhancement')
source('ATL Analysis Utils.R')
source("DBDA2E-utilities.R")

library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)

####Function to Generate Model String with specified priors

CreateModelString <- function(beta_params,gamma_params){
  
  modelString = paste('model{',
                      "for (s in 1:Nsubj){",
                      "z[s] ~ dbin(theta[s],N[s])",
                      "theta[s] ~ dbeta(omega*(kappa-2)+1 , (1-omega)*(kappa-2)+1)",
                      "}",
                      
                      paste0("omega ~ dbeta(",beta_params$a,",",beta_params$b,")"),
                      "kappa <- kappaMinusTwo + 2",
                      paste0("kappaMinusTwo ~ dgamma(",round(gamma_params$shape,3),",",round(gamma_params$rate,3),")"),
                      '}',sep="\n")
  
  return(modelString)
  
}
##################################################################



#####################################################################


.pardefault <- par(no.readonly = T) ##Get default graphical parameters
###############Data should have be in the format specified in the file####
#s -> Indicates Band
#z -> No of effective alerts in a band
#N -> No of alerts in a band
myData = read.csv("Alert_Data.csv")
combinations <- unique(myData[,c(1:3)])

# Select Desired combination
#combn <- combinations[1,] #10 bands
combn <- combinations[1,] #10 bands
#combn <- combinations[8,] #19 bands

#Observed Data
mydata <- myData[myData$Scenario==combn$Scenario&myData$Segement==combn$Segement&
                        myData$Parameter==combn$Parameter,]

##critical value to be compared against - Tolerance of 2%
compVal=0.02; 

##Region of practival equivalence
rope=c(0.0175,0.02) 

#################Choose priors##################
# Strong prior - high effectiveness
# beta_params <- betaABfromModeKappa(0.02,500)
# gamma_params <- gammaShRaFromModeSD(500,50)

#Strong prior - low effectiveness
beta_params <- betaABfromModeKappa(0.005,500)
gamma_params <- gammaShRaFromModeSD(500,50)


#Generate MCMC samples
codaSamples <- genMCMC2(data = mydata , sName="s" , zName="z" ,  NName = "N", beta_params,gamma_params,
                                  numSavedSteps=50000 , saveName=NULL , thinSteps=1)


#MCMC Diagnostics
diagMCMC(codaSamples,varnames(codaSamples)[1])
diagMCMC(codaSamples,varnames(codaSamples)[2])
diagMCMC(codaSamples,varnames(codaSamples)[3])

######Summary of posterior distribution########
summaryInfo = smryMCMC2( codaSamples , compVal=compVal , rope=rope ,
                        compValDiff=0.0 )

####### Plot Posterior distribution #########

plotMCMC2(codaSamples , data = mydata , sName="s" , zName="z" ,  NName = "N" , 
                     compVal=0.02 , rope=c(0.0175,0.02) , thetaParams = c(1) ) 
  
  


#################Analyze trends of effectiveness################


##Prepare data for regression###
data_for_reg <- prep_data(codaSamples,no_of_bands=dim(mydata)[1],sample_size=1000)

###Analyze trends using frequentist linear regression###

get_freq_trend(data_for_reg)


#########Analyze trends using Bayesian regression #########
###The part below is slow and is probably not necessary, simple regression above --
###should suffice##
library(rstanarm)
library(rstan)



####Fit bayesian linear and quadratic fits#####
fitted_models <- fit_bayes_models(data_for_reg,n_chains = 3 ,n_iter = 4000,n_warmup=500)
######Analyze fitted models#######
analyze_bayes_models(fitted_models[[1]],fitted_models[[2]],data_for_plot= data_for_reg,n_bands =dim(mydata)[1])
