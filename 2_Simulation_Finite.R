

# SIMULATION FOR FINITE 
# using results from 1_Optimizations_Finite.R

 #------------------------------------------------------------------------------#
 # Loading packages
 #------------------------------------------------------------------------------#

library(purrr) #for rbernoulli
library(rms) #for performance measures
library(MASS) #for rmvnorm
library(simsalapar) #for tryCatch.W.E for warning and error catching


#------------------------------------------------------------------------------#
# Source required functions from 0_Functions_Thesis.R
#------------------------------------------------------------------------------#
options(scipen= 999) #no scientific notation

source("./0_Functions_Thesis.R")

#------------------------------------------------------------------------------#
# Load results from 1_Optimization_Finite.R
#------------------------------------------------------------------------------#
#Loading the results for the INTERCEPT required for the set of targeted treatment 
# proportions
load("./Results/res_intercept_finite.RData")

#Loading the results for the parameters required to obtain targeted values for
# the AUC and outcome proportion in the different settings
load("./Results/res_AUC_finite.RData")

#The relationship between the predictors and the outcome differs slightly due
# to different settings and same targeted AUC and outcome proportion 
range(res_AUC_finite$beta.2)
#0.296 0.324

#Loading the characteristics of the predictors 
# 8 predictors, standard normally distributed, all correlated with covariance = 0.2
# and the scenarios 
load("./Results/n_dist_scen_finite.RData")




#------------------------------------------------------------------------------#
# GLOBAL SET UP
#------------------------------------------------------------------------------#

# Number of Iterations
iter_finite <- 1000
 



#------------------------------------------------------------------------------#
# SIMULATION
#------------------------------------------------------------------------------#

 #############################################
 #          Set up Result matrix             #
 #############################################

#Running and saving results for the case where T is based on P

#Finite simulation results
set.seed(1706)
res_sim_finite <- my_sim(scen = scenarios,
                         n_deri = n_finite_deri, 
                         n_vali = n_finite_vali,
                         num_scen = 1:N_finite, 
                         beta_mat = beta_tuned,
                         treat_rel = "all", 
                         I = iter_finite,
                         pred_mean = p_mean,
                         pred_sigma = p_sigma,
                         n_pred = n_p)

save(res_sim_finite, file = "./Results/res_sim_finite.RData")



