
# SIMULATION FOR LARGE 
# using results from 1_Optimizations_Large.R


#------------------------------------------------------------------------------#
# Loading the required packages
#------------------------------------------------------------------------------#
library(purrr) #for rbernoulli
library(rms) #for performance measures
library(MASS) #for rmvnorm
library(simsalapar) #for tryCatch.W.E for warning and error catching


#------------------------------------------------------------------------------#
# Source requried functions from 0_Functions_Thesis.R
#------------------------------------------------------------------------------#

source("./0_Functions_Thesis.R")


#------------------------------------------------------------------------------#
# Load results from 1_Optimization_Large.R
#------------------------------------------------------------------------------#
#Loading the results for the INTERCEPT required for the set of targeted treatment 
# proportions
load("./Results/res_intercept_large.RData")

#Loading the results for the parameters required to obtain targeted values for
# the AUC and outcome proportion in the different settings
load("./Results/res_AUC_large.RData")


#The relationship between the predictors and the outcome differs slightly due
# to different settings and same targeted AUC and outcome proportion 
range(res_AUC_large$beta.2)
#1.446 1.608

#Loading the characteristics of the predictors 
# 1 predictor, standard normally distributed
# and the scenarios 
load("./Results/n_dist_scen_large.RData")



#############################################
#               SAMPLE SIZE                 #
#############################################
n_large <- 100000


#------------------------------------------------------------------------------#
# ITERATION SET UP 
#------------------------------------------------------------------------------#

iter_large <- 1

#############################################
#             Simulation study              #
#############################################


set.seed(1706)
res_sim_large <- my_sim(scen= scenarios_l,
                        n_deri = n_large, 
                        n_vali = n_large,
                        num_scen = 1:N_large, 
                        treat_rel = "all", 
                        beta_mat = beta_tuned_l,
                        I = iter_large,
                        pred_mean = p_mean_l,
                        pred_sigma = p_sigma_l,
                        n_pred = n_p_l)

save(res_sim_large, file = "./Results/res_sim_large.RData")

