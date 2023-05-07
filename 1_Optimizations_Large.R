
#------------------------------------------------------------------------------#
# Loading the required packages
#------------------------------------------------------------------------------#
library(MASS)
library(rms)
library(pmsampsize)



#------------------------------------------------------------------------------#
# Source (loss) functions from 0_Functions_Thesis.R
#------------------------------------------------------------------------------#

source("./0_Functions_Thesis.R")

#############################################
#                Distributions              #
#############################################

# EXAMPLES
# 1 predictor for large sample size
n_p_l <- 1      #number of predictors
p_cov <- 0.2   #covariance of two correlated predictors
prop_corr <-1  #proportion of predictors which are correlated

p_mean_l <- rep(0, n_p_l)#,0)
p_sd_l <- rep(1, n_p_l)#,10)

### Zoe Dunias
cov_row <- rep(p_cov, times = round(length(p_mean_l)*prop_corr)^2) # Covariance of length n_predictors * n_predictors
p_sigma_l <- matrix(cov_row, ncol = round(length(p_mean_l)*prop_corr)) # Matrix of covariances of n_predictors by n_predictors  
p_sigma_l <- cbind(p_sigma_l, matrix(0, nrow= nrow(p_sigma_l), ncol = round(length(p_mean_l)*(1-prop_corr))))
p_sigma_l <- rbind(p_sigma_l, matrix(0, nrow= round(length(p_mean_l)*(1-prop_corr)), ncol = ncol(p_sigma_l)))

diag(p_sigma_l) <- p_sd_l^2  # Replace diagonal of matrix with 1's (variances)
p_sigma_l
### 


#############################################
#                   Targets                 #
#############################################
my_req_AUC <-  0.8
my_req_out_p_large <- 0.5


#############################################
#               SAMPLE SIZE                 #
#############################################
n_large <- 100000


#############################################
#             Scenarios AUC                 #
#############################################

# Proportion treated 
deri_prop_l <- c(0.1, 0.5, 0.9)
# Treatment effect size
deri_ES <-  c(-0.1989, -0.5008, -0.8020)

scen_AUC_l <- expand.grid(d_p = deri_prop_l, d_ES = deri_ES)
scen_AUC_l$S <- 1:nrow(scen_AUC_l)


#------------------------------------------------------------------------------#
# Tuning intercept LARGE scenario
#------------------------------------------------------------------------------#
#intercept for the relationship between predictors and treatment 
# all effect sizes of the predictors are constraint to be the same 

#Using large sample size for optimization
n_large <- 10000

#Mean absolute prediction error
exp(-0.508 - 0.544 * log(100000) + 0.259 * log(0.5) + 0.504 * log(1))


set.seed(1705)
res_l_int <- matrix(0, nrow = 1000, ncol = length(deri_prop_l))
res_l_prop <- matrix(0, nrow = 1000, ncol = length(deri_prop_l))

#trying to get more stable results for optimizing the intercept
## performing optimization 1000 times, using a large sample size of  10000
## taking the mean of the resulting proportion to check if the mean is close
##   to the target proportion
## saving the mean of the intercept as the result


for(i in 1:1000){
  print(i)
  res_l_intercept_opt <- intercept_optim(d_p = deri_prop_l, 
                                         pred_mean = p_mean_l, 
                                         pred_sigma = p_sigma_l,
                                         n = n_large)
  res_l_int[i,] <- res_l_intercept_opt[[1]]
  res_l_prop[i,] <- res_l_intercept_opt[[2]]
}


res_l_int_mean <- apply(res_l_int, 2, mean)
res_l_prop_mean <- apply(res_l_prop, 2, mean)
#0.0987198 0.4999663 0.9014344

res_l_prop_sd <- apply(res_l_prop, 2, sd)
# 0.004846712 0.006152724 0.004943654



# I considered taking the one whose proportion is closest to target
 res_l_prop_diff <- matrix(nrow= dim(res_l_prop)[1], ncol= dim(res_l_prop)[2])
 for(j in 1:length(deri_prop_l)){
   res_l_prop_diff[,j] <- res_l_prop[,j] - deri_prop_l[j]
 }
 index <- cbind(apply(abs(res_l_prop_diff), 2, which.min), 1:6)
 
 
 res_l_prop_opt <- numeric(length(deri_prop_l))
 for(k in 1:length(deri_prop_l)){
   res_l_prop_opt[k] <-(res_l_prop[index[k,1], index[k,2]])
 }
  res_l_int_opt <- numeric(length(deri_prop_l))
  for(k in 1:length(deri_prop_l)){
    res_l_int_opt[k] <- (res_l_int[index[k,1], index[k,2]])
  }
  
  res_l_prop_opt
  res_l_int_opt





# CHECKING RESULTS

#checking which approach gives more stable and accurate results 
set.seed(2345)
check_l_prop_opt <- matrix(nrow=  1000, ncol = length(res_l_int_opt))
for(k in 1:1000){
  for(j in 1:length(res_l_int_opt)){
    p <- rnorm(n_large)
    treat <-  rbinom(n_large, 1, plogis(res_l_int_opt[j] + p))
    check_l_prop_opt[k,j] <- (table(treat)[2]/n_large)
  }
  
}
apply(check_l_prop_opt, 2, mean)
# 0.1007896 0.4988389 0.8986431
apply(check_l_prop_opt, 2, sd)
# 0.003133485 0.004864848 0.002974158




set.seed(2345)
check_l_prop_mean <- matrix(nrow=  1000, ncol = length(res_l_int_mean))
for(k in 1:1000){
  for(j in 1:length(res_l_int_mean)){
    p <- mvrnorm(n_large, mu = p_mean_l, Sigma = p_sigma_l)
    treat <-  rbinom(n_large, 1, plogis(res_l_int_mean[j] + p))
    check_l_prop_mean[k,j] <- (table(treat)[2]/n_large)
  }
  
}

apply(check_l_prop_mean, 2, mean)
# 0.0984210 0.4998593 0.9013497
apply(check_l_prop_mean, 2, sd)
# 0.003086818 0.004877286 0.002938918

#Identifying for which cases the mean and for which the optimal appraoch 
# gives best results.

best_int_res_l <- apply(abs(rbind((apply(check_l_prop_mean, 2, mean) - deri_prop_l) , (apply(check_l_prop_opt, 2, mean) - deri_prop_l))), 2, which.min)
# 0.0030694 0.0033076

#saving the vector of best intercepts
res_l_int_overall <- numeric(length(best_int_res_l))
for(i in 1:length(best_int_res_l)){
  res_l_int_overall[i] <- rbind(res_l_int_mean, res_l_int_opt)[best_int_res_l[i],i]
}
rbind(res_l_int_mean, res_l_int_opt)
res_l_int_overall

#saving optimized intercepts
save(res_l_int, res_l_prop, res_l_int_mean, res_l_int_opt, res_l_int_overall, file = "Results/res_intercept_large.RData")
res_l_int_overall

#load("Results/res_intercept_large.RData")





#------------------------------------------------------------------------------#
# Tuning AUC LARGE scenario
#------------------------------------------------------------------------------#
start_seed <- 1705
n_beta_rep <- 5 

#############################################
#             Data generation               #
#############################################

#constraint (same beta for all predictors, here only 1 predictor)
#using GRID SEARCH

#initial sequence for intercept and slope for first grid search
## same for all scenarios in AUC_grid 
## (choices motivated by results for Research Report):
#intercept = seq(0, 1, by = 0.1)
#slope = seq(1.4, 1.75, by = 0.05)

#Grid sequences for different scenarios:
###
#1#
###
int_list_l_1 <- list(seq(0, 1, by = 0.1), nrow(scen_AUC_l))
slope_seq_l_1 <- seq(1.4, 1.75, by = 0.05)
scen_int_l_1 <- rep(1, nrow(scen_AUC_l)) 
e_small_l_1 <- 0.0001
e_stop_l_1 <- 0.002

res_AUC_grid_l_1 <- AUC_grid(scenarios_AUC = scen_AUC_l, 
                             res_int = res_l_int_overall,
                             intercept_list = int_list_l_1, 
                             slope = slope_seq_l_1,
                             scen_int = scen_int_l_1, 
                             e_stop = e_stop_l_1, 
                             e_small = e_small_l_1,
                             req_AUC = 0.8,
                             req_out_p = 0.5,
                             n_pred = n_p_l,
                             pred_mean = p_mean_l,
                             pred_sigma = p_sigma_l,
                             d_prop = deri_prop_l)
res_AUC_grid_l_1 
save(int_list_l_1, slope_seq_l_1, scen_int_l_1, res_AUC_grid_l_1, e_small_l_1, e_stop_l_1,
     file = "./Results/res_AUC_grid_l_1.RData")



###
#2#
###
int_list_l_2 <- list(seq(0, 0.5, by = 0.05), seq(0.5, 1, by= 0.05))
slope_seq_l_2 <- seq(1.4, 1.7, by = 0.01)
scen_int_l_2 <- c(1,1,1,1,1,2,1,2,2)
e_small_l_2 <- 0.00001
e_stop_l_2 <- 0.0001

res_AUC_grid_l_2 <- AUC_grid(scenarios_AUC = scen_AUC_l, 
                             res_int = res_l_int_overall,
                             intercept_list = int_list_l_2, 
                             slope = slope_seq_l_2,
                             scen_int = scen_int_l_2, 
                             e_stop = e_stop_l_2, 
                             e_small = e_small_l_2,
                             req_AUC = 0.8,
                             req_out_p = 0.5,
                             n_pred = n_p_l,
                             pred_mean = p_mean_l,
                             pred_sigma = p_sigma_l,
                             d_prop = deri_prop_l)
res_AUC_grid_l_2 
save(int_list_l_2, slope_seq_l_2, scen_int_l_2, res_AUC_grid_l_2, e_small_l_2, e_stop_l_2,
     file = "./Results/res_AUC_grid_l_2.RData")


###
#3#
###
int_list_l_3 <- list(seq(0, 0.4, by = 0.02), seq(0.28, 0.8, by= 0.02))
slope_seq_l_3 <- seq(1.4, 1.8, by = 0.02)
scen_int_l_3 <- c(1,1,2,2)
e_small_l_3 <- 0.00001
e_stop_l_3 <- 0.0001

res_AUC_grid_l_3 <- AUC_grid(scenarios_AUC = scen_AUC_l[c(2,4,6,8),], 
                             res_int = res_l_int_overall,
                             intercept_list = int_list_l_3, 
                             slope = slope_seq_l_3,
                             scen_int = scen_int_l_3, 
                             e_stop = e_stop_l_3, 
                             e_small = e_small_l_3,
                             req_AUC = 0.8,
                             req_out_p = 0.5,
                             n_pred = n_p_l,
                             pred_mean = p_mean_l,
                             pred_sigma = p_sigma_l,
                             d_prop = deri_prop_l)
res_AUC_grid_l_3 
save(int_list_l_3, slope_seq_l_3, scen_int_l_3, res_AUC_grid_l_3, e_small_l_3, e_stop_l_3,
     file = "./Results/res_AUC_grid_l_3.RData")




#Saving the final results by filling in the four scenarios optimized separately 
# in the third step
res_AUC_large <- res_AUC_grid_l_2
for(i in 1:4){
  ind <- c(2,4,6,8)
  res_AUC_large[ind[i],] <- res_AUC_grid_l_3[i,]
}

beta_tuned_l <- res_AUC_large[,c("d_p", "d_ES", "beta.1", "beta.2")]

save(beta_tuned_l, res_AUC_large, file = "./Results/res_AUC_large.RData")
#Saving AUC results for all 9 derivation scenarios, 5 repetitions for each scenario




#############################################
#            Set up Scenarios               #
#############################################


#According to Pencina et al. (2018), we use odds ratios of 1.22, 1.65,
# and 2.23 for a small, moderate and large effect size. 
# As we have a negative effect we will use the reciprocal value of the 
# odds ratios. 
#1/1.22
#1/1.65
#1/2.23
#0.8197, 0.6061, 0.4484 as effect sizes (odds)

# Proportion treated 
deri_prop_l <- vali_prop_l <-  c(0.10, 0.5, 0.9)
# Treatment effect size
deri_ES <- vali_ES <- c(-0.1989, -0.5008, -0.8020)

# Creating a matrix for all possible scenarios
scenarios_l <- expand.grid(d_p = deri_prop_l, d_ES = deri_ES, v_p =vali_prop_l, 
                         v_ES = vali_ES)

scenarios_l$d_int <- rep(res_l_int_overall, nrow(scenarios_l)/3)
scenarios_l$v_int <- rep(res_l_int_overall, each = 9, times = 3)

N_large <- nrow(scenarios_l)


#Saving the sample sizes for the derivation and validation data sets,
# the number, mean vector, and correlation matrix of the predictors, as well as
# the scenarios used for optimization of the AUC, and the ones needed for the
# simulation study
save(n_large, p_mean_l, p_sigma_l, n_p_l, 
     scen_AUC_l, scenarios_l, N_large, file = "./Results/n_dist_scen_large.RData")
