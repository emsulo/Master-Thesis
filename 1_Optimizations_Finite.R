
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
# 8 predictors, standard normally distributed, all correlated with covariance = 0.2
n_p <- 8       #number of predictors
p_cov <- 0.2   #covariance of two correlated predictors
prop_corr <-1  #proportion of predictors which are correlated

p_mean <- rep(0, n_p)#,0)
p_sd <- rep(1, n_p)#,10)

### Zoe Dunias
cov_row <- rep(p_cov, times = round(length(p_mean)*prop_corr)^2) # Covariance of length n_predictors * n_predictors
p_sigma <- matrix(cov_row, ncol = round(length(p_mean)*prop_corr)) # Matrix of covariances of n_predictors by n_predictors  
p_sigma <- cbind(p_sigma, matrix(0, nrow= nrow(p_sigma), ncol = round(length(p_mean)*(1-prop_corr))))
p_sigma <- rbind(p_sigma, matrix(0, nrow= round(length(p_mean)*(1-prop_corr)), ncol = ncol(p_sigma)))

diag(p_sigma) <- p_sd^2  # Replace diagonal of matrix with 1's (variances)
p_sigma
### 

#############################################
#                   Targets                 #
#############################################
my_req_AUC <-  0.8
my_req_out_p <- 0.2

#############################################
#     Finite SAMPLE SIZE calculations       #
#############################################
#calculate required sample size and specify n accordingly
sampsize <- pmsampsize(
  type = "b",
  #binary outcome
  cstatistic = my_req_AUC,
  #AUC of 0.8 across scenarios
  prevalence = my_req_out_p,
  #outcome rate of 0.2 for finite sample simulation
  parameters = n_p,
  seed = 1705
) #number of predictors, change to 1 if we only use P in the models
n_deri_min <- sampsize$sample_size
set.seed(NULL)

n_finite_deri <- n_deri_min * (1 / (1 - 0.9))

#############################################
#             Scenarios AUC                 #
#############################################

# Proportion treated 
deri_prop <- c(0.1,0.15, 0.5, 0.55, 0.9, 0.95)
# Treatment effect size
deri_ES <-  c(-0.1989, -0.5008, -0.8020)

scen_AUC <- expand.grid(d_p = deri_prop, d_ES = deri_ES)
scen_AUC$S <- 1:nrow(scen_AUC)

#YES, all of these are deviation proportions at some point! 0.15 to 0.1 etc. 

#------------------------------------------------------------------------------#
# Tuning intercept FINITE scenario
#------------------------------------------------------------------------------#
#intercept for the relationship between predictors and treatment 
# all effect sizes of the predictors are constraint to be the same 

#Using large sample size for optimization
n_large <- 10000

set.seed(1705)
res_int <- matrix(0, nrow = 1000, ncol = length(deri_prop))
res_prop <- matrix(0, nrow = 1000, ncol = length(deri_prop))

#Trying to get more stable results for optimizing the intercept
## performing optimization 1000 times, using a large sample size of 10000
## taking the mean of the resulting proportion to check if the mean is close
##   to the target proportion


for(i in 1:1000) {
  print(i)
  res_intercept_opt <- intercept_optim(
    d_p = deri_prop,
    pred_mean = p_mean,
    pred_sigma = p_sigma,
    n = n_large            #using a large sample size for optimizations
  )
  res_int[i, ] <- res_intercept_opt[[1]]
  res_prop[i, ] <- res_intercept_opt[[2]]
}
res_int_mean <- apply(res_int, 2, mean)
res_prop_mean <- apply(res_prop, 2, mean)
# 0.0987331 0.1459185 0.5001598 0.5464366 0.9014538 0.9513122
res_prop_sd <- apply(res_prop, 2, sd)
# 0.004969589 0.013173252 0.005975189 0.009520501 0.004656143 0.004266330


# I considered taking the one whose proportion is closest to target

res_prop_diff <- matrix(nrow= dim(res_prop)[1], ncol= dim(res_prop)[2])
for(j in 1:length(deri_prop)){
 res_prop_diff[,j] <- res_prop[,j] - deri_prop[j]
 }
index <- cbind(apply(abs(res_prop_diff), 2, which.min), 1:6)
 
 
res_prop_opt <- numeric(length(deri_prop))
 for(k in 1:length(deri_prop)){
   res_prop_opt[k] <-(res_prop[index[k,1], index[k,2]])
 }
  res_int_opt <- numeric(length(deri_prop))
  for(k in 1:length(deri_prop)){
  res_int_opt[k] <- (res_int[index[k,1], index[k,2]])
  }
  
  res_prop_opt
  # 0.10 0.15 0.50 0.55 0.90 0.95
  res_int_opt
  # -2.560624390 -2.075695399  0.004971576  0.270128065  2.550565791  3.361527133


#Checking which approach gives more stable and accurate results 
set.seed(2345)
check_prop_opt <- matrix(nrow=  1000, ncol = length(res_int_opt))
for(k in 1:1000){
  for(j in 1:length(res_int_opt)){
    p <- rnorm(n_large)
    treat <-  rbinom(n_large, 1, plogis(res_int_opt[j] + p))
    check_prop_opt[k,j] <- (table(treat)[2]/n_large)
  }
  
}
apply(check_prop_opt, 2, mean)
# 0.1002333 0.1469980 0.5009694 0.5556083 0.8988172 0.9495582
apply(check_prop_opt, 2, sd)
# 0.002998548 0.003509141 0.004924400 0.004913211 0.002944356 0.002219829



set.seed(2345)
check_prop_mean <- matrix(nrow=  1000, ncol = length(res_int_mean))
for(k in 1:1000){
  for(j in 1:length(res_int_mean)){
    p <- mvrnorm(n_large, mu = p_mean, Sigma = p_sigma)
    treat <-  rbinom(n_large, 1, plogis(res_int_mean[j] + p))
    check_prop_mean[k,j] <- (table(treat)[2]/n_large)
  }
  
}

apply(check_prop_mean, 2, mean)
# 0.0986098 0.1455067 0.4999995 0.5467964 0.9014922 0.9514659
apply(check_prop_mean, 2, sd)
# 0.002847939 0.003552901 0.004895424 0.004851879 0.002998431 0.002141914


apply(abs(rbind((apply(check_prop_mean, 2, mean) - deri_prop) , (apply(check_prop_opt, 2, mean) - deri_prop))), 1, sum)
# 0.0120457 0.0114376



##NOTE: There is almost no difference between choosing the mean or the optimal.

#Identifying for which cases the mean and for which the optimal appraoch 
# gives best results.

best_int_res <- apply(abs(rbind((apply(check_prop_mean, 2, mean) - deri_prop) , 
                                (apply(check_prop_opt, 2, mean) - deri_prop))), 2, which.min)

#saving the vector of best intercepts
res_int_overall <- numeric(length(best_int_res))
for(i in 1:length(best_int_res)){
  res_int_overall[i] <- rbind(res_int_mean, res_int_opt)[best_int_res[i],i]
}
rbind(res_int_mean, res_int_opt)
res_int_overall




#saving optimized intercepts
save(res_int_opt, res_prop_opt, res_int_mean, res_prop_mean, res_int_overall, file = "Results/res_intercept_finite.RData")
res_int_overall

#load("Results/res_intercept_finite.RData")



#------------------------------------------------------------------------------#
# Tuning AUC FINITE scenario
#------------------------------------------------------------------------------#
start_seed = 1705
n_beta_rep <- 5 

#############################################
#             Data generation               #
#############################################

#constraint (same beta for all predictors)
#using GRID SEARCH

#initial sequence for intercept and slope for first grid search
## same for all scenarios in AUC_grid:
#intercept = seq(-3, 1, by = 0.5)
#slope = seq(0.1, 0.5, by = 0.05)

#Grid sequences for different scenarios:
###
#1#
###
int_list_1 <- list(seq(-3, 1, by = 0.5), nrow(scen_AUC))
slope_seq_1 <- seq(0.1, 0.5, by = 0.05)
scen_int_1 <- rep(1, nrow(scen_AUC)) 
e_small_1 <- 0.013
e_stop_1 <- 0.014

res_AUC_grid_1 <- AUC_grid(scenarios_AUC = scen_AUC,
                           res_int = res_int_overall, 
                           intercept_list = int_list_1, 
                           slope = slope_seq_1,
                           scen_int = scen_int_1, 
                           e_stop = e_stop_1, 
                           e_small = e_small_1,
                           n_pred = n_p,
                           req_AUC = 0.8,
                           req_out_p = 0.2,
                           pred_mean = p_mean,
                           pred_sigma = p_sigma,
                           d_prop = deri_prop)
res_AUC_grid_1 
save(int_list_1, slope_seq_1, scen_int_1, res_AUC_grid_1, e_small_1, e_stop_1,
     file = "./Results/res_AUC_grid_1.RData")


###
#2#
###

int_list_2 <- list(seq(-2.2, -1.8, by = 0.05), seq(-1.8, -1.4, by= 0.05), 
                 seq(-1.4, -0.9, by = 0.05))
slope_seq_2 <- seq(0.28, 0.36, by = 0.01)
scen_int_2 <- c(1, 1, 1, 1, 2, 2, 1, 1, 2, 2, 3, 3, 1, 1, 3, 3, 3, 3)
e_small_2 <- 0.00001
e_stop_2 <- 0.0001

res_AUC_grid_2 <- AUC_grid(scenarios_AUC = scen_AUC, 
                           res_int = res_int_overall, 
                           intercept_list = int_list_2, 
                           slope = slope_seq_2,
                           scen_int = scen_int_2,  
                           e_stop = e_stop_2, 
                           e_small = e_small_2,
                           n_pred = n_p,
                           req_AUC = 0.8,
                           req_out_p = 0.2,
                           pred_mean = p_mean,
                           pred_sigma = p_sigma,
                           d_prop = deri_prop)

res_AUC_grid_2

save(int_list_2, slope_seq_2, scen_int_2, res_AUC_grid_2, e_small_2, e_stop_2,
     file = "./Results/res_AUC_grid_2.RData")

###
#3#
###

#Only optimizing scenarios for which the optimum in the second round was on 
# the edge of the grid
int_list_3 <- list(seq(-1.9, -1.6, by = 0.05))
slope_seq_3 <- seq(0.29, 0.34, by = 0.01)
scen_int_3 <- c(1,1,1,1)
e_small_3 <- 0.00004
e_stop_3 <- 0.0001
res_AUC_grid_3 <- AUC_grid(scenarios_AUC = scen_AUC[c(3,4,8,14),],
                           res_int = res_int_overall,
                           intercept_list = int_list_3, 
                           slope = slope_seq_3,
                           scen_int = scen_int_3, 
                           e_small = e_small_3, 
                           e_stop = e_stop_3,
                           n_pred = n_p,
                           req_AUC = 0.8,
                           req_out_p = 0.2,
                           pred_mean = p_mean,
                           pred_sigma = p_sigma,
                           d_prop = deri_prop)

res_AUC_grid_3
save(int_list_3, slope_seq_3, scen_int_3, res_AUC_grid_3, file = "./Results/res_AUC_grid_3.RData")



#Saving the final results by filling in the four scenarios optimized separately 
# in the third step
res_AUC_finite <- res_AUC_grid_2
for(i in 1:4){
  ind <- c(3,4,8,14)
  res_AUC_finite[ind[i],] <- res_AUC_grid_3[i,]
}

beta_tuned <- res_AUC_finite[,c("d_p", "d_ES", "beta.1", "beta.2")]

save(beta_tuned, res_AUC_finite, file = "./Results/res_AUC_finite.RData")
#Saving AUC results for all 18 derivation scenarios, 5 repetitions for each scenario



#############################################
#          Set up Scenarios                 #
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
deri_prop <-  vali_prop <-  c(0.1,0.15, 0.5, 0.55, 0.9, 0.95)
# Treatment effect size
deri_ES <- vali_ES <-  c(-0.1989, -0.5008, -0.8020)


# Creating a matrix for all possible scenarios
scenarios <- expand.grid(d_p = deri_prop, d_ES = deri_ES, v_p =vali_prop, 
                         v_ES = vali_ES)

#removing scenarios which are not of interest for the finite sample size
scen_rem <- sort(unique(c(which(abs(scenarios$d_ES - scenarios$v_ES) > 0.5), 
                          which(abs(scenarios$d_p - scenarios$v_p) > 0.4), 
                          which(scenarios$d_p == 0.15 & scenarios$v_p != 0.1),
                          which(scenarios$d_p == 0.55 & scenarios$v_p != 0.5),
                          which(scenarios$d_p == 0.95 & scenarios$v_p != 0.9),
                          which(scenarios$v_p == 0.15 & scenarios$d_p != 0.1),
                          which(scenarios$v_p == 0.55 & scenarios$d_p != 0.5),
                          which(scenarios$v_p == 0.95 & scenarios$d_p != 0.9))))
scenarios <- scenarios[-scen_rem,]
for(i in 1:nrow(scenarios)){
  scenarios$d_int[i] <- res_int_mean[which(deri_prop == scenarios$d_p[i])]
  scenarios$v_int[i] <- res_int_mean[which(deri_prop == scenarios$v_p[i])]
}

N_finite <- nrow(scenarios)

#############################################
#      Required external sample size        #
#############################################

#Checking whether assuming a normal distribution for the linear predictor
# is justified 
lin_pred_hist <-  function(scen = scenarios, 
                           n = n_finite_deri, 
                           treat_rel = "all",
                           beta_mat =beta_tuned){
  for(S in 1:nrow(scen)){
    #choosing beta for the respective scenario
    
    d_p_int <- which(scen$d_p[S] == beta_mat$d_p)
    beta_int <- d_p_int[which(scen$d_ES[S] == beta_mat$d_ES[d_p_int])]
    
    beta <- beta_mat[beta_int,3:4]
    
    
    #continuous predictors
    deri <- as.data.frame(mvrnorm(n, p_mean, p_sigma))
  
  #TREATMENT
  #not based on any predictor
  if(treat_rel == "no"){
    deri$treat <- rbinom(n,1, scen$d_p[S])
  }
  
  #based on all predictors
  if(treat_rel == "all"){
    deri$treat <- rbinom(n, 1, plogis(scen$d_int[S] + as.matrix(deri)))                         
  }
  
  #OUTCOME
  #considering treatment, L, and P
  deri$prob <- plogis(as.numeric(as.numeric(beta[1]) + as.numeric(beta[2]) * rowSums(as.matrix(deri[,1:n_p])) + scen$d_ES[S] * deri$treat))
  
  linear_pred <- as.numeric(as.numeric(beta[1]) + as.numeric(beta[2]) * rowSums(as.matrix(deri[,1:n_p])) + scen$d_ES[S] * deri$treat)
  hist(linear_pred, freq = FALSE, breaks = 100)
  
  }
}

lin_pred_hist()
#The assumption of normal distribution for the linear predictors is justified.
# Some variation for the different scenarios.


#Plotting the distribution of the predicted risk to eye-ball the underlying
# distribution of the linear predictor using Figure 1 in Riley and collegues (2021)
pred_risk_hist <-  function(scen = scenarios, 
                           n = n_finite_deri, 
                           treat_rel = "all",
                           beta_mat =beta_tuned){
  for(S in 1:nrow(scen)){
    #choosing beta for the respective scenario
    
    d_p_int <- which(scen$d_p[S] == beta_mat$d_p)
    beta_int <- d_p_int[which(scen$d_ES[S] == beta_mat$d_ES[d_p_int])]
    
    beta <- beta_mat[beta_int,3:4]
    
    
    #continuous predictors
    deri <- as.data.frame(mvrnorm(n, p_mean, p_sigma))
    
    #TREATMENT
    #not based on any predictor
    if(treat_rel == "no"){
      deri$treat <- rbinom(n,1, scen$d_p[S])
    }
    
    #based on all predictors
    if(treat_rel == "all"){
      deri$treat <- rbinom(n, 1, plogis(scen$d_int[S] + as.matrix(deri)))                         
    }
    
    #OUTCOME
    #considering treatment, L, and P
    deri$prob <- plogis(as.numeric(as.numeric(beta[1]) + as.numeric(beta[2]) * rowSums(as.matrix(deri[,1:n_p])) + scen$d_ES[S] * deri$treat))
    
    print(densityplot(deri$prob, type = "density"))
    
  }
}

pred_risk_hist()


#Using eye-balling and the overview provided in FIGURE 1 in Riley and colleagues 
# (2021), we conclude that the linear predictors follow a N(-1.39, 1) distribution.



#Computing the required sample size for the external validation data set 
# according to Riley and collegues (2021) BOX 1 for the calibration slope

#We only calculate it for the calibration slope as it tends to give the highest 
# required sample size. 
ext_samp_size <- function(lp_mean = -1.39, #Riley (2021), Box 1, PART B
                          lp_sd = 1,
                          se_slope = 0.05,
                          n = n_large){ 
  alpha_samp_size <- 0 #Riley (2021), Box 1, PART A
  beta_samp_size <- 1
  lp <- rnorm(n, mean = lp_mean, sd = lp_sd) #Riley (2021), Box 1, PART C
  
  #Riley (2021), Box 1, PART D
  a <- exp(alpha_samp_size + beta_samp_size * lp)/((1 +exp(alpha_samp_size + beta_samp_size * lp))^2)
  b <- (lp * exp(alpha_samp_size + beta_samp_size * lp))/((1 + exp(alpha_samp_size + beta_samp_size * lp))^2)
  c <- (lp^2 * exp(alpha_samp_size + beta_samp_size * lp))/((1 + exp(alpha_samp_size + beta_samp_size * lp))^2)
  
  #Riley (2021), Box 1, PART E
  abc_mean <- apply(cbind(a, b, c), 2, mean)
  names(abc_mean) <- c("I_alpha", "I_alpha,beta", "I_beta")
  
  samp_size <- abc_mean["I_alpha"]/(se_slope^2 * ((abc_mean["I_alpha"] * abc_mean["I_beta"]) - (abc_mean["I_alpha,beta"]^2)))
  
  return(samp_size)
}

set.seed(1706)
res_ext_samp_size <- ext_samp_size() #0.2 is outcome proportion
res_ext_samp_size
# 3526.016 

#Number of events
res_ext_samp_size * 0.2

#Rounding up to the next heighest natural number
n_finite_vali <- ceiling(res_ext_samp_size)

#Saving the sample sizes for the derivation and validation data sets,
# the number, mean vector, and correlation matrix of the predictors, as well as
# the scenarios used for optimization of the AUC, and the ones needed for the 
# simulation study
save(n_finite_deri, n_finite_vali, p_mean, p_sigma, n_p, scen_AUC,
     scenarios, N_finite, file = "./Results/n_dist_scen_finite.RData")



