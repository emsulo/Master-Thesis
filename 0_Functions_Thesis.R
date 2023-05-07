#Functions for 1_Optimizations_Finite.R 

#------------------------------------------------------------------------------#
# Optimizing intercept to obtain treatment proportion
#------------------------------------------------------------------------------#

###########################################
##       loss function intercept         ##
###########################################

#The loss function to use when optimizing the intercept to obtain the targeted 
# proportion treated. 
#INPUT:  int - current value for the intercept 
#OUTPUT: diff - absolute difference of the proportion treated and the targeted
#               proportion treated

loss_function_intercept <- function(int) {
  treat <- rbinom(n_large, 1, plogis(int + p))
  
  prop <<- prop.table(table(treat))[2]

  diff <- abs(prop - aim)
  
  return(diff)
}


#################################################
##       optimization function intercept       ##
#################################################

#Function to optimize the intercept to obtain various targeted proportions treated
#INPUT:  d_p - the vector of targeted proportions treated
#        pred_mean - vector of means of the predictors used, 
#                    length corresponds to the number of predictors
#        pred_sigma - correlation-matrix of the predictors 
#        n - number of observations simulated for optimization
#OUTPUT: list
#        [[1]] - vector of optimized intercept for each proportion treated 
#        [[2]] - vector of obtained treated proportions corresponding to the 
#                optimized intercepts

intercept_optim <-
  function(d_p,
           pred_mean = p_mean,
           pred_sigma = p_sigma,
           n = n_large) {
    
    res_int <- numeric(length(d_p))
    opt_prop <- numeric(length(d_p))
    
    p <<- mvrnorm(n, mu = pred_mean, Sigma = pred_sigma)
    
    for (i in 1:length(d_p)) {
      aim <<- d_p[i]
      
      res_optimize <-
        optimize(
          loss_function_intercept,
          interval = c(-5, 5),
          tol = .Machine$double.eps ^ 0.5
        )
      res_int[i] <- res_optimize$minimum
      
      opt_prop[i] <- prop
    }
    return(list(res_int, opt_prop))
  }



#------------------------------------------------------------------------------#
# Optimizing parameters to obtain AUC and outcome proportion 
#------------------------------------------------------------------------------#

#Function to perform a grid search to find the optimal parameters 
# to obtain AUC and outcome proportion
#INPUT: scenarios_AUC  - matrix of scenarios for which parameters are needed
#       res_int        - vector of the optimized intercepts for targeted treatment proportion
#                       for each scenario
#       intercept_list - list of sequences for the grid for the intercept (beta_0)
#       slope          - sequence for the grid for the slope (beta_1)
#       scen_int       - vector to match scenario with a sequence of the intercept_list
#                        (for second grid search or later to reduce run time)
#       e_small        - what is considered a very small derivation from the 
#                        targeted AUC and outcome proportion
#       e_stop         - derivation from the targeted AUC and outcome proportion
#                        for which grid search is stopped if e > e_stop
#                        (after having been smaller than e_small at least for one setting)
#       req_AUC        - targeted AUC
#       req_out_p      - targeted outcome proportion
#       pred_mean      - vector of means of the predictors used, 
#                        length corresponds to the number of predictors
#       pred_sigma     - correlation-matrix of the predictors 
#       d_prop         - vector of treatment proportions included in scenarios
#OUTPUT: matrix with the one row per row and the following columns
#       method - for which approach we optimize (ignore)
#       d_p    - target treatment proportion for scenario 
#       d_ES   - target treatment effect size for scenario 
#       int_t  - optimized intercept for scenario
#       beta.1 - optimized beta_0 for scenario
#       beta.2 - optimized beta_1 for scenario
#       AUC_ig - obtained AUC for ignore approach for optimized parameters
#       AUC_ig - obtained AUC for naive approach for optimized parameters
#    outcome_p - obtained outcome proportion for optimized parameters
#      treat_p - obtained treatment proportion for optimized parameters
#       e      - derivation from targeted AUC and outcome prop. for optimized parameters
#    sd_beta_0 - standard deviation of beta_0 across the repetitions 
#                 (# given by n_beta_rep)
#    sd_beta_1 - standard deviation of beta_1 across the repetitions 
#                 (# given by n_beta_rep)

AUC_grid <- function(scenarios_AUC = scen_AUC[1, ],
                     res_int = res_int_mean,
                     intercept_list = int_list,
                     slope = seq(0.2, 0.35, by = 0.01),
                     scen_int = rep(1, scen_AUC),
                     e_small,
                     e_stop,
                     req_AUC = my_req_AUC,
                     req_out_p = my_req_out_p,
                     n_pred,
                     pred_mean,
                     pred_sigma,
                     d_prop) {
  
  #Starting AUC of 0.5 and e of 1000
  AUC <- 0.5
  e <- 1000
  
  
  res_AUC_ig <-
    res_AUC_na <- numeric(length = nrow(scenarios_AUC))
  #n_p <<- length(p_mean)
  
  
  res_mean <-
    cbind("method" = rep("ignore", nrow(scenarios_AUC)),
      scenarios_AUC[, 1:2],
      "int_t" = numeric(nrow(scenarios_AUC)),
      "beta" = matrix(0, nrow = nrow(scenarios_AUC), ncol = 2),
      #"b_l" = numeric(nrow(scenarios_AUC)),
      "AUC_ig" = numeric(nrow(scenarios_AUC)),
      "AUC_na" = numeric(nrow(scenarios_AUC)),
      "outcome_p" = numeric(nrow(scenarios_AUC)),
      "treat_p" =  numeric(nrow(scenarios_AUC)),
      "e" = numeric(nrow(scenarios_AUC)),
      "sd_beta_0" = numeric(nrow(scenarios_AUC)),
      "sd_beta_1" = numeric(nrow(scenarios_AUC)))
  
  
  
  for (S in 1:nrow(scenarios_AUC)) {
    intercept <- intercept_list[[scen_int[S]]]
    res_mat <-
      as.data.frame(
        cbind(
          "method" = rep("ignore", n_beta_rep),
          matrix(
            rep(scenarios_AUC[S, 1:2], n_beta_rep),
            nrow = n_beta_rep,
            ncol = 2,
            byrow = TRUE
            ),
          "int_t" = numeric(n_beta_rep),
          "beta" = matrix(0, nrow = n_beta_rep, ncol = 2),
          #"b_l" = numeric(n_beta_rep),
          "AUC_ig" = numeric(n_beta_rep),
          "AUC_na" = numeric(n_beta_rep),
          "outcome_p" = numeric(n_beta_rep),
          "treat_p" =  numeric(n_beta_rep),
          "e" =  numeric(n_beta_rep))
      )
    
    opt_betas_mat <- matrix(NA, nrow = n_beta_rep, ncol = 3)
    
    small <- "FALSE" #checking if good results have been obtained, to stop if e is increasing again
    
    for (k in 1:n_beta_rep) {
      for (beta_0 in intercept) {
        for (beta_1 in slope) {
          print(paste("scenario", S, "rep.", k))
          set.seed(start_seed + k)
          cat("seed = ", start_seed + k, "\n")
          opt_betas_mat[k, 1] <- start_seed + k
          #CONTINOUS PREDICTORS
          if (n_pred == 1) {
            deri <- as.data.frame(rnorm(n_large, pred_mean, pred_sigma))
          } else{
            deri <- as.data.frame(mvrnorm(n_large, pred_mean, pred_sigma))
          }
          
          #TREATMENT
          int <-
            res_int[which(d_prop == scenarios_AUC$d_p[S])]
          
          #based on predictors
          deri$treat <-
            rbinom(n_large, 1, prob = plogis(int + as.matrix(deri))) 
          
          #save the proportion treated, should be close to d_p according to scenario
          d_t_p <- prop.table(table(deri$treat))[2]
          
          #Saving previous AUC value for comparison
          AUC_previous <-
            AUC #First time AUC is set to 0.5, then updated
          
          
          # Calculate probabilities
          #EL: only equal weight to predictors for large sample size
          #    option to only have 1 predictor
          
          #OUTCOME
          #considering treatment, L, and P
          deri$prob <-
            plogis(beta_0 + beta_1 * rowSums(as.matrix(deri[, 1:n_pred])) + scenarios_AUC$d_ES[S] * deri$treat)
          
          deri$out_treat <- rbinom(n_large, 1, deri$prob)
          
          #save the outcome proportion, should be approx.
          d_o_p <- prop.table(table(deri$out_treat))[2]
          
          #                Model builing              #
          #model ignoring treatment
          lr_ignore <-
            glm(out_treat ~ . - treat - prob - out_treat ,
                data = deri,
                family = "binomial")
          
          
          #model naive
          lr_naive <-
            glm(out_treat ~ . - treat - prob - out_treat ,
                data = deri[which(deri$treat == 0), ],
                family = "binomial")
          
          if (lr_naive$null.deviance == 0 ||
              lr_ignore$null.deviance == 0) {
            print("glm.fit does not converge")
            break
          }
          
          #             Risk predictions              #
          
          #ignore
          pred_deri_ig <- predict(lr_ignore, type = "response")
          #naive
          pred_deri_na <- predict(lr_naive, type = "response")
          
          
          #            Calculating the AUC            #
          pm_deri_ig <-
            val.prob(p = pred_deri_ig, deri$out_treat, pl = FALSE)
          res_AUC_ig <-  pm_deri_ig['C (ROC)']
          
          pm_deri_na <-
            val.prob(p = pred_deri_na, deri$out_treat[which(deri$treat == 0)], pl = FALSE)
          res_AUC_na <- pm_deri_na['C (ROC)']
          
          
          if (d_o_p == 0) {
            AUC <- AUC_previous
          }
          else {
            AUC <- res_AUC_ig
          }
          
          #Previous deviation from targeted AUC and outcome proportion
          e_previous <- e
          
          
          # L2-norm continuous over entire domain to be minimized
          e <-
            (AUC - req_AUC) ^ 2 + (d_o_p - req_out_p) ^ 2 
          print(
            paste(
              "With S =",S,
              "k =", k,
              "intercept=",round(beta_0, 4),
              "slope=",round(beta_1, 4),
              "\n",
              "AUC_ig is",round(res_AUC_ig, 4),
              "\n",
              "AUC_na is",round(res_AUC_na, 4),
              "outcome prop.=", d_o_p,
              "e =", e,
              "small =",small
            )
          )
          
          #checking whether the current setting is better than the best out of 
          # the previous settings
          if (e < e_previous) {
            res_mat[k, 4:11] <-
              as.numeric(c(
                int, beta_0, beta_1,
                res_AUC_ig, res_AUC_na,
                d_o_p, d_t_p,
                round(e, digits = 4)
              ))
            
            #this helps terminating the grid search sooner if min. was reached 
            if (e < e_small) {  
              small <- "TRUE"
            }
          } else{
            if (e > e_stop && small == TRUE)
              break
          } 
        }
      }
    }
    
    #saving results
    res_mean[S, 4:13] <-
      c(
        apply(matrix(unlist(res_mat[, 4:11]), nrow = n_beta_rep, ncol = 8), 2, mean), 
        apply(matrix(unlist(res_mat[, 5:6]), nrow = n_beta_rep, ncol = 2), 2, sd))
  }
  return(res_mean)
}



# Functions to perform simulation study for the large and finite sample size study
# 2_Simulation_Large.R and 2_Simulation_Finite.R

# Scenario: which scenario is simulation
# I: which iteration are we in
# d_p: proportion treated in derivation data set
# v_p: proportion treated in validation data set 
# d_ES: treatment effect size in derivation data set
# v_ES: treatment effect size in validation data set 
# d_t_p: actual treatment proportion in derivation data set
# v_t_p: actual treatment proportion in validation data set
# d_o_p: actual outcome proportion in derivation data set
# v_o_p: actual outcome proportion in validation data set
# d_untreat_o_p: actual outcome proportion in untreated derivation data set
# v_untreat_o_p: actual outcome proportion in untreated validation data set
# d_AUC: AUC for derivation
# v_AUC: AUC for validation
# d_Brier: Brier score for derivation
# v_Brier: Brier score for validation
# d_c_mean: calibration mean for derivation
# d_c_slope: calibration slope for derivation
# v_c_mean: calibration mean for validation
# v_c_slope: calibration slope for validation

## Summary:
# The function my_sim performs the simulation to obtain performance measures
# for a given set of scenarios and number of iterations using the tuned 
# parameter settings from AUC_grid() and intercept_.

## Input: 
#  - n_deri: sample size for derivation data set
#  - n_vali: sample size for validation data set
#  - num_scen: vector of evaluated scenarios
#  - beta_mat: matrix of tuned intercept and effect of predictors
#  - I: number of iteration for each of the scenarios
#  - scen: matrix of scenarios, including treatm. prop. and effect size
#  - treat_rel: options "all" - treatment based on all predictors (default)
#                       "no" - treatment not based on the predictors
#  - n_pred: number of predictors
#  - pred_mean: vector of means of the predictors used
#  - pred_sigma: correlation-matrix of the predictors 

## Output:
# A list containing 
#  - [[1]] "Results": data frame 
#    results for every scenario and iteration for the simulation study
#  - [[2]] "warn": matrix with 3 columns and same number of rows as the matrix 
#                      in "Results"
#    number of non-converged glm for ignore and naive approach separately and summed
#    up
#  - [[3]] "error": matrix with 3 columns and same number of rows as the matrix 
#                      in "Results"
#    number of errors in glm for ignore and naive approach separately and summed
#    up


my_sim <- function(n_deri,
                   # sample size for derivation data set
                   n_vali,
                   # sample size for validation data set
                   num_scen = 1:N,
                   # vector of evaluated scenarios
                   beta_mat = beta_tuned,
                   # matrix of tuned intercept and effect of predictors
                   I = iter,
                   # number of iteration for each of the scenarios
                   scen = scenarios,
                   # matrix of scenarios, including treatm. prop. and effect size
                   treat_rel = "all",
                   # treatment based on non/all predictors
                   n_pred,
                   # number of predictors
                   pred_mean,
                   # vector of predictor means
                   pred_sigma
                   # matrix of standard deviation for predictors
                   ) {
  #treatment related to no/all of the predictors
  
  # matrix saving results for iterations
  res_it_blank <- matrix(nrow = I * 2, ncol = 25)
  colnames(res_it_blank) <-
    c(
      "Scenario",
      "I",
      "d_p",
      "d_ES",
      "v_p",
      "v_ES",
      "d_t_p",
      "v_t_p",
      "d_o_p" ,
      "v_o_p",
      "d_untreat_o_p",
      "v_untreat_o_p",
      "d_AUC",
      "v_AUC",
      "d_Brier",
      "v_Brier",
      "d_c_mean",
      "v_c_mean",
      "d_c_slope",
      "v_c_slope",
      "error_val.p",
      "warn",
      "warn_mess",
      "warn_mess_pred_deri",
      "warn_mess_pred_vali"
    )
  #   "p_t_1", "p_t_3", "p_t_5")
  
  rownames(res_it_blank) <- rep(c("Ignore", "Naive"), I)
  res_it_blank[, "warn_mess"] <- rep(NA, I * 2)
  #for counting non-converged glm per scenario
  warn_scen_ig <- warn_scen_na <- numeric(length(num_scen))
  error_scen_deri <- error_scen_vali <- numeric(length(num_scen))
  
  #SCENARIO
  for (S in num_scen) {
    print(S)
    
    ################# Setting up results matrix #####################
    res_it <- as.data.frame(res_it_blank)
    #specifying scenario in the result matrix
    res_it[, 1] <- S
    
    #choosing beta for the respective scenario
    d_p_int <- which(scen$d_p[S] == beta_mat$d_p)
    beta_int <-
      d_p_int[which(scen$d_ES[S] == beta_mat$d_ES[d_p_int])]
    
    beta <- beta_mat[beta_int, 3:4]
    
    #for counting non-converged glm per iteration
    warn_ig <-
      warn_na <- warn_pred_deri <- warn_pred_vali <- numeric(I)
    error_deri <- error_vali <- numeric(I)
    
    #ITERATION
    for (i in 1:I) {
      #print(i)
      
      #specify which iteration in the result matrix
      res_it[((i - 1) * 2 + 1):(i * 2), 2] <- i
      
      #selecting the parameters for given scenario (+ adding them to the result matrix)
      res_it[((i - 1) * 2 + 1):(i * 2), 3:6] <-
        rep(as.numeric(scen[S, 1:4]), each = 2)
      
      ############### Generating data for scenario & iteration ################
      ###DERIVATION###
      #CONTINOUS PREDICTORS
      deri <- as.data.frame(mvrnorm(n_deri, pred_mean, pred_sigma))
      
      #TREATMENT
      
      #based on L (choose the (1-der_prop)-quantile of the normal distribution as intercept)
      #deri$treat_l <- rbinom(n,1, plogis(qnorm((1 - res_it$d_p[1]), l_mean, l_sd) + deri$l))
      
      #not based on any predictor
      if (treat_rel == "no") {
        deri$treat <- rbinom(n_deri, 1, scen$d_p[S])
      }
      
      #based on all predictors
      if (treat_rel == "all") {
        deri$treat <-
          rbinom(n_deri, 1, plogis(scen$d_int[S] + as.matrix(deri)))
      }
      
      
      #save the proportion treated, should be close to d_p according to scenario
      res_it$d_t_p[((i - 1) * 2 + 1):(i * 2)] <-
        (table(deri$treat) / n_deri)[2]
      
      #OUTCOME
      #considering treatment, L, and P
      deri$prob <-
        plogis(as.numeric(
          as.numeric(beta[1]) + as.numeric(beta[2]) * rowSums(as.matrix(deri[, 1:n_pred])) + res_it$d_ES[1] * deri$treat
        ))
      
      deri$out_treat <- rbinom(n_deri, 1, deri$prob)
      
      #save the outcome proportion, should be approx. 0.5 for large; 0.2 for finite
      res_it$d_o_p[((i - 1) * 2 + 1):(i * 2)] <-
        (table(deri$out_treat) / n_deri)[2]
      
      
      
      #save the outcome proportion in the untreated
      res_it$d_untreat_o_p[((i - 1) * 2 + 1):(i * 2)] <-
        (table(deri$out_treat[which(deri$treat == 0)]) / n_deri)[2]
      
      ###VALIDATION###
      #CONTINOUS PREDICTOR
      vali <- as.data.frame(mvrnorm(n_vali, pred_mean, pred_sigma))
      
      #TREATMENT
      #treatment not based on predictors
      if (treat_rel == "no") {
        vali$treat <- rbinom(n_vali, 1, scen$v_p[S])
      }
      
      #treatment based on all predictors
      if (treat_rel == "all") {
        vali$treat <- rbinom(n_vali, 1, plogis(scen$v_int[S] + as.matrix(vali)))
      }
      
      
      #save the proportion treated, should be close to v_p according to scenario
      res_it$v_t_p <- (table(vali$treat) / n_vali)[2]
      
      #OUTCOME
      #considering treatment, L, and P
      
      vali$prob <-
        plogis(as.numeric(
          as.numeric(beta[1]) +  as.numeric(beta[2]) * rowSums(as.matrix(vali[, 1:n_pred])) + res_it$v_ES[1] * vali$treat
        ))
      vali$out_treat <- rbinom(n_vali, 1, vali$prob)
      
      #save the outcome proportion
      res_it$v_o_p[((i - 1) * 2 + 1):(i * 2)] <-
        (table(vali$out_treat) / n_vali)[2]
      
      #save the outcome proportion in the untreated, should be approx. 0.5
      res_it$v_untreat_o_p[((i - 1) * 2 + 1):(i * 2)] <-
        (table(vali$out_treat[which(vali$treat == 0)]) / n_vali)[2]
      
      
      ############### Fitting the models to the derivation data  ################
      #model ignoring treatment
      catch_ig <-
        tryCatch.W.E(glm(
          out_treat ~ . - treat - prob - out_treat ,
          data = deri,
          family = "binomial"
        ))
      lr_ignore <- catch_ig$value
     
      if (!is.null(catch_ig[[2]])) {
        print(paste(
          "Ignore",
          catch_ig$warning$message ,
          "in Scenario",
          S,
          "Iteration",
          i
        ))
        warn_ig[i] <- warn_ig[i] + 1
        if (grepl("Wahrscheinlichkeit", catch_ig$warning$message)) {
          res_it$warn_mess[((i - 1) * 2 + 1)] <- "prob"
        }
        if (grepl("konvergiert", catch_ig$warning$message)) {
          res_it$warn_mess[((i - 1) * 2 + 1)] <- "conv"
        }
        
      }
      
      #model naive
      catch_na <-
        tryCatch.W.E(glm(
          out_treat ~ . - treat - prob - out_treat ,
          data = deri[which(deri$treat == 0), ],
          family = "binomial"
        ))
      lr_naive <- catch_na$value
      #summary(lr_naive)
      if (!is.null(catch_na[[2]])) {
        print(paste(
          "Naive",
          catch_na$warning$message ,
          "in Scenario",
          S,
          "Iteration",
          i
        ))
        warn_na[i] <- warn_na[i] + 1
        if (grepl("Wahrscheinlichkeit", catch_na$warning$message)) {
          res_it$warn_mess[2 * i] <- "prob"
        }
        if (grepl("konvergiert", catch_na$warning$message)) {
          res_it$warn_mess[2 * i] <- "conv"
        }
      }
      
      ############### Risk Predictions  ################
      ###DERIVATION & VALIDATION###
      
      #ignore
      pred_deri_ig <- predict(lr_ignore, type = "response")
      pred_vali_ig <-
        predict(lr_ignore, type = "response", newdata = vali)
      
      #naive
      catch_pred_deri <-
        tryCatch.W.E(predict(lr_naive, type = "response", newdata = deri[which(deri$treat == 0), ]))
      if (!is.null(catch_pred_deri[[2]])) {
        print(
          paste(
            "Deri",
            catch_pred_deri$warning$message ,
            "in Scenario",
            S,
            "Iteration",
            i
          )
        )
        warn_pred_deri[i] <- warn_pred_deri[i] + 1
        if (grepl("Rang", catch_pred_deri$warning$message)) {
          res_it$warn_mess_pred_deri[((i - 1) * 2 + 1)] <- "rank"
        }
      }
      pred_deri_na <- catch_pred_deri$value
      
      
      catch_pred_vali <-
        tryCatch.W.E(predict(lr_naive, type = "response", newdata = vali))
      if (!is.null(catch_pred_vali[[2]])) {
        print(
          paste(
            "Vali",
            catch_pred_vali$warning$message ,
            "in Scenario",
            S,
            "Iteration",
            i
          )
        )
        warn_pred_vali[i] <- warn_pred_vali[i] + 1
        if (grepl("Rang", catch_pred_vali$warning$message)) {
          res_it$warn_mess_pred_vali[(i * 2)] <- "rank"
        }
      }
      pred_vali_na <- catch_pred_vali$value
      
      
      
      ############### Performance measures  ################
      #ignore
      #derivation
      pm_deri_ig <-
        val.prob(p = pred_deri_ig, deri$out_treat, pl = FALSE)
      res_it$d_AUC[((i - 1) * 2 + 1)] <- pm_deri_ig[2]
      res_it$d_Brier[((i - 1) * 2 + 1)] <-
        1 - (pm_deri_ig[11] / (mean(pred_deri_ig) * (1 - mean(pred_deri_ig))))
      res_it$d_c_mean[((i - 1) * 2 + 1)] <- pm_deri_ig[12]
      res_it$d_c_slope[((i - 1) * 2 + 1)] <- pm_deri_ig[13]
      
      
      #validation
      pm_vali_ig <-
        val.prob(p = pred_vali_ig, vali$out_treat, pl = FALSE)
      
      res_it$v_AUC[((i - 1) * 2 + 1)] <- pm_vali_ig[2]
      res_it$v_Brier[((i - 1) * 2 + 1)] <-
        1 - (pm_vali_ig[11] / (mean(pred_vali_ig) * (1 - mean(pred_vali_ig))))
      res_it$v_c_mean[((i - 1) * 2 + 1)] <- pm_vali_ig[12]
      res_it$v_c_slope[((i - 1) * 2 + 1)] <- pm_vali_ig[13]
      
      
      #naive
      #derivation
      catch_val.prob_deri_na <-
        tryCatch.W.E(val.prob(p = pred_deri_na, deri$out_treat[which(deri$treat == 0)], pl = FALSE))
      
      if (inherits(catch_val.prob_deri_na$value, "error")) {
        print(paste("deri_na", catch_val.prob_deri_na$value))
        error_deri[i] <- error_deri[i] + 1
        next
      }
      pm_deri_na <- catch_val.prob_deri_na$value
      res_it$d_AUC[(i * 2)] <- pm_deri_na[2]
      res_it$d_Brier[(i * 2)] <-
        1 - (pm_deri_na[11] / (mean(pred_deri_na) * (1 - mean(pred_deri_na))))
      res_it$d_c_mean[(i * 2)] <- pm_deri_na[12]
      res_it$d_c_slope[(i * 2)] <- pm_deri_na[13]
      
      
      #validation
      if (pred_vali_na[1] < 0.0000000001 ||
          pred_vali_na[1] > 0.9999999999) {
        print("Predictions very close to 0 or 1")
      }
      catch_val.prob_vali_na  <-
        tryCatch.W.E(val.prob(p = pred_vali_na, vali$out_treat, pl = FALSE))
      
      if (inherits(catch_val.prob_vali_na$value, "error")) {
        print(paste("vali_na", catch_val.prob_vali_na$value))
        error_vali[i] <- error_vali[i] + 1
        res_it$warn[((i - 1) * 2 + 1):(i * 2)] <-
          c(warn_ig[i],  warn_na[i])
        res_it$error_val.p[((i - 1) * 2 + 1):(i * 2)] <-
          c(0, error_deri[i] + error_vali[i])
        next
      }
      pm_vali_na <- catch_val.prob_vali_na$value
      
      res_it$v_AUC[(i * 2)] <- pm_vali_na[2]
      res_it$v_Brier[(i * 2)] <-
        1 - (pm_vali_na[11] / (mean(pred_vali_na) * (1 - mean(pred_vali_na))))
      res_it$v_c_mean[(i * 2)] <- pm_vali_na[12]
      res_it$v_c_slope[(i * 2)] <- pm_vali_na[13]
      
      
      res_it$warn[((i - 1) * 2 + 1):(i * 2)] <-
        c(warn_ig[i],  warn_na[i])
      res_it$error_val.p[((i - 1) * 2 + 1):(i * 2)] <-
        c(0, error_deri[i] + error_vali[i])
      
    } #end ITERATION
    
    if (S == num_scen[1]) {
      res_scen <- res_it
    } else{
      res_scen <- rbind(res_scen, res_it)
    }
    
    warn_scen_ig[which(num_scen == S)] <- sum(warn_ig)
    warn_scen_na[which(num_scen == S)] <- sum(warn_na)
    
    error_scen_deri[which(num_scen == S)] <- sum(error_deri)
    error_scen_vali[which(num_scen == S)] <- sum(error_vali)
    
    
  } #end SCENARIO
  
  
  return(list(
    "Results" = res_scen,
    "warn" = as.data.frame(
      cbind(
        "ignore" = warn_scen_ig,
        "naive" = warn_scen_na,
        "sum" = warn_scen_ig + warn_scen_na
      )
    ),
    "errors" = as.data.frame(
      cbind(
        "deri" = error_scen_deri,
        "vali" = error_scen_vali,
        "sum" = error_scen_deri + error_scen_vali
      )
    )
  ))
}


