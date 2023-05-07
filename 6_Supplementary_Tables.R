
# GETTING THE LATEX CODE FOR THE TABLES IN THE SUPPLEMENTARY MATERIALS 

#Note that adjustments were made in the LaTeX file after copying the xtable output
# for readablility reasons. 

#------------------------------------------------------------------------------#
# Loading packages
#------------------------------------------------------------------------------#

library(magrittr) #for pipes 
library(dplyr) #for summarize
library(ggplot2) #for plots
library(grid) #for strip 
library(xtable) #for LaTeX code

load("./Results/res_sim_large.RData")
load("./Results/n_dist_scen_large.RData")
res_l <- res_sim_large$Results

#Selecting only the columns for validation
res_l <- cbind(res_l[,c(3:6,seq(14,20,by = 2))])
res_l$Model <- as.factor(rep(c("Ignore", "Naive"), 81))

res_l <- res_l %>% mutate(diff_p = ifelse(d_p > v_p, "p_d > p_v", ifelse(d_p < v_p, "p_d < p_v", "p_d = p_v")),
                          diff_ES = ifelse(d_ES > v_ES, "|ES_d|<|ES_v|", ifelse(d_ES < v_ES, "|ES_d|>|ES_v|", "|ES_d|=|ES_v|")),
                          "v_p - d_p" = as.factor(round(v_p - d_p, 2)),
                          "v_ES - d_ES" = as.factor(round(abs(v_ES) - abs(d_ES), 2)),
                          label_p_diff = rep("p_v - p_d", nrow(res_l)),
                          label_ES_diff = rep("|ES_v| - |ES_d|", nrow(res_l)),
                          label_p_equal = rep("p_d = p_v", nrow(res_l)),
                          label_ES_equal = rep("|ES_d| = |ES_v|", nrow(res_l)),
                          d_ES = abs(d_ES),
                          v_ES = abs(v_ES)
)%>% 
  mutate(diff_p = as.factor(diff_p), diff_ES = as.factor(diff_ES)) 
#res_l$diff_ES <- relevel(res_l$diff_ES, "Effect weaker at v")
res_l$diff_p <- relevel(res_l$diff_p, "p_d < p_v")
colnames(res_l)[3] <- "p_d = p_v"
colnames(res_l)[4] <- "ES_d = ES_v"



load("./Results/n_dist_scen_finite.RData")
load("./Results/res_sim_finite.RData")
res_f <- res_sim_finite$Results

res_f <- cbind(res_f[,c(3:6,seq(14,20,by = 2))])
res_f$Model <- as.factor(rep(c("Ignore", "Naive"), nrow(res_f)/2))

res_f <- res_f %>% mutate(diff_p = ifelse(d_p > v_p, "p_d > p_v", 
                                          ifelse(d_p < v_p, "p_d < p_v", "p_d = p_v")),
                          diff_ES = ifelse(d_ES > v_ES, "|ES_d|<|ES_v|", 
                                           ifelse(d_ES < v_ES, "|ES_d|>|ES_v|", "|ES_d|=|ES_v|")),
                          "v_p - d_p" = as.factor(round(v_p - d_p, 2)),
                          "v_ES - d_ES" = as.factor(round(abs(v_ES) - abs(d_ES), 2)),
                          label_p_diff = rep("p_v - p_d", nrow(res_f)),
                          label_ES_diff = rep("|ES_v| - |ES_d|", nrow(res_f)),
                          label_p_equal = rep("p_d = p_v", nrow(res_f)),
                          label_ES_equal = rep("ES_d = ES_v", nrow(res_f)),
                          d_ES = abs(d_ES),
                          v_ES = abs(v_ES)
)%>% 
  mutate(diff_p = as.factor(diff_p), diff_ES = as.factor(diff_ES))
#res_f$diff_ES <- relevel(res_f$diff_ES, "Effect weaker at v")
res_f$diff_p <- relevel(res_f$diff_p, "p_d < p_v")
colnames(res_f)[3] <- "p_d = p_v"
colnames(res_f)[4] <- "ES_d = ES_v"




###############################################
# Table 1: Study 1, Ignore Treatment approach #
###############################################

supp_sum_l_ig <- res_l %>% 
  filter(`v_p - d_p` !=0, `v_ES - d_ES` != 0 , Model == "Ignore") %>% 
  group_by(`v_ES - d_ES`, `v_p - d_p`,  d_ES, d_p) %>% 
  summarise("ES_v" = `ES_d = ES_v`,"p_v" = `p_d = p_v` ,  "Int" = v_c_mean, 
            "Slope" = v_c_slope, "c_stat" = v_AUC, "Brier" = v_Brier, .groups = "keep") %>% 
  select(`v_ES - d_ES`,  d_ES , ES_v, `v_p - d_p`,  d_p, p_v, Int, Slope, c_stat, Brier)

print(xtable(supp_sum_l_ig, digits = c(0,1,2,2,1,2,2,4,4,4,4)), include.rownames =FALSE)

###############################################
# Table 2: Study 1, Treatment Naive approach #
###############################################
supp_sum_l_na <- res_l %>% 
  filter(`v_p - d_p` !=0, `v_ES - d_ES` != 0 , Model == "Naive") %>%  
  group_by(`v_ES - d_ES`, `v_p - d_p`,  d_ES, d_p) %>% 
  summarise("ES_v" = `ES_d = ES_v`,"p_v" = `p_d = p_v` ,  "Int" = v_c_mean, 
            "Slope" = v_c_slope, "c_stat" = v_AUC, "Brier" = v_Brier, .groups = "keep") %>% 
  select(`v_ES - d_ES`,  d_ES , ES_v, `v_p - d_p`,  d_p, p_v, Int, Slope, c_stat, Brier)


print(xtable(supp_sum_l_na, digits = c(0,1,2,2,1,2,2,4,4,4,4)), include.rownames =FALSE)



###############################################
# Table 3: Study 2, Ignore Treatment approach #
###############################################
supp_sum_f_ig <- res_f %>%
  filter(`v_p - d_p` !=0, `v_ES - d_ES` != 0 ,Model == "Ignore") %>%   
  group_by(`v_ES - d_ES`,`v_p - d_p`, d_p, d_ES) %>% 
  summarise("v_ES" = unique(`ES_d = ES_v`), "p_v" = unique(`p_d = p_v`), "Int_median" = median(v_c_mean), "Int_mean" = mean(v_c_mean), "Int_var" = var(v_c_mean),
            "Slope_median" = median(v_c_slope), "Slope_mean" = mean(v_c_slope), "Slope_var" = var(v_c_slope),
            "c_stat_median" = median(v_AUC), "c_stat_mean" = mean(v_AUC), "c_stat_var" = var(v_AUC),
            "Brier_median" = median(v_Brier), "Brier_mean" = mean(v_Brier), "Brier_var" = var(v_Brier), .groups = "keep")
supp_sum_f_ig <- supp_sum_f_ig %>%   
  select(`v_ES - d_ES`,  d_ES , v_ES, `v_p - d_p`,  d_p, p_v, colnames(supp_sum_f_ig)[7:18])


print(xtable(supp_sum_f_ig, digits = c(0,1,2,2,1,2,2,rep(4,12)), 
             display=c(rep("f", 7), rep(c("f", "f", "e"), 4))), 
      include.rownames =FALSE)

###############################################
# Table 3: Study 2, Treatment Naive approach  #
###############################################
supp_sum_f_na <- res_f %>%
  filter(`v_p - d_p` !=0, `v_ES - d_ES` != 0 ,Model == "Naive") %>%   
  group_by(`v_ES - d_ES`,`v_p - d_p`, d_p, d_ES) %>% 
  summarise("v_ES" = unique(`ES_d = ES_v`), "p_v" = unique(`p_d = p_v`), "Int_median" = median(v_c_mean), "Int_mean" = mean(v_c_mean), "Int_var" = var(v_c_mean),
            "Slope_median" = median(v_c_slope), "Slope_mean" = mean(v_c_slope), "Slope_var" = var(v_c_slope),
            "c_stat_median" = median(v_AUC), "c_stat_mean" = mean(v_AUC), "c_stat_var" = var(v_AUC),
            "Brier_median" = median(v_Brier), "Brier_mean" = mean(v_Brier), "Brier_var" = var(v_Brier), .groups = "keep")
supp_sum_f_na <- supp_sum_f_na %>% 
  select(`v_ES - d_ES`,  d_ES , v_ES, `v_p - d_p`,  d_p, p_v, colnames(supp_sum_f_ig)[7:18])


print(xtable(supp_sum_f_na, digits = c(0,1,2,2,1,2,2,rep(4,12)), display=c(rep("f", 7), rep(c("f", "f", "e"), 4))), include.rownames =FALSE)
