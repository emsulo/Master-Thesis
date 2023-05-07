

# PROCESSING RESULTS FROM 2_Simulation_Setup.R 
# using results saved in "./Results/res_sim_finite.RData"

#------------------------------------------------------------------------------#
# Loading packages
#------------------------------------------------------------------------------#

library(magrittr) #for pipes 
library(dplyr) #for summarize
library(ggplot2) #for plots
library(grid) #for strip 
library(gtable) #for strip

load("./Results/res_sim_large.RData")
load("./Results/n_dist_scen_large.RData")
res_inter_l <- res_sim_large$Results

#Preparation COLOR
my_palette <- c(rgb(1,205/255,0), rgb(192/255,10/255,53/255), rgb(0,0,0), 
                rgb(35/255,109/255, 182/255), rgb(60/255, 150/255,50/255))

#color scheme (RGB)
# UU orange 255/205/0
# UU red 192/10/53
# UU black 0/0/0
# UMC blue 35/109/182
# green 60/150/50

# AUC 
sum_d_AUC <- res_inter_l %>%  group_by(Scenario) %>%
  dplyr::summarize(Mean = mean(d_AUC, na.rm=TRUE), Min = min(d_AUC, na.rm=TRUE),
                   Max = max(d_AUC, na.rm=TRUE), Var = var(d_AUC, na.rm = TRUE),
                   SD = sd(d_AUC, na.rm=TRUE), n_NA = sum(is.na(d_AUC)))
#View(sum_d_AUC)

mean(sum_d_AUC$Mean)
median(sum_d_AUC$Mean)
sd(sum_d_AUC$Mean)
range(sum_d_AUC$Mean)



#d_p
sum_d_AUC %>% 
  #attach d_p for each scenario
  mutate("d_p" = scenarios_l$d_p) %>% 
  ggplot(aes(x = Mean, fill = as.factor(d_p))) +
  geom_histogram(bins = 50)

#d_ES
sum_d_AUC %>% 
  #attach d_p for each scenario
  mutate("d_p" = scenarios_l$d_p) %>% 
  mutate("d_ES" = scenarios_l$d_ES) %>% 
  ggplot(aes(x = Mean, fill = as.factor(d_ES))) +
  geom_histogram(bins = 50)


# OUTCOME PROPORTION  
sum_d_o_p <- res_inter_l %>%  group_by(Scenario) %>%
  dplyr::summarize(Mean = mean(d_o_p, na.rm=TRUE), Min = min(d_o_p, na.rm=TRUE),
                   Max = max(d_o_p, na.rm=TRUE), Var = var(d_o_p, na.rm = TRUE),
                   SD = sd(d_o_p, na.rm=TRUE), n_NA = sum(is.na(d_o_p)))
#View(sum_d_o_p)

mean(sum_d_o_p$Mean)
median(sum_d_o_p$Mean)
sd(sum_d_o_p$Mean)
range(sum_d_o_p$Mean)

#d_p
sum_d_o_p %>% 
  #attach d_p for each scenario
  mutate("d_p" = scenarios_l$d_p) %>% 
  ggplot(aes(x = Mean, fill = as.factor(d_p))) +
  geom_histogram(bins = 50)


#d_ES
sum_d_o_p %>% 
  #attach d_p for each scenario
  mutate("d_p" = scenarios_l$d_p) %>% 
  mutate("d_ES" = scenarios_l$d_ES) %>% 
  ggplot(aes(x = Mean, fill = as.factor(d_ES))) +
  geom_histogram(bins = 50)



# TREATMENT PROPORTION
sum_d_t_p <- res_inter_l %>%  group_by(Scenario) %>%
  dplyr::summarize(Mean = mean(d_t_p, na.rm=TRUE), Min = min(d_t_p, na.rm=TRUE),
                   Max = max(d_t_p, na.rm=TRUE), Var = var(d_t_p, na.rm = TRUE),
                   SD = sd(d_t_p, na.rm=TRUE), n_NA = sum(is.na(d_t_p)))
#View(sum_d_t_p)

#We take the differences of the obtained and targeted treatment proportion per scenario
# Difference for number of NA does not make sense
NA_ind <- which(colnames(sum_d_t_p) == "n_NA")
#View(sum_d_t_p[,-NA_ind] - scenarios_l$d_p)

mean(sum_d_t_p$Mean - scenarios_l$d_p)
sd(sum_d_t_p$Mean - scenarios_l$d_p)
range(sum_d_t_p$Mean - scenarios_l$d_p)

#d_p
sum_d_t_p %>% 
  #attach d_p for each scenario
  mutate("d_p" = scenarios_l$d_p) %>% 
  mutate("Diff" = Mean - d_p) %>% 
  ggplot(aes(x = Diff, fill = as.factor(d_p))) +
  geom_histogram(bins = 50)

#d_ES
sum_d_t_p %>% 
  #attach d_p for each scenario
  mutate("d_p" = scenarios_l$d_p) %>% 
  mutate("d_ES" = scenarios_l$d_ES) %>% 
  mutate("Diff" = Mean - d_p) %>% 
  ggplot(aes(x = Diff, fill = as.factor(d_ES))) +
  geom_histogram(bins = 50)

res_inter_l$d_untreat_o_p




#------------------------------------------------------------------------------#
# Plots for report (only validation, sorted by proportion/effect size)
# only T based on P
#------------------------------------------------------------------------------#

res_l <- res_sim_large$Results

#------------------------------------------------------------------------------#
# Results into long format for visualizations
#------------------------------------------------------------------------------#
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


#Save long format processed data set 
save(res_l, file = "./Results/res_large_processed.RData")
#load("./Results/res_large_processed.RData")





