
# CREATING PLOTS DISPLAYED IN THE APPENDIX OF THE REPORT

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
load("./Results/res_large_processed.RData")


load("./Results/n_dist_scen_finite.RData")
load("./Results/res_sim_finite.RData")
load("./Results/res_finite_processed.RData")

my_palette <- c(rgb(1,205/255,0), rgb(192/255,10/255,53/255), rgb(0,0,0), 
                rgb(35/255,109/255, 182/255), rgb(60/255, 150/255,50/255))


###########
# Study 1 #
###########



#Appendix plots 
# Intercept + ES varies
int_ES <- res_l %>% 
  filter(d_p == `p_d = p_v`) %>%
  mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(`ES_d = ES_v`), y= v_c_mean, col = Model))+ 
  geom_point(size = 1.2)+
  facet_grid(  `p_d = p_v`  ~  `v_ES - d_ES`, labeller = labeller(`p_d = p_v` = label_value, diff_ES = label_value))+
  geom_hline(yintercept = 0, col= my_palette[2] )+
  labs(x = expression(Validation~Effect~Size~(ES[v])), 
       y= "Calibration-in-the-large \n Coeffiecient",
       title = "Heterogeneity in Effect Size",
       tag = "Constant Proportion")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.85, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
        
        
        
  )
int_ES

ggsave("./Plots/int_ES_large_abs.png", int_ES, height=1050,width=1800, units = "px")

# Intercept + prop. varies
int_prop <- res_l %>% 
  filter(d_ES == `ES_d = ES_v`) %>% 
  mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(`p_d = p_v`), y= v_c_mean, col = Model ))+ 
  geom_point(size = 1.2)+
  geom_hline(yintercept = 0, col= my_palette[2] )+
  facet_grid( `ES_d = ES_v`  ~  `v_p - d_p`, labeller = labeller(`ES_d = ES_v` =label_value, diff_p = label_value))+
  labs(x = expression(Validation~Effect~Size~(ES[v])), 
       y= "Calibration-in-the-large \n Coeffiecient",
       title = "Heterogeneity in Proportion",
       tag = "Constant Effect Size")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.85, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
int_prop
ggsave("./Plots/int_prop_large_abs.png", int_prop, height=1050,width=1800, units = "px")


# Slope + ES varies
slope_ES <- res_l %>%
  filter(d_p == `p_d = p_v`) %>% 
  #filter(d_p == 0.50) %>%  #only middle row
  mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(`ES_d = ES_v`), y= v_c_slope, col = Model ))+ 
  geom_point(size = 1.2)+
  facet_grid( `p_d = p_v`  ~ `v_ES - d_ES`, labeller = labeller(`p_d = p_v` = label_value, diff_ES = label_value))+
  geom_hline(yintercept = 1 , col= my_palette[2] )+
  labs(x = expression(Validation~Effect~Size~(ES[v])), 
       y= "Calibration Slope",
       title = "Heterogeneity in Effect Size",
       tag = "Constant Proportion")+
  scale_color_manual(values = my_palette[c(3,4)])+
  ylim(0.8,1.2)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.85, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
slope_ES
ggsave("./Plots/slope_ES_large_abs.png", slope_ES, height=1050,width=1800, units = "px")

# Slope + prop. varies
slope_prop <- res_l %>% 
  filter(d_ES == `ES_d = ES_v`) %>%
  mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(`p_d = p_v`), y= v_c_slope, col = Model ))+ 
  geom_point(size = 1.2)+
  geom_hline(yintercept = 1, col= my_palette[2] )+
  facet_grid( `ES_d = ES_v`  ~  `v_p - d_p`, labeller = labeller(`ES_d = ES_v` =label_value, diff_p = label_value))+
  labs(x = expression(Validation~Effect~Size~(ES[v])), 
       y= "Calibration Slope",
       title = "Heterogeneity in Proportion",
       tag = "Constant Effect Size")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.85, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
slope_prop

ggsave("./Plots/slope_prop_large_abs.png", slope_prop, height=1050,width=1800, units = "px")



# c-statistic +ES varies
auc_ES <- res_l %>% 
  filter(d_p == `p_d = p_v`) %>% 
  mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(`ES_d = ES_v`), y= v_AUC, col = Model ))+ 
  geom_point(size = 1.2)+
  facet_grid( `p_d = p_v`  ~ `v_ES - d_ES`, labeller = labeller(`p_d = p_v` = label_value, diff_ES = label_value))+
  geom_hline(yintercept = 0.8 , col= my_palette[2] )+
  labs(x = expression(Validation~Effect~Size~(ES[v])), 
       y= "C-statistic",
       title = "Heterogeneity in Effect Size",
       tag = "Constant Proportion")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.85, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
auc_ES
ggsave("./Plots/auc_ES_large_abs.png", auc_ES, height=1050,width=1800, units = "px")

# c-statistic +prop. varies
auc_prop <- res_l %>% 
  filter(d_ES == `ES_d = ES_v`) %>%
  mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, digits= 2)) %>% 
  ggplot(aes(x = as.factor(`p_d = p_v`), y= v_AUC, col = Model ))+ 
  geom_point(size = 1.2)+
  geom_hline(yintercept = 0.8, col= my_palette[2] )+
  facet_grid( `ES_d = ES_v`  ~  `v_p - d_p`, labeller = labeller(`ES_d = ES_v` =label_value, diff_p = label_value))+
  labs(x = expression(Validation~Effect~Size~(ES[v])), 
       y= "C-statistic",
       title = "Heterogeneity in Proportion",
       tag = "Constant Effect Size")+scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.85, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
auc_prop

ggsave("./Plots/auc_prop_large_abs.png", auc_prop, height=1050,width=1800, units = "px")

# Brier + ES varies
brier_ES <- res_l %>% 
  filter(d_p == `p_d = p_v`) %>% 
  mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(`ES_d = ES_v`), y= v_Brier, col = Model ))+ 
  geom_point(size = 1.2)+
  facet_grid( `p_d = p_v`  ~ `v_ES - d_ES`, labeller = labeller(`p_d = p_v` = label_value, diff_ES = label_value))+
  #geom_hline(yintercept = 1 , col= my_palette[2] )+
  labs(x = expression(Validation~Effect~Size~(ES[v])), 
       y= "Scaled Brier score",
       title = "Heterogeneity in Effect Size",
       tag = "Constant Proportion")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.85, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
brier_ES
ggsave("./Plots/brier_ES_large_abs.png", brier_ES,height=1050,width=1800, units = "px")

# Brier +prop. varies
brier_prop <- res_l %>% 
  filter(d_ES == `ES_d = ES_v`) %>%
  mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(`p_d = p_v`), y= v_Brier, col = Model))+ 
  geom_point(size = 1.2)+
  #geom_hline(yintercept = 1, col= my_palette[2] )+
  facet_grid( `ES_d = ES_v`  ~  `v_p - d_p`, labeller = labeller(`ES_d = ES_v` =label_value, diff_p = label_value))+
  labs(x = expression(Validation~Effect~Size~(ES[v])), 
       y= "Scaled Brier score",
       title = "Heterogeneity in Proportion",
       tag = "Constant Effect Size")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.85, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
brier_prop

ggsave("./Plots/brier_prop_large_abs.png", brier_prop, height=1050,width=1800, units = "px") 





###########
# Study 2 #
###########



# Intercept + ES varies
int_ES_f <- res_f %>% 
  filter(d_p == `p_d = p_v`) %>% 
  mutate(`ES_d = ES_v` == round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(round(`ES_d = ES_v`, 2)), y= v_c_mean, col = Model))+ 
  geom_boxplot(outlier.size = 0.5, lwd = 0.4)+
  facet_grid( `p_d = p_v`  ~ `v_ES - d_ES`, labeller = labeller(`p_d = p_v` = label_value, `v_ES - d_ES` = label_value))+
  geom_hline(yintercept = 0, col= my_palette[2] )+
  labs(x = expression(Validation~Effect~Size~(ES[v])), y= "Calibration-in-the-large \n Coefficient",
       tag = "Constant Proportion", title = "Heterogeneity in Effect Size")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal() +
  theme(axis.text.x = element_text(),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.91, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
int_ES_f

ggsave("./Plots/int_ES_finite_abs.pdf", int_ES_f, height = 6, width = 10)

# Intercept + prop. varies
int_prop_f <- res_f %>% 
  filter(d_ES == `ES_d = ES_v`) %>% 
  mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(format(round(`p_d = p_v`, 3), nsmall = 2)), y= v_c_mean, col = Model))+ 
  geom_boxplot(outlier.size = 0.5, lwd = 0.4)+
  geom_hline(yintercept = 0, col= my_palette[2] )+
  facet_grid( `ES_d = ES_v` ~ `v_p - d_p`, labeller = labeller(`ES_d = ES_v` =label_value, diff_p = label_value))+
  labs(x = expression(Derivation~Proportion~(p[d])), y= "Calibration-in-the-large \n Coefficient",
       tag = "Constant Effect Size", title = "Heterogeneity in Proportion")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  theme(axis.text.x = element_text( angle = -90),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.91, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
int_prop_f
ggsave("./Plots/int_prop_finite_abs.pdf", int_prop_f, height = 6, width = 10)


# Slope + ES varies
slope_ES_f <- res_f %>%
  filter(d_p == `p_d = p_v`) %>% 
  mutate(`ES_d = ES_v` == round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(round(`ES_d = ES_v`, 2)), y= v_c_slope, col = Model))+ 
  geom_boxplot(outlier.size = 0.5, lwd = 0.4)+
  facet_grid( `p_d = p_v`  ~ `v_ES - d_ES`, labeller = labeller(`p_d = p_v` = label_value, `v_ES - d_ES` = label_value))+
  geom_hline(yintercept = 1 , col= my_palette[2] )+
  labs(x = expression(Validation~Effect~Size~(ES[v])), y= "Calibration Slope",
       tag = "Constant Proportion", title = "Heterogeneity in Effect Size")+
  scale_color_manual(values = my_palette[c(3,4)])+
  #ylim(0.8,1.2)+
  theme_minimal()+
  theme(axis.text.x = element_text(),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.91, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
slope_ES_f
ggsave("./Plots/slope_ES_finite_abs.pdf", slope_ES_f, height = 6, width = 10)

# Slope + prop. varies
slope_prop_f <- res_f %>% 
  filter(d_ES == `ES_d = ES_v`) %>%
  mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(format(round(`p_d = p_v`, 3), nsmall = 2)), y= v_c_slope, col = Model))+ 
  geom_boxplot(outlier.size = 0.5, lwd = 0.4)+
  geom_hline(yintercept = 1, col= my_palette[2] )+
  facet_grid( `ES_d = ES_v` ~ `v_p - d_p`, labeller = labeller(`ES_d = ES_v` =label_value, diff_p = label_value))+
  labs(x = expression(Derivation~Proportion~(p[d])), y= "Calibration Slope",
       tag = "Effect Size", title = "Heterogeneity in Proportion")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  #ylim(0.8,1.2)+
  theme(axis.text.x = element_text(angle = -90),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.91, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
slope_prop_f

ggsave("./Plots/slope_prop_finite_abs.pdf", slope_prop_f, height = 6, width = 10)


# c-statistic +ES varies
auc_ES_f <- res_f %>% 
  filter(d_p == `p_d = p_v`) %>% 
  mutate(`ES_d = ES_v` == round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(round(`ES_d = ES_v`, 2)), y= v_AUC, col = Model))+ 
  geom_boxplot(outlier.size = 0.5, lwd = 0.4)+
  facet_grid( `p_d = p_v`  ~ `v_ES - d_ES`, labeller = labeller(`p_d = p_v` = label_value, `v_ES - d_ES` = label_value))+
  geom_hline(yintercept = 0.8 , col= my_palette[2] )+
  labs(x = expression(Validation~Effect~Size~(ES[v])), y= "C-statistic",
       tag = "Constant Proportion", title = "Heterogeneity in Effect Size")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  theme(axis.text.x = element_text(),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.91, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
auc_ES_f
ggsave("./Plots/auc_ES_finite_abs.pdf", auc_ES_f,height = 6, width = 10)

# c-statistic +prop. varies
auc_prop_f <- res_f %>% 
  filter(d_ES == `ES_d = ES_v`) %>%
  mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>%
  ggplot(aes(x = as.factor(format(round(`p_d = p_v`, 3), nsmall = 2)), y= v_AUC, col = Model))+ 
  geom_boxplot(outlier.size = 0.5, lwd = 0.4)+
  geom_hline(yintercept = 0.8, col= my_palette[2] )+
  facet_grid( `ES_d = ES_v` ~ `v_p - d_p`, labeller = labeller(`ES_d = ES_v` =label_value, `v_p - d_p` = label_value))+
  labs(x = expression(Validation~Proportion~(p[v])), y= "C-statistic",
       tag = "Constant Effect Size", title = "Heterogeneity in Proportion")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = -90),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.91, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top"
  )
auc_prop_f

ggsave("./Plots/auc_prop_finite_abs.pdf", auc_prop_f, height = 6, width = 10)

# Brier + ES varies
brier_ES_f <- res_f %>% 
  filter(d_p == `p_d = p_v`) %>% 
  mutate(`ES_d = ES_v` == round(`ES_d = ES_v`, 2)) %>% 
  ggplot(aes(x = as.factor(round(`ES_d = ES_v`, 2)), y= v_Brier, col = Model))+ 
  geom_boxplot(outlier.size = 0.5, lwd = 0.4)+
  facet_grid( `p_d = p_v`  ~ `v_ES - d_ES`, labeller = labeller(`p_d = p_v` = label_value, `v_ES - d_ES` = label_value))+
  #geom_hline(yintercept = 1 , col= my_palette[2] )+
  labs(x = expression(Validation~Effect~Size~(ES[v])), y= "Scaled Brier score",
       tag = "Constant Proportion", title = "Heterogeneity in Effect Size")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  theme(axis.text.x = element_text(),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.91, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top",
        axis.title.y = element_text(size = 12)
  )
brier_ES_f
ggsave("./Plots/brier_ES_finite_abs.pdf", brier_ES_f,height = 6, width = 10)


# Brier +prop. varies
brier_prop_f <- res_f %>% 
  filter(d_ES == `ES_d = ES_v`) %>%
  mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>%
  ggplot(aes(x = as.factor(format(round(`p_d = p_v`, 3), nsmall = 2)), y= v_Brier, col = Model))+ 
  geom_boxplot(outlier.size = 0.5, lwd = 0.4)+
  #geom_hline(yintercept = 1, col= my_palette[2] )+
  facet_grid( `ES_d = ES_v` ~ `v_p - d_p`, labeller = labeller(`ES_d = ES_v` =label_value, `v_p - d_p` = label_value))+
  labs(x = expression(Validation~Proportion~(p[v])), y= "Scaled Brier score",
       tag = "Constant Effect Size", title = "Heterogeneity in Proportion")+
  scale_color_manual(values = my_palette[c(3,4)])+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = -90),
        plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 0, vjust = 0.58),
        plot.tag.position = c(0.91, 0.5),
        plot.tag = element_text(angle = 270),
        panel.spacing = unit(1, "lines"),
        legend.justification =  "top",
        axis.title.y = element_text(size = 12)
  )
brier_prop_f

ggsave("./Plots/brier_prop_finite_abs.pdf", brier_prop_f, height = 6, width = 10) 




