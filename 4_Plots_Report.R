
# CREATING PLOTS DISPLAYED IN THE MAIN TEXT OF THE REPORT

#------------------------------------------------------------------------------#
# Loading packages
#------------------------------------------------------------------------------#



library(magrittr) #for pipes 
library(dplyr) #for summarize
library(ggplot2) #for plots
library(grid) #for strip 
library(gtable) #for strip
library(patchwork) #for combining plots into one


load("./Results/n_dist_scen_finite.RData")
load("./Results/res_sim_finite.RData")


load("./Results/res_sim_large.RData")
load("./Results/n_dist_scen_large.RData")

#Preparation COLOR
my_palette <- c(rgb(1,205/255,0), rgb(192/255,10/255,53/255), rgb(0,0,0), 
                rgb(35/255,109/255, 182/255), rgb(60/255, 150/255,50/255))

#color scheme (RGB)
# UU orange 255/205/0
# UU red 192/10/53
# UU black 0/0/0
# UMC blue 35/109/182
# green 60/150/50



load("./Results/res_large_processed.RData")
load("./Results/res_finite_processed.RData")


#res_plot input: type_ - "ES" if heterogeneity in Effect size
#                        "p" if heterogeneity in proportion
#                study = 1 or 2
#                measure =  "Int", "Slope"
res_plot <- function(type, study, measure){
  #browser()
  my_color <- my_palette[2] 
  if(measure == "Int"){
    meas <- "v_c_mean"
    y_lab <- "Calibration-in-the-large\n Coefficient"
    y_int <- 0
    add_y_lim <- c(-0.1, 0.1)
    
   
  }
  
  if(measure == "Slope"){
    meas <- "v_c_slope"
    y_lab <- "Calibration Slope"
    y_int <- 1
    add_y_lim <- c(-0.01, 0.01)
  }
  
  
  #####################
  #      STUDY 1      #
  #####################
  if(study == 1){
    if(type == "ES"){
      ######  ES  #######
      y_lim <- res_l %>% 
        filter(d_p == `p_d = p_v`) %>%
        filter(d_p == 0.50) %>%  #only middle row
        select(!!sym(meas)) %>% 
        range()
      y_lim <- y_lim + add_y_lim
      diff_vec <- as.numeric(levels(res_l$`v_ES - d_ES`))
      het_vec <- c(rep("Decrease", (length(diff_vec)-1)/2), "Homogeneity", rep("Increase",(length(diff_vec)-1)/2))
     
      
        
    #First facet
    i <- 1
        my_plot <- res_l %>% 
          filter(d_p == `p_d = p_v`) %>%
          filter(d_p == 0.50) %>%  #only middle row
          filter(`v_ES - d_ES` == diff_vec[i]) %>% 
          mutate(`ES_d = ES_v` == round(`ES_d = ES_v`, 2)) %>% 
          ggplot(aes(x = round(`ES_d = ES_v`, 2), y= !!sym(meas), col = Model))+ 
          geom_point(size = 2.5)+
          scale_x_continuous(expression(Validation~Effect~Size~(ES[v])), 
                             breaks = c(0.2, 0.5, 0.8), 
                             limits = c(0.15, 0.85),
                             sec.axis = sec_axis(~ . - (-0.6) ,
                                                 name = expression(Derivation~Effect~Size~(ES[d])),
                                                breaks = c(0.2, 0.5, 0.8)))+
          #facet_grid( ~ label_ES_diff + `v_ES - d_ES`, labeller = labeller(diff_ES = label_value))+
          geom_hline(yintercept = y_int, col= my_color )+
          #annotate("label", x = 0.5 , y = 0.45, label = paste(diff))+
          ylim(y_lim)+
          labs(x = expression(Validation~Effect~Size~(ES[v])), y= y_lab,
               title = paste(het_vec[i], "\n",(-0.6)))+#,
          #subtitle = expression(paste(p[d]," = ",p[v]," = 0.50"))
          scale_color_manual(values = my_palette[c(3,4)])+
          theme_minimal()+
          theme(axis.text.x = element_text( ),
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18),
                plot.margin = unit(c(0,0,0,0), "cm")
                #strip.placement = "outside"###
          )
        if(i > 1){
          my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
        }
        
        if(i < length(diff_vec)){
          my_plot <- my_plot + theme(legend.position = "none",
                                     plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        }
        if(i == ceiling(length(diff_vec)/2)){
          my_plot <- my_plot + theme(axis.title.x = element_text())
        }
        #save(my_plot ,file = paste0("./Plots/try_p_", i,".RData"))
        assign(paste0("p_", i), my_plot) 
        
    #Second facet    
        i <- 2
        
        my_plot <- res_l %>% 
          filter(d_p == `p_d = p_v`) %>%
          filter(d_p == 0.50) %>%  #only middle row
          filter(`v_ES - d_ES` == diff_vec[i]) %>% 
          mutate(`ES_d = ES_v` == round(`ES_d = ES_v`, 2)) %>% 
          ggplot(aes(x = round(`ES_d = ES_v`, 2), y= !!sym(meas), col = Model))+ 
          geom_point(size = 2.5)+
          scale_x_continuous(expression(Validation~Effect~Size~(ES[v])), 
                             breaks = c(0.2, 0.5, 0.8), 
                             limits = c(0.15, 0.85),
                             sec.axis = sec_axis(~ . - (-0.3) ,
                                                 name = expression(Derivation~Effect~Size~(ES[d])),
                                                 breaks = c(0.2, 0.5, 0.8)))+
          #facet_grid( ~ label_ES_diff + `v_ES - d_ES`, labeller = labeller(diff_ES = label_value))+
          geom_hline(yintercept = y_int, col= my_color )+
          #annotate("label", x = 0.5 , y = 0.45, label = paste(diff))+
          ylim(y_lim)+
          labs(x = expression(Validation~Effect~Size~(ES[v])), y= y_lab,
               title = paste(het_vec[i], "\n",(-0.3)))+#,
          #subtitle = expression(paste(p[d]," = ",p[v]," = 0.50"))
          scale_color_manual(values = my_palette[c(3,4)])+
          theme_minimal()+
          theme(axis.text.x = element_text( ),
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18),
                plot.margin = unit(c(0,0,0,0), "cm")
                #strip.placement = "outside"###
          )
        if(i > 1){
          my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
        }
        
        if(i < length(diff_vec)){
          my_plot <- my_plot + theme(legend.position = "none",
                                     plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        }
        if(i == ceiling(length(diff_vec)/2)){
          my_plot <- my_plot + theme(axis.title.x = element_text())
          #}
        
        }
        assign(paste0("p_", i), my_plot)
        
      
        #Third facet    
        i <- 3
        
        my_plot <- res_l %>% 
          filter(d_p == `p_d = p_v`) %>%
          filter(d_p == 0.50) %>%  #only middle row
          filter(`v_ES - d_ES` == diff_vec[i]) %>% 
          mutate(`ES_d = ES_v` == round(`ES_d = ES_v`, 2)) %>% 
          ggplot(aes(x = round(`ES_d = ES_v`, 2), y= !!sym(meas), col = Model))+ 
          geom_point(size = 2.5)+
          scale_x_continuous(expression(Validation~Effect~Size~(ES[v])), 
                             breaks = c(0.2, 0.5, 0.8), 
                             limits = c(0.15, 0.85),
                             sec.axis = sec_axis(~ . - (0) ,
                                                 name = expression(Derivation~Effect~Size~(ES[d])),
                                                 breaks = c(0.2, 0.5, 0.8)))+
          #facet_grid( ~ label_ES_diff + `v_ES - d_ES`, labeller = labeller(diff_ES = label_value))+
          geom_hline(yintercept = y_int, col= my_color )+
          #annotate("label", x = 0.5 , y = 0.45, label = paste(diff))+
          ylim(y_lim)+
          labs(x = expression(Validation~Effect~Size~(ES[v])), y= y_lab,
               title = paste(het_vec[i], "\n",(0)))+#,
          #subtitle = expression(paste(p[d]," = ",p[v]," = 0.50"))
          scale_color_manual(values = my_palette[c(3,4)])+
          theme_minimal()+
          theme(axis.text.x = element_text(),
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18),
                plot.margin = unit(c(0,0,0,0), "cm")
                #strip.placement = "outside"###
          )
        if(i > 1){
          my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
        }
        
        if(i < length(diff_vec)){
          my_plot <- my_plot + theme(legend.position = "none",
                                     plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        }
        if(i == ceiling(length(diff_vec)/2)){
          my_plot <- my_plot + theme(axis.title.x = element_text())
          #}
          
        }
        assign(paste0("p_", i), my_plot)
        
    
      
        
        #Fourth facet    
        i <- 4
        
        my_plot <- res_l %>% 
          filter(d_p == `p_d = p_v`) %>%
          filter(d_p == 0.50) %>%  #only middle row
          filter(`v_ES - d_ES` == diff_vec[i]) %>% 
          mutate(`ES_d = ES_v` == round(`ES_d = ES_v`, 2)) %>% 
          ggplot(aes(x = round(`ES_d = ES_v`, 2), y= !!sym(meas), col = Model))+ 
          geom_point(size = 2.5)+
          scale_x_continuous(expression(Validation~Effect~Size~(ES[v])), 
                             breaks = c(0.2, 0.5, 0.8), 
                             limits = c(0.15, 0.85),
                             sec.axis = sec_axis(~ . - (0.3) ,
                                                 name = expression(Derivation~Effect~Size~(ES[d])),
                                                 breaks = c(0.2, 0.5, 0.8)))+
          #facet_grid( ~ label_ES_diff + `v_ES - d_ES`, labeller = labeller(diff_ES = label_value))+
          geom_hline(yintercept = y_int, col= my_color )+
          #annotate("label", x = 0.5 , y = 0.45, label = paste(diff))+
          ylim(y_lim)+
          labs(x = expression(Validation~Effect~Size~(ES[v])), y= y_lab,
               title = paste(het_vec[i], "\n",(0.3)))+#,
          #subtitle = expression(paste(p[d]," = ",p[v]," = 0.50"))
          scale_color_manual(values = my_palette[c(3,4)])+
          theme_minimal()+
          theme(axis.text.x = element_text( ),
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18),
                plot.margin = unit(c(0,0,0,0), "cm")
                #strip.placement = "outside"###
          )
        if(i > 1){
          my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
        }
        
        if(i < length(diff_vec)){
          my_plot <- my_plot + theme(legend.position = "none",
                                     plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        }
        if(i == ceiling(length(diff_vec)/2)){
          my_plot <- my_plot + theme(axis.title.x = element_text())
          #}
          
        }
        assign(paste0("p_", i), my_plot)
        
        
        #Fifth facet    
        i <- 5
        
        my_plot <- res_l %>% 
          filter(d_p == `p_d = p_v`) %>%
          filter(d_p == 0.50) %>%  #only middle row
          filter(`v_ES - d_ES` == diff_vec[i]) %>% 
          mutate(`ES_d = ES_v` == round(`ES_d = ES_v`, 2)) %>% 
          ggplot(aes(x = round(`ES_d = ES_v`, 2), y= !!sym(meas), col = Model))+ 
          geom_point(size = 2.5)+
          scale_x_continuous(expression(Validation~Effect~Size~(ES[v])), 
                             breaks = c(0.2, 0.5, 0.8), 
                             limits = c(0.15, 0.85),
                             sec.axis = sec_axis(~ . - (0.6) ,
                                                 name = expression(Derivation~Effect~Size~(ES[d])),
                                                 breaks = c(0.2, 0.5, 0.8)))+
          #facet_grid( ~ label_ES_diff + `v_ES - d_ES`, labeller = labeller(diff_ES = label_value))+
          geom_hline(yintercept = y_int, col= my_color )+
          #annotate("label", x = 0.5 , y = 0.45, label = paste(diff))+
          ylim(y_lim)+
          labs(x = expression(Validation~Effect~Size~(ES[v])), y= y_lab,
               title = paste(het_vec[i], "\n",(0.6)))+#,
          #subtitle = expression(paste(p[d]," = ",p[v]," = 0.50"))
          scale_color_manual(values = my_palette[c(3,4)])+
          theme_minimal()+
          theme(axis.text.x = element_text(),
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18),
                plot.margin = unit(c(0,0,0,0), "cm")
                #strip.placement = "outside"###
          )
        if(i > 1){
          my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
        }
        
        if(i < length(diff_vec)){
  
          my_plot <- my_plot + theme(legend.position = "none",
                                     plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
          
        }
        if(i == ceiling(length(diff_vec)/2)){
          my_plot <- my_plot + theme(axis.title.x = element_text())
          #}
          
        }
        assign(paste0("p_", i), my_plot)
        
        
      p_patch <- p_1 + p_2 + p_3 + p_4 + p_5 + plot_layout(design =
                                                 "12345")
     wrapped_plot <- wrap_elements(panel = p_patch) +
       labs(
          #tag = expression(Validation~Effect~Size~(ES[v])),
            #subtitle = expression(Derivation~Effect~Size~(ES[d])), #) +#,
            title = "Effect size" )+
              #expression(paste(ES[v]~-~ES[d])))+
                           #paste("-0.6\;\;-0.3 0   0.3   0.6")
       theme(
         #plot.tag = element_text(size = rel(1), vjust = -1),
         #plot.tag.position = "bottom",
         #plot.subtitle = element_text(hjust = 0.5),
         plot.title = element_text(hjust = 0.495, size = 20,
                                   face = "bold")
       )
    
    }else{
      ######  p  #######
      y_lim <- res_l %>% 
        filter(d_ES == `ES_d = ES_v`) %>% 
        filter(d_ES == 0.5008) %>%  #only middle row
        select(!!sym(meas)) %>% 
        range()
      y_lim <- y_lim + add_y_lim
      diff_vec <- as.numeric(levels(res_l$`v_p - d_p`))
      het_vec <- c(rep("Decrease", (length(diff_vec)-1)/2), "Homogeneity", rep("Increase",(length(diff_vec)-1)/2))
      
      i <- 1
      my_plot <-  res_l %>% 
        filter(d_ES == `ES_d = ES_v`) %>% 
        filter(d_ES == 0.5008) %>%  #only middle row
        filter(`v_p - d_p` == diff_vec[i]) %>%
        mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = `p_d = p_v`, y= !!sym(meas), col = Model ))+ 
        geom_point(size = 2.5)+
        geom_hline(yintercept = y_int, col= my_color )+
        #facet_grid(  ~ label_p_diff + `v_p - d_p`, labeller = labeller( diff_p = label_value))+
        #label_ES_equal + `ES_d = ES_v`   + `ES_d = ES_v` =label_value,
        scale_x_continuous(expression(Validation~Proportion~(p[v])), 
                           breaks = c(0.1, 0.5, 0.9), 
                           limits = c(0.05, 0.95),
                           sec.axis = sec_axis(~ . - (-0.8) ,
                                               name = expression(Derivation~Proportion~(p[d])),
                                               breaks = c(0.1, 0.5, 0.9)))+
        labs(x = expression(Validation~Proportion~(p[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(-0.8)))+#,
        #subtitle = expression(paste(ES[d]," = ",ES[v]," = -0.50"))
        ylim(y_lim)+
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal()+
        theme(axis.text.x = element_text(),
            text = element_text(size = 18),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 18),
            plot.margin = unit(c(0,0,0,0), "cm")
            #strip.placement = "outside"###
      )
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      }
      
      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        #}
        
      }
      assign(paste0("p_", i), my_plot)
      
      
      
      i <- 2
      my_plot <-  res_l %>% 
        filter(d_ES == `ES_d = ES_v`) %>% 
        filter(d_ES == 0.5008) %>%  #only middle row
        filter(`v_p - d_p` == diff_vec[i]) %>%
        mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = `p_d = p_v`, y= !!sym(meas), col = Model ))+ 
        geom_point(size = 2.5)+
        geom_hline(yintercept = y_int, col= my_color )+
        #facet_grid(  ~ label_p_diff + `v_p - d_p`, labeller = labeller( diff_p = label_value))+
        #label_ES_equal + `ES_d = ES_v`   + `ES_d = ES_v` =label_value,
        scale_x_continuous(expression(Validation~Proportion~(p[v])), 
                           breaks = c(0.1, 0.5, 0.9), 
                           limits = c(0.05, 0.95),
                           sec.axis = sec_axis(~ . - (-0.4) ,
                                               name = expression(Derivation~Proportion~(p[d])),
                                               breaks = c(0.1, 0.5, 0.9)))+
        labs(x = expression(Validation~Proportion~(p[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(-0.4)))+#,
        #subtitle = expression(paste(ES[d]," = ",ES[v]," = -0.50"))
        ylim(y_lim)+
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal()+
        theme(axis.text.x = element_text(),
              text = element_text(size = 18),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18),
              plot.margin = unit(c(0,0,0,0), "cm")
              #strip.placement = "outside"###
        )
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      }
      
      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        #}
        
      }
      assign(paste0("p_", i), my_plot)
      
      
      
      
      i <- 3
      my_plot <-  res_l %>% 
        filter(d_ES == `ES_d = ES_v`) %>% 
        filter(d_ES == 0.5008) %>%  #only middle row
        filter(`v_p - d_p` == diff_vec[i]) %>%
        mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = `p_d = p_v`, y= !!sym(meas), col = Model ))+ 
        geom_point(size = 2.5)+
        geom_hline(yintercept = y_int, col= my_color )+
        #facet_grid(  ~ label_p_diff + `v_p - d_p`, labeller = labeller( diff_p = label_value))+
        #label_ES_equal + `ES_d = ES_v`   + `ES_d = ES_v` =label_value,
        scale_x_continuous(expression(Validation~Proportion~(p[v])), 
                           breaks = c(0.1, 0.5, 0.9), 
                           limits = c(0.05, 0.95),
                           sec.axis = sec_axis(~ . - (0) ,
                                               name = expression(Derivation~Proportion~(p[d])),
                                               breaks = c(0.1, 0.5, 0.9)))+
        labs(x = expression(Validation~Proportion~(p[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(0)))+#,
        #subtitle = expression(paste(ES[d]," = ",ES[v]," = -0.50"))
        ylim(y_lim)+
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal()+
        theme(axis.text.x = element_text(),
              text = element_text(size = 18),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18),
              plot.margin = unit(c(0,0,0,0), "cm")
              #strip.placement = "outside"###
        )
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      }
      
      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        #}
        
      }
      assign(paste0("p_", i), my_plot)
      
      
      
      i <- 4
      my_plot <-  res_l %>% 
        filter(d_ES == `ES_d = ES_v`) %>% 
        filter(d_ES == 0.5008) %>%  #only middle row
        filter(`v_p - d_p` == diff_vec[i]) %>%
        mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = `p_d = p_v`, y= !!sym(meas), col = Model ))+ 
        geom_point(size = 2.5)+
        geom_hline(yintercept = y_int, col= my_color )+
        #facet_grid(  ~ label_p_diff + `v_p - d_p`, labeller = labeller( diff_p = label_value))+
        #label_ES_equal + `ES_d = ES_v`   + `ES_d = ES_v` =label_value,
        ylim(y_lim)+
        scale_x_continuous(expression(Validation~Proportion~(p[v])), 
                           breaks = c(0.1, 0.5, 0.9), 
                           limits = c(0.05, 0.95),
                           sec.axis = sec_axis(~ . - (0.4) ,
                                               name = expression(Derivation~Proportion~(p[d])),
                                               breaks = c(0.1, 0.5, 0.9)))+
        labs(x = expression(Validation~Proportion~(p[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(0.4)))+#,
        #subtitle = expression(paste(ES[d]," = ",ES[v]," = -0.50"))
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal()+
        theme(axis.text.x = element_text(),
              text = element_text(size = 18),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18),
              plot.margin = unit(c(0,0,0,0), "cm")
              #strip.placement = "outside"###
        )
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      }
      
      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        #}
        
      }
      assign(paste0("p_", i), my_plot)
      
      
      
      i <- 5
      my_plot <-  res_l %>% 
        filter(d_ES == `ES_d = ES_v`) %>% 
        filter(d_ES == 0.5008) %>%  #only middle row
        filter(`v_p - d_p` == diff_vec[i]) %>%
        mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = `p_d = p_v`, y= !!sym(meas), col = Model ))+ 
        geom_point(size = 2.5)+
        geom_hline(yintercept = y_int, col= my_color )+
        #facet_grid(  ~ label_p_diff + `v_p - d_p`, labeller = labeller( diff_p = label_value))+
        #label_ES_equal + `ES_d = ES_v`   + `ES_d = ES_v` =label_value,
        scale_x_continuous(expression(Validation~Proportion~(p[v])), 
                           breaks = c(0.1, 0.5, 0.9), 
                           limits = c(0.05, 0.95),
                           sec.axis = sec_axis(~ . - (0.8) ,
                                               name = expression(Derivation~Proportion~(p[d])),
                                               breaks = c(0.1, 0.5, 0.9)))+
        labs(x = expression(Validation~Proportion~(p[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(0.8)))+#,
        ylim(y_lim)+
        #subtitle = expression(paste(ES[d]," = ",ES[v]," = -0.50"))
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal()+
        theme(axis.text.x = element_text(),
              text = element_text(size = 18),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18),
              plot.margin = unit(c(0,0,0,0), "cm")
              #strip.placement = "outside"###
        )
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      }
      
      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        #}
        
      }
      assign(paste0("p_", i), my_plot)
      
      
      
      p_patch <- p_1 + p_2 + p_3 + p_4 + p_5 + plot_layout(design =
                                                             "12345")
      wrapped_plot <- wrap_elements(panel = p_patch) +
        labs(
          #tag = expression(Validation~Effect~Size~(ES[v])),
          #subtitle = expression(Derivation~Effect~Size~(ES[d])), #) +#,
          title = "Proportion" )+
        #expression(paste(ES[v]~-~ES[d])))+
        #paste("-0.6\;\;-0.3 0   0.3   0.6")
        theme(
          #plot.tag = element_text(size = rel(1), vjust = -1),
          #plot.tag.position = "bottom",
          #plot.subtitle = element_text(hjust = 0.5),
          plot.title = element_text(hjust = 0.495, size = 20,
                                    face = "bold")
        )
      
    }
  }
 #####################
 #      STUDY 2      #
 #####################
 
  if(study == 2){
    ######  ES  #######
    if(type == "ES"){
      y_lim <- res_f %>% 
        filter(d_p == `p_d = p_v`) %>%
        filter(d_p == 0.50) %>%  #only middle row
        select(!!sym(meas)) %>% 
        range()
      y_lim <- y_lim + add_y_lim
      diff_vec <- as.numeric(levels(res_f$`v_ES - d_ES`))
      het_vec <- c(rep("Decrease", (length(diff_vec)-1)/2), "Homogeneity", rep("Increase",(length(diff_vec)-1)/2))
      my_limits <- c(-0.05, 1.1)
      my_width <- diff(my_limits) * 0.2
      
      
      i <- 1
      my_plot <- res_f %>% 
        filter(d_p == `p_d = p_v`) %>% 
        filter(d_p == 0.50) %>% # only middle row
        filter(`v_ES - d_ES` == as.numeric(levels(res_f$`v_ES - d_ES`))[i]) %>% 
        mutate(`ES_d = ES_v` == round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = round(`ES_d = ES_v`, 2), y= !!sym(meas), col = Model, group = interaction(`ES_d = ES_v`, Model)))+ 
        geom_boxplot(outlier.size = 1.2, lwd = 0.4, width = my_width , position = position_dodge2(preserve = "single"))+
        #facet_grid(~ label_ES_diff + `v_ES - d_ES`, labeller = labeller(`v_ES - d_ES` = label_value))+ #label_p_equal + `p_d = p_v`  ;`p_d = p_v` = label_value, 
        scale_x_continuous(expression(Validation~Effect~Size~(ES[v])), 
                           breaks = c(0.2, 0.5, 0.8), 
                           limits = my_limits,
                           sec.axis = sec_axis(~ . - (-0.3) ,
                                               name = expression(Derivation~Effect~Size~(ES[d])),
                                               breaks = c(0.2, 0.5, 0.8)))+
        geom_hline(yintercept = y_int, col= my_color )+
        labs(x = expression(Validation~Effect~Size~(ES[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(-0.3)))+ 
        ylim(y_lim)+
        #subtitle = expression(paste(p[d]," = ",p[v]," = 0.50"))
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal() +
        theme(axis.text.x = element_text( ),
              text = element_text(size = 18),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18),
              plot.margin = unit(c(0,0,0,0), "cm"))
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
        }

      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        #}
        
      }
      assign(paste0("p_", i), my_plot)
      
      
      i <- 2
      my_plot <- res_f %>% 
        filter(d_p == `p_d = p_v`) %>% 
        filter(d_p == 0.50) %>% # only middle row
        filter(`v_ES - d_ES` == as.numeric(levels(res_f$`v_ES - d_ES`))[i]) %>% 
        mutate(`ES_d = ES_v` == round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = round(`ES_d = ES_v`, 2), y= !!sym(meas), col = Model, group = interaction(`ES_d = ES_v`, Model)))+ 
        geom_boxplot(outlier.size = 1.2, lwd = 0.4, width = my_width , position = position_dodge2(preserve = "single"))+
        #facet_grid(~ label_ES_diff + `v_ES - d_ES`, labeller = labeller(`v_ES - d_ES` = label_value))+ #label_p_equal + `p_d = p_v`  ;`p_d = p_v` = label_value, 
        scale_x_continuous(expression(Validation~Effect~Size~(ES[v])), 
                           breaks = c(0.2, 0.5, 0.8), 
                           limits = my_limits,
                           sec.axis = sec_axis(~ . - (0) ,
                                               name = expression(Derivation~Effect~Size~(ES[d])),
                                               breaks = c(0.2, 0.5, 0.8)))+
        geom_hline(yintercept = y_int, col= my_color )+
        labs(x = expression(Validation~Effect~Size~(ES[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(0)))+
        ylim(y_lim)+
        #subtitle = expression(paste(p[d]," = ",p[v]," = 0.50"))
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal() +
        theme(axis.text.x = element_text( ),
              text = element_text(size = 18),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18),
              plot.margin = unit(c(0,0,0,0), "cm"))
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      }
      
      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        #}
        
      }
      assign(paste0("p_", i), my_plot)
      
      
      
      i <- 3
      my_plot <- res_f %>% 
        filter(d_p == `p_d = p_v`) %>% 
        filter(d_p == 0.50) %>% # only middle row
        filter(`v_ES - d_ES` == as.numeric(levels(res_f$`v_ES - d_ES`))[i]) %>% 
        mutate(`ES_d = ES_v` == round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = round(`ES_d = ES_v`, 2), y= !!sym(meas), col = Model, group = interaction(`ES_d = ES_v`, Model)))+ 
        geom_boxplot(outlier.size = 1.2, lwd = 0.4, width = my_width , position = position_dodge2(preserve = "single"))+
        #facet_grid(~ label_ES_diff + `v_ES - d_ES`, labeller = labeller(`v_ES - d_ES` = label_value))+ #label_p_equal + `p_d = p_v`  ;`p_d = p_v` = label_value, 
        scale_x_continuous(expression(Validation~Effect~Size~(ES[v])), 
                           breaks = c(0.2, 0.5, 0.8), 
                           limits = my_limits,
                           sec.axis = sec_axis(~ . - (0.3) ,
                                               name = expression(Derivation~Effect~Size~(ES[d])),
                                               breaks = c(0.2, 0.5, 0.8)))+
        geom_hline(yintercept = y_int, col= my_color )+
        labs(x = expression(Validation~Effect~Size~(ES[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(0.3)))+ #, 
        ylim(y_lim)+
        #subtitle = expression(paste(p[d]," = ",p[v]," = 0.50"))
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal() +
        theme(axis.text.x = element_text( ),
              text = element_text(size =18),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18),
              plot.margin = unit(c(0,0,0,0), "cm"))
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      }
      
      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        
        
      }
      assign(paste0("p_", i), my_plot)
      
      p_patch <- p_1 + p_2 + p_3 + plot_layout(design =
                                                             "123")
      wrapped_plot <- wrap_elements(panel = p_patch) +
        labs(
          #tag = expression(Validation~Effect~Size~(ES[v])),
          #subtitle = expression(Derivation~Effect~Size~(ES[d])), #) +#,
          title = "Effect size" )+
        #expression(paste(ES[v]~-~ES[d])))+
        #paste("-0.6\;\;-0.3 0   0.3   0.6")
        theme(
          #plot.tag = element_text(size = rel(1), vjust = -1),
          #plot.tag.position = "bottom",
          #plot.subtitle = element_text(hjust = 0.5),
          plot.title = element_text(hjust = 0.49, size = 18,
                                    face = "bold")
        )
      
    }else{
      
      
      ######  p  #######
      y_lim <- res_f %>% 
        filter(d_ES == `ES_d = ES_v`) %>% 
        filter(d_ES == 0.5008) %>%  #only middle row
        select(!!sym(meas)) %>% 
        range()
      y_lim <- y_lim + add_y_lim
    
      diff_vec <- as.numeric(levels(res_f$`v_p - d_p`))
      het_vec <- c(rep("Decrease", (length(diff_vec)-1)/2), "Homogeneity", rep("Increase",(length(diff_vec)-1)/2))
      my_limits <- c(-0.05, 1.1)
      my_width <- diff(my_limits) * 0.2
      
      i <- 1
      my_plot <-  res_f %>% 
        filter(d_ES == `ES_d = ES_v`) %>% 
        filter(d_ES == 0.5008) %>%  #only middle row
        filter(`v_p - d_p` == diff_vec[i]) %>%
        mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = `p_d = p_v`, y= !!sym(meas), col = Model, group = interaction(`p_d = p_v`, Model) ))+ 
        geom_boxplot(outlier.size = 1.2, lwd = 0.4, width = my_width , position = position_dodge2(preserve = "single"))+
        geom_hline(yintercept = y_int, col= my_color )+
        #facet_grid(  ~ label_p_diff + `v_p - d_p`, labeller = labeller( diff_p = label_value))+
        #label_ES_equal + `ES_d = ES_v`   + `ES_d = ES_v` =label_value,
        scale_x_continuous(expression(Validation~Proportion~(p[v])), 
                           breaks = c(0.1, 0.5,  0.9), 
                           limits = my_limits,
                           sec.axis = sec_axis(~ . - (-0.4) ,
                                               name = expression(Derivation~Proportion~(p[d])),
                                               breaks = c(0.1 ,0.5, 0.9)))+
        labs(x = expression(Validation~Proportion~(p[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(-0.4)))+#,
        ylim(y_lim)+
        #subtitle = expression(paste(ES[d]," = ",ES[v]," = -0.50"))
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal()+
        theme(axis.text.x = element_text(),
              text = element_text(size = 18),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18),
              plot.margin  = margin(5.5,5.5,5.5,6.5, "pt")
              #strip.placement = "outside"###
        )
      my_plot
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      }
      
      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        #}
        
      }
      assign(paste0("p_", i), my_plot)
      
      
      
      
      i <- 2
      my_plot <-  res_f %>% 
        filter(d_ES == `ES_d = ES_v`) %>% 
        filter(d_ES == 0.5008) %>%  #only middle row
        filter(`v_p - d_p` == diff_vec[i]) %>%
        mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = `p_d = p_v`, y= !!sym(meas), col = Model, group = interaction(`p_d = p_v`, Model) ))+ 
        geom_boxplot(outlier.size = 1.2, lwd = 0.4,  width = my_width , position = position_dodge2(preserve = "single"))+
        geom_hline(yintercept = y_int, col= my_color )+
        #facet_grid(  ~ label_p_diff + `v_p - d_p`, labeller = labeller( diff_p = label_value))+
        #label_ES_equal + `ES_d = ES_v`   + `ES_d = ES_v` =label_value,
        scale_x_continuous(expression(Validation~Proportion~(p[v])), 
                           breaks = c(0.1, 0.5,  0.9), 
                           limits = my_limits,
                           sec.axis = sec_axis(~ . - (-0.05) ,
                                               name = expression(Derivation~Proportion~(p[d])),
                                               breaks = c(0.15 ,0.55, 0.95)))+
        labs(x = expression(Validation~Proportion~(p[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(-0.05)))+#,
        ylim(y_lim)+
        #subtitle = expression(paste(ES[d]," = ",ES[v]," = -0.50"))
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal()+
        theme(axis.text.x = element_text(),
              text = element_text(size = 18),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18),
              plot.margin = unit(c(0,0,0,0), "cm")
              #strip.placement = "outside"###
        )
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      }
      
      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        #}
        
      }
      assign(paste0("p_", i), my_plot)
      
      
      
      
      i <- 3
      my_plot <-  res_f %>% 
        filter(d_ES == `ES_d = ES_v`) %>% 
        filter(d_ES == 0.5008) %>%  #only middle row
        filter(`v_p - d_p` == diff_vec[i]) %>%
        mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = `p_d = p_v`, y= !!sym(meas), col = Model, group = interaction(`p_d = p_v`, Model) ))+ 
        geom_boxplot(outlier.size = 1.2, lwd = 0.4, width = my_width , position = position_dodge2(preserve = "single"))+
        geom_hline(yintercept = y_int, col= my_color )+
        #facet_grid(  ~ label_p_diff + `v_p - d_p`, labeller = labeller( diff_p = label_value))+
        #label_ES_equal + `ES_d = ES_v`   + `ES_d = ES_v` =label_value,
        scale_x_continuous(expression(Validation~Proportion~(p[v])), 
                           breaks = c(0.1, 0.5,  0.9), 
                           limits = my_limits,
                           sec.axis = sec_axis(~ . - (0) ,
                                               name = expression(Derivation~Proportion~(p[d])),
                                               breaks = c(0.1 ,0.5, 0.9)))+
        labs(x = expression(Validation~Proportion~(p[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(0)))+#,
        ylim(y_lim)+
        #subtitle = expression(paste(ES[d]," = ",ES[v]," = -0.50"))
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal()+
        theme(axis.text.x = element_text(),
              text = element_text(size = 18),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18),
              plot.margin = unit(c(0,0,0,0), "cm")
              #strip.placement = "outside"###
        )
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      }
      
      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        #}
        
      }
      assign(paste0("p_", i), my_plot)
      
      
      
      
      i <- 4
      my_plot <-  res_f %>% 
        filter(d_ES == `ES_d = ES_v`) %>% 
        filter(d_ES == 0.5008) %>%  #only middle row
        filter(`v_p - d_p` == diff_vec[i]) %>%
        mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = `p_d = p_v`, y= !!sym(meas), col = Model, group = interaction(`p_d = p_v`, Model) ))+ 
        geom_boxplot(outlier.size = 1.2, lwd = 0.4, width = my_width , position = position_dodge2(preserve = "single"))+
        geom_hline(yintercept = y_int, col= my_color )+
        #facet_grid(  ~ label_p_diff + `v_p - d_p`, labeller = labeller( diff_p = label_value))+
        #label_ES_equal + `ES_d = ES_v`   + `ES_d = ES_v` =label_value,
        scale_x_continuous(expression(Validation~Proportion~(p[v])), 
                           breaks = c(0.15, 0.55,  0.95), 
                           limits = my_limits,
                           sec.axis = sec_axis(~ . - (0.05) ,
                                               name = expression(Derivation~Proportion~(p[d])),
                                               breaks = c(0.1 ,0.5, 0.9)))+
        labs(x = expression(Validation~Proportion~(p[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(0.05)))+#,
        ylim(y_lim)+
        #subtitle = expression(paste(ES[d]," = ",ES[v]," = -0.50"))
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal()+
        theme(axis.text.x = element_text(),
              text = element_text(size = 18),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18),
              plot.margin = margin(5.5,5.5,5.5,6.5, "pt")
              #strip.placement = "outside"###
        )
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      }
      
      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        #}
        
      }
      assign(paste0("p_", i), my_plot)
      
      
      
      i <- 5
      my_plot <-  res_f %>% 
        filter(d_ES == `ES_d = ES_v`) %>% 
        filter(d_ES == 0.5008) %>%  #only middle row
        filter(`v_p - d_p` == diff_vec[i]) %>%
        mutate(`ES_d = ES_v` = round(`ES_d = ES_v`, 2)) %>% 
        ggplot(aes(x = `p_d = p_v`, y= !!sym(meas), col = Model, group = interaction(`p_d = p_v`, Model) ))+ 
        geom_boxplot(outlier.size = 1.2, lwd = 0.4,  width = my_width , position = position_dodge2(preserve = "single"))+
        geom_hline(yintercept = y_int, col= my_color )+
        #facet_grid(  ~ label_p_diff + `v_p - d_p`, labeller = labeller( diff_p = label_value))+
        #label_ES_equal + `ES_d = ES_v`   + `ES_d = ES_v` =label_value,
        scale_x_continuous(expression(Validation~Proportion~(p[v])), 
                           breaks = c(0.1, 0.5,  0.9), 
                           limits = my_limits,
                           sec.axis = sec_axis(~ . - (0.4) ,
                                               name = expression(Derivation~Proportion~(p[d])),
                                               breaks = c(0.1 ,0.5, 0.9)))+
        labs(x = expression(Validation~Proportion~(p[v])), y= y_lab,
             title = paste(het_vec[i], "\n",(0.4)))+#,
        ylim(y_lim)+
        #subtitle = expression(paste(ES[d]," = ",ES[v]," = -0.50"))
        scale_color_manual(values = my_palette[c(3,4)])+
        theme_minimal()+
        theme(axis.text.x = element_text(),
              text = element_text(size = 18),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18),
              plot.margin = margin(5.5,5.5,5.5,6.5, "pt")
              #strip.placement = "outside"###
        )
      if(i > 1){
        my_plot <- my_plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      }
      
      if(i < length(diff_vec)){
        
        my_plot <- my_plot + theme(legend.position = "none",
                                   plot.margin = margin(5.5,5.5,5.5,6.5, "pt"))
        
      }
      if(i == ceiling(length(diff_vec)/2)){
        my_plot <- my_plot + theme(axis.title.x = element_text())
        #}
        
      }
      assign(paste0("p_", i), my_plot)
      
      
      
      p_patch <- p_1 + p_2 + p_3 + p_4 + p_5 + plot_layout(design =
                                                             "12345")
      wrapped_plot <- wrap_elements(panel = p_patch) +
        labs(
          #tag = expression(Validation~Effect~Size~(ES[v])),
          #subtitle = expression(Derivation~Effect~Size~(ES[d])), #) +#,
          title = "Proportion" )+
        #expression(paste(ES[v]~-~ES[d])))+
        #paste("-0.6\;\;-0.3 0   0.3   0.6")
        theme(
          #plot.tag = element_text(size = rel(1), vjust = -1),
          #plot.tag.position = "bottom",
          #plot.subtitle = element_text(hjust = 0.5),
          plot.title = element_text(hjust = 0.495, size = 20,
                                    face = "bold")
        )
    }
  }
  plot(wrapped_plot)

}


#Study 1
#Calibration-in-the-large
pdf("./Plots/int_ES_l_val.pdf", width = 10, height = 6)
res_plot(type = "ES", study = 1, measure= "Int") 
dev.off()

pdf("./Plots/int_p_l_val.pdf", width = 10, height = 6)
res_plot(type = "p", study = 1, measure= "Int") 
dev.off()


#Calibration Slope
pdf("./Plots/slope_ES_l_val.pdf", width = 10, height = 6)
res_plot(type = "ES", study = 1, measure= "Slope") 
dev.off()

pdf("./Plots/slope_p_l_val.pdf", width = 10, height = 6)
res_plot(type = "p", study = 1, measure= "Slope") 
dev.off()


#Study 2
#Calibration-in-the-large
pdf("./Plots/int_ES_f_val.pdf", width = 10, height = 6)
res_plot(type = "ES", study = 2, measure= "Int") 
dev.off()


pdf("./Plots/int_p_f_val.pdf", width = 10, height = 6)
res_plot(type = "p", study = 2, measure= "Int") 
dev.off()


#Calibration Slope
pdf("./Plots/slope_ES_f_val.pdf", width = 10, height = 6)
res_plot(type = "ES", study = 2, measure= "Slope") 
dev.off()

pdf("./Plots/slope_p_f_val.pdf", width = 10, height = 6)
res_plot(type = "p", study = 2, measure= "Slope") 
dev.off()

