#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotEcoQualityOfLife <- function(df_economy, output_dir, one_plot) {
  
  name = "Quality_of_life_plot"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  #  ----------------------------
  
  #group_by(tick) %>% summarise(total = mean(count_people_with_not_is_young_and_is_in_poverty))  

  df_QOL <- df_economy %>% select(tick, run_number, Scenario, mean_QOL = mean_quality_of_life_indicator_of_people,
                                  median_QOL = median_quality_of_life_indicator_of_people,
                                  min_QOL = min_quality_of_life_indicator_of_people,
                                  max_QOL = max_quality_of_life_indicator_of_people)
  
  df_QOL_mean_std <- df_QOL %>% group_by(tick, Scenario) %>%
    summarise(tick, Scenario,
              mean = mean(mean_QOL)
              ,std = sd(mean_QOL)
              ,min = mean(min_QOL)
              ,max = mean(max_QOL)
    ) %>% unique()
  
  
  # seg_QOL <- gather(df_QOL, variable, measurement, mean_QOL)
  # plot_ggplot(seg_QOL, "tick", "all")
  # 
  # plot_ggplot_smooth_uncertainty(df_QOL_mean_std, "tick","all")
  # 
  # plot_ggplot_smooth_minmax(df_QOL_mean_std, "tick","all")
  
  # Add days converted from ticks
  df_QOL_mean_std_day <- df_QOL_mean_std
  df_QOL_mean_std_day$day <- dmfConvertTicksToDay(df_QOL_mean_std_day$tick)
  df_QOL_mean_std_day <- df_QOL_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std)
    , min = mean(min)
    , max = mean(max))  %>% unique()
  
  # plot_ggplot_smooth_uncertainty(df_QOL_mean_std_day, "day","all")
  # 
  # plot_ggplot_smooth_minmax(df_QOL_mean_std_day, "day","all")
  
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_QOL, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_QOL_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_QOL_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))

  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  print(paste(name, " making plots", sep=""))
  
  
  #----- ticks --------------------
  seg_QOL <- gather(df_QOL, variable, measurement, mean_QOL)
  
  
  dmfPdfOpen(output_dir, "eco_QoL_mean_tick")
  print(plot_ggplot(seg_QOL, "tick", "all"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_QoL_mean_smooth_uncertainty_tick")
  print(plot_ggplot_smooth_uncertainty(df_QOL_mean_std, "tick","all"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_QoL_mean_smooth_min_max_tick")
  print(plot_ggplot_smooth_minmax(df_QOL_mean_std, "tick","all"))
  dmfPdfClose()
  
  
  #----- days --------------------
  dmfPdfOpen(output_dir, "eco_QoL_mean_smooth_uncertainty_day")
  print(plot_ggplot_smooth_uncertainty(df_QOL_mean_std_day, "day","all"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_QoL_mean_smooth_min_max_day")
  print(plot_ggplot_smooth_minmax(df_QOL_mean_std_day, "day","all"))
  dmfPdfClose()
  
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================



plot_ggplot <- function(data_to_plot, timeframe, type_of_people) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = measurement)) +
    #geom_smooth(aes(col=Scenario), span=0.1, se=FALSE) +
    gl_plot_line +
    #geom_errorbar(aes(ymin = mean_goods_produced - std_mean_goods_produced, ymax = mean_goods_produced + std_mean_goods_produced,
    #                  color=Scenario, group = Scenario)) +
    #continues_colour_brewer(palette = "Spectral", name="Infected") +
    xlab(paste(timeframe, "s", sep = "")) +
    ylab("QoL %") + 
    labs(title="Quality of life",
         subtitle="Mean QOL", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_smooth_uncertainty <- function(data_to_plot, timeframe, type_of_people) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = mean)) +
    gl_plot_smooth +
    gl_plot_ribbon_std + 
    #scale_colour_brewer(palette = "Spectral", name="Infected") +
    xlab(paste(timeframe, "s", sep = "")) +
    ylab("QoL %") + 
    labs(title="Quality of life",
         subtitle="Mean QOL with std. dev. across runs for area", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}


plot_ggplot_smooth_minmax <- function(data_to_plot, timeframe, type_of_people) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = mean)) +
    gl_plot_smooth +
    geom_ribbon(aes(ymin = min, ymax = max,
                    color= Scenario), alpha=0.025) +
    #scale_colour_brewer(palette = "Spectral", name="Infected") +
    xlab(paste(timeframe, "s", sep = "")) +
    ylab("QoL %") + 
    labs(title="Quality of life",
         subtitle="Mean QOL with area that shows maximum and minimum value present", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}
