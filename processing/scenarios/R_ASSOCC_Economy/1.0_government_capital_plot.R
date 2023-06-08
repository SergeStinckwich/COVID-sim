#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotEcoGovernmentCapital <- function(df_economy, output_dir, one_plot) {
  
  name = "government_capital_plot"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  # CONTACTS PER GATHERING POINT PER APP USAGE SCENARIO ----------------------------
  
  df_government_capital <- df_economy %>% select(tick, run_number, Scenario, government_capital = government_reserve_of_capital)
  
  df_government_capital_mean_std <- df_economy %>% group_by(tick, Scenario) %>%
    summarise(tick, Scenario,
              mean = mean(government_reserve_of_capital)
              ,std = sd(government_reserve_of_capital)
    )
  
  # ----- convert to days
  df_government_capital_mean_std_day <- df_government_capital_mean_std
  df_government_capital_mean_std_day$day <- dmfConvertTicksToDay(df_government_capital_mean_std_day$tick)
  df_government_capital_mean_std_day <- df_government_capital_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_government_capital, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_government_capital_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_government_capital_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  seg_government_capital <- gather(df_government_capital, variable, measurement, government_capital)
  
  print(paste(name, " making plots", sep=""))
  
  dmfPdfOpen(output_dir, "eco_government_capital")
  print(plot_ggplot(seg_government_capital, "tick"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_government_capital_smooth_uncertainty")
  print(plot_ggplot_smooth_uncertainty_std(df_government_capital_mean_std, "tick"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_government_capital_smooth_uncertainty_day")
  print(plot_ggplot_smooth_uncertainty_std(df_government_capital_mean_std_day, "day"))
  dmfPdfClose()
  
  
  # dmfPdfOpen(output_dir, "eco_government_capital_smooth")
  # print(plot_ggplot_smooth(df_government_capital_mean_std))
  # dmfPdfClose()
  
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================


plot_ggplot <- function(data_to_plot, timeframe) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = measurement)) +
    #geom_smooth(aes(col=Scenario), span=0.1, se=FALSE) +
    gl_plot_line +
    #geom_errorbar(aes(ymin = mean_goods_produced - std_mean_goods_produced, ymax = mean_goods_produced + std_mean_goods_produced,
    #                  color=Scenario, group = Scenario)) +
    #continues_colour_brewer(palette = "Spectral", name="Infected") +
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("Capital") + 
    labs(title="Government capital",
         subtitle="Capital of the government in reserve", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_smooth <- function(data_to_plot, timeframe) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = mean)) +
    gl_plot_smooth +
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("Capital") + 
    labs(title="Government capital",
         subtitle="Capital of the government in reserve (smoothed)", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_smooth_uncertainty_std <- function(data_to_plot, timeframe) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = mean)) +
    gl_plot_smooth +
    gl_plot_ribbon_std + 
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("Capital") + 
    labs(title="Government capital",
         subtitle="Capital of the government in reserve (smoothed + uncertainty (std. dev.)))", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}