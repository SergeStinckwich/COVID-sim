#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotEcoSystemCapital <- function(df_economy, output_dir, one_plot) {
  
  name = "Capital_in_system_plot"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  # CONTACTS PER GATHERING POINT PER APP USAGE SCENARIO ----------------------------
  
  df_capital_in_system <- df_economy %>% select(tick, run_number, Scenario, total_capital = total_amount_of_capital_in_the_system)
  
  df_capital_in_system_mean_std <- df_economy %>% group_by(tick, Scenario) %>%
    summarise(tick, Scenario,
              mean = mean(total_amount_of_capital_in_the_system)
              ,std = sd(total_amount_of_capital_in_the_system)
    )
  
  # ----- convert to days
  df_capital_in_system_mean_std_day <- df_capital_in_system_mean_std
  df_capital_in_system_mean_std_day$day <- dmfConvertTicksToDay(df_capital_in_system_mean_std_day$tick)
  df_capital_in_system_mean_std_day <- df_capital_in_system_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_capital_in_system, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_capital_in_system_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_capital_in_system_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  seg_capital_in_system <- gather(df_capital_in_system, variable, measurement, total_capital)
  
  print(paste(name, " making plots", sep=""))
  
  dmfPdfOpen(output_dir, "eco_capital_in_system")
  print(plot_ggplot(seg_capital_in_system, "tick"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_capital_in_system_smooth_uncertainty")
  print(plot_ggplot_smooth_uncertainty(df_capital_in_system_mean_std, "tick"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_capital_in_system_smooth_uncertainty_day")
  print(plot_ggplot_smooth_uncertainty(df_capital_in_system_mean_std_day, "day"))
  dmfPdfClose()
  
  
  
  # dmfPdfOpen(output_dir, "eco_capital_in_system_smooth")
  # print(plot_ggplot_smooth(df_capital_in_system_mean_std))
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
    geom_line(size=1,alpha=0.8,aes(color=Scenario, group = Scenario)) +
    #geom_errorbar(aes(ymin = mean - std, ymax = mean + std,
    #                  color=Scenario, group = Scenario)) +
    #continues_colour_brewer(palette = "Spectral", name="Infected") +
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("Capital") + 
    labs(title="Capital in the system",
         subtitle="Capital in the total system", 
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
    #scale_colour_brewer(palette = "Spectral", name="Infected") +
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("Capital") + 
    labs(title="Capital in the system",
         subtitle="Capital in the total system (smoothed)", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_smooth_uncertainty <- function(data_to_plot, timeframe) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = mean)) +
    gl_plot_smooth +
    gl_plot_ribbon_std + 
    #scale_colour_brewer(palette = "Spectral", name="Infected") +
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("Capital") + 
    labs(title="Capital in the system",
         subtitle="Capital in the total system (smoothed + uncertainty (std. dev.)))", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}