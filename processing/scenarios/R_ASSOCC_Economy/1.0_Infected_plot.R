#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotEcoInfected <- function(df_economy, output_dir, one_plot) {
  
  name = "Infected_plot"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  # Add days converted from ticks
  #df_economy$day <- dmfConvertTicksToDay(df_economy$tick)  
  
  df_infected <- df_economy %>% select(tick, run_number, Scenario, infected = count_people_with_is_infected)
  
  #for uncertainty area
  df_infected_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(
                                                                    mean = mean(count_people_with_is_infected)
                                                                    ,std = sd(count_people_with_is_infected))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_infected, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  seg_infected <- gather(df_infected, variable, measurement, infected)
  
  print(paste(name, " making plots", sep=""))
  
  dmfPdfOpen(output_dir, "eco_infected")
  print(plot_ggplot(seg_infected))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_infected_smooth")
  print(plot_ggplot_smooth(seg_infected))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_infected_smooth_uncertainty")
  print( plot_ggplot_smooth_uncertainty(df_infected_mean_std))
  dmfPdfClose() 
  
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================


plot_ggplot <- function(data_to_plot) {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement)) +
    gl_plot_line +
    #continues_colour_brewer(palette = "Spectral", name="Infected") +
    xlab("Ticks") +
    ylab("Infected") + 
    labs(title="Infected numbers",
         subtitle="Total number of people infected", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_smooth <- function(data_to_plot) {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement)) +
    geom_smooth(aes(col=Scenario, group = run_number), span=0.1, se=FALSE) +
    #scale_colour_brewer(palette = "Spectral", name="Infected") +
    xlab("Ticks") +
    ylab("Infected") + 
    labs(title="Infected numbers",
         subtitle="Total number of people Infected (smoothed)", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_smooth_uncertainty <- function(data_to_plot) {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = mean)) +
    gl_plot_smooth +
    gl_plot_ribbon_std + 
    #scale_colour_brewer(palette = "Spectral", name="Infected") +
    xlab("Ticks") +
    ylab("Infected") + 
    labs(title="Infected numbers",
         subtitle="Total number of people infected (smoothed with uncertainty (std. dev.))", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}