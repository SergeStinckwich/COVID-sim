#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotEcoPeopleStdCapital <- function(df_economy, output_dir, one_plot) {
  
  name = "People_std_capital_plot"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  # CONTACTS PER GATHERING POINT PER APP USAGE SCENARIO ----------------------------
  
  #group_by(tick) %>% summarise(total = mean(count_people_with_not_is_young_and_is_in_poverty))
  
  # Add days converted from ticks
  #df_economy$day <- dmfConvertTicksToDay(df_economy$tick)  
  
  # df_people_captial <- df_economy %>% select(tick, run_number, Scenario,
  #                                      workers = workers_average_amount_of_goods,
  #                                      retired = retirees_average_amount_of_goods,
  #                                      students = students_average_amount_of_goods)
  
  df_workers_capital_std <- df_economy %>% select(tick, run_number, Scenario,
                                                   capitalstd = standard_deviation_my_amount_of_capital_of_workers,
  )
  
  df_workers_capital_std_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(tick, Scenario,
                                                                                             mean = mean(standard_deviation_my_amount_of_capital_of_workers)
                                                                                             ,std = sd(standard_deviation_my_amount_of_capital_of_workers)
  )
  
  df_students_capital_std <- df_economy %>% select(tick, run_number, Scenario,
                                                       capital_std = standard_deviation_my_amount_of_capital_of_students,
  )
  
  df_students_capital_std_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(tick, Scenario,
                                                                                                 mean = mean(standard_deviation_my_amount_of_capital_of_students)
                                                                                                 ,std = sd(standard_deviation_my_amount_of_capital_of_students)
  )
  
  df_retired_capital_std <- df_economy %>% select(tick, run_number, Scenario,
                                              capital_std = standard_deviation_my_amount_of_capital_of_retireds,
  )
  
  df_retired_capital_std_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(tick, Scenario,
                                                                                        mean = mean(standard_deviation_my_amount_of_capital_of_retireds)
                                                                                        ,std = sd(standard_deviation_my_amount_of_capital_of_retireds)
  )
  
  
  # ----- convert to days
  df_workers_capital_std_mean_std_day <- df_workers_capital_std_mean_std
  df_workers_capital_std_mean_std_day$day <- dmfConvertTicksToDay(df_workers_capital_std_mean_std_day$tick)
  df_workers_capital_std_mean_std_day <- df_workers_capital_std_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  df_students_capital_std_mean_std_day <- df_students_capital_std_mean_std
  df_students_capital_std_mean_std_day$day <- dmfConvertTicksToDay(df_students_capital_std_mean_std_day$tick)
  df_students_capital_std_mean_std_day <- df_students_capital_std_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  df_retired_capital_std_mean_std_day <- df_retired_capital_std_mean_std
  df_retired_capital_std_mean_std_day$day <- dmfConvertTicksToDay(df_retired_capital_std_mean_std_day$tick)
  df_retired_capital_std_mean_std_day <- df_retired_capital_std_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  

  
  #seg_people_calpital <- gather(df_mean_std, variable, measurement, mean, std)
  
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_workers_capital_std_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_students_capital_std_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_retired_capital_std_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  #seg_people_calpital <- gather(df_people_captial, variable, measurement, workers, retired)
  
  print(paste(name, " making plots", sep=""))
  
  dmfPdfOpen(output_dir, "eco_workers_capital_capital_std")
  print(plot_ggplot(df_workers_capital_std_mean_std, "tick", "workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_workers_capital_std_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_workers_capital_std_mean_std, "tick", "workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_students_capital_capital_std")
  print(plot_ggplot(df_students_capital_std_mean_std, "tick", "students"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_students_capital_std_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_students_capital_std_mean_std, "tick", "students"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_retired_capital_std")
  print(plot_ggplot(df_retired_capital_std_mean_std, "tick", "retired"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_retired_capital_std_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_retired_capital_std_mean_std, "tick", "retired"))
  dmfPdfClose()
  
  # --- days
  dmfPdfOpen(output_dir, "eco_workers_capital_capital_std_day")
  print(plot_ggplot(df_workers_capital_std_mean_std_day, "day", "workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_workers_capital_std_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_workers_capital_std_mean_std_day, "day", "workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_students_capital_capital_std")
  print(plot_ggplot(df_students_capital_std_mean_std_day, "day", "students"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_students_capital_std_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_students_capital_std_mean_std_day, "day", "students"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_retired_capital_std")
  print(plot_ggplot(df_retired_capital_std_mean_std_day, "day", "retired"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_retired_capital_std_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_retired_capital_std_mean_std_day, "day", "retired"))
  dmfPdfClose()
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================



plot_ggplot <- function(data_to_plot, timeframe, type_of_people) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = mean)) +
    #geom_smooth(aes(col=Scenario), span=0.1, se=FALSE) +
    geom_line(size=2,alpha=0.8,aes(color=Scenario, group = Scenario)) +
    #geom_errorbar(aes(ymin = mean_goods_produced - std_mean_goods_produced, ymax = mean_goods_produced + std_mean_goods_produced,
    #                  color=Scenario, group = Scenario)) +
    #continues_colour_brewer(palette = "Spectral", name="Infected") +
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("Standard deviation") + 
    labs(title=paste("Capital standard deviation of", type_of_people, "", sep = " "),
         subtitle=paste("Standard deviation of capital owned by ", type_of_people, sep = " "), 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}


plot_ggplot_smooth_uncertainty_std <- function(data_to_plot, timeframe, type_of_people) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = mean)) +
    gl_plot_smooth +
    gl_plot_ribbon_std + 
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("Standard deviation") + 
    labs(title=paste("Capital standard deviation of", type_of_people, "", sep = " "),
         subtitle=paste("Standard deviation of capital owned by ", type_of_people, "(smoothed + uncertainty (std. dev.))", sep = " "), 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}
