#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotEcoPeopleCapital <- function(df_economy, output_dir, one_plot) {
  
  name = "People_capital_plot"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  # CONTACTS PER GATHERING POINT PER APP USAGE SCENARIO ----------------------------
  
  #group_by(tick) %>% summarise(total = mean(count_people_with_not_is_young_and_is_in_poverty))
  
  # Add days converted from ticks
  #df_economy$day <- dmfConvertTicksToDay(df_economy$tick)  
  
  # df_people_captial <- df_economy %>% select(tick, run_number, Scenario,
  #                                      students = students_average_amount_of_capital,
  #                                      retired = retirees_average_amount_of_capital,
  #                                      students = students_average_amount_of_capital)
  
  df_workers_captial <- df_economy %>% select(tick, run_number, Scenario,
                                             workers = workers_average_amount_of_capital,
                                             )
  
  df_workers_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(tick, Scenario,
                                              mean = mean(workers_average_amount_of_capital)
                                              ,std = sd(workers_average_amount_of_capital)
                                              ,max = max(workers_average_amount_of_capital)
                                              ,min = min(workers_average_amount_of_capital)
      )
  
  df_retired_captial <- df_economy %>% select(tick, run_number, Scenario,
                                              retired = retirees_average_amount_of_capital,
  )
  
  df_retired_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(tick, Scenario,
                                              mean = mean(retirees_average_amount_of_capital)
                                              ,std = sd(retirees_average_amount_of_capital)
                                              ,max = max(retirees_average_amount_of_capital)
                                              ,min = min(retirees_average_amount_of_capital)
  )
  
  df_students_captial <- df_economy %>% select(tick, run_number, Scenario,
                                              students = students_average_amount_of_capital,
  )
  
  df_students_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(tick, Scenario,
                                                mean = mean(students_average_amount_of_capital)
                                                ,std = sd(students_average_amount_of_capital)
                                                ,max = max(students_average_amount_of_capital)
                                                ,min = min(students_average_amount_of_capital)
  )
  
  # ------ DAYs -------
  
  df_workers_mean_std_day <- df_workers_mean_std
  df_workers_mean_std_day$day <- dmfConvertTicksToDay(df_workers_mean_std_day$tick)
  df_workers_mean_std_day <- df_workers_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean), # max as "doing something today" should be 1, not 0.25
    std = mean(std),
    max = max(max),
    min = max(min)) %>% unique()
  
  df_retired_mean_std_day <- df_retired_mean_std
  df_retired_mean_std_day$day <- dmfConvertTicksToDay(df_retired_mean_std_day$tick)
  df_retired_mean_std_day <- df_retired_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean), # max as "doing something today" should be 1, not 0.25
    std = mean(std),
    max = max(max),
    min = max(min)) %>% unique()
  
  df_students_mean_std_day <- df_students_mean_std
  df_students_mean_std_day$day <- dmfConvertTicksToDay(df_students_mean_std_day$tick)
  df_students_mean_std_day <- df_students_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean), # max as "doing something today" should be 1, not 0.25
    std = mean(std),
    max = max(max),
    min = max(min)) %>% unique()
  
  
  #seg_people_calpital <- gather(df_mean_std, variable, measurement, mean_capital, std_mean_capital)

  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_workers_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_retired_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_students_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  write.csv(df_workers_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_retired_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_students_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  #seg_people_calpital <- gather(df_people_captial, variable, measurement, students, retired)
  
  print(paste(name, " making plots", sep=""))
  
  dmfPdfOpen(output_dir, "eco_workers_capital")
  print(plot_ggplot(df_workers_mean_std, "tick", "workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_workers_capital_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_workers_mean_std, "tick", "workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_retired_capital")
  print(plot_ggplot(df_retired_mean_std, "tick", "retired"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_retired_capital_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_retired_mean_std, "tick", "retired"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_students_capital")
  print(plot_ggplot(df_students_mean_std, "tick", "students"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_students_capital_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_students_mean_std, "tick", "students"))
  dmfPdfClose()
  
 # --- days ---
  dmfPdfOpen(output_dir, "eco_workers_capital")
  print(plot_ggplot(df_workers_mean_std_day, "day", "workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_workers_capital_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_workers_mean_std_day, "day", "workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_retired_capital")
  print(plot_ggplot(df_retired_mean_std_day, "day", "retired"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_retired_capital_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_retired_mean_std_day, "day", "retired"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_students_capital")
  print(plot_ggplot(df_students_mean_std_day, "day", "students"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_students_capital_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_students_mean_std_day, "day", "students"))
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
    xlab(paste(timeframe, "s", sep = "")) +
    ylab("Capital") + 
    labs(title=paste("Average", type_of_people, "capital", sep = " "),
         subtitle=paste("Average capital of", type_of_people, sep = " "), 
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
    xlab(paste(timeframe, "s", sep = "")) +
    ylab("Capital") + 
    labs(title=paste("Average", type_of_people, "capital", sep = " "),
         subtitle=paste("Average capital of", type_of_people, "(smoothed + uncertainty (std. dev.))", sep = " "), 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    #scale_colour_viridis_d(option = "d") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}

#plot_ggplot_smooth(df_mean_std, "students")