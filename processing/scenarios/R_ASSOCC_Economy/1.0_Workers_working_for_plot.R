#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotEcoPeopleWokringFor <- function(df_economy, output_dir, one_plot) {
  
  name = "People_working_for_plot"
  
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
  
  df_workers_working_for_essential_shop <- df_economy %>% select(tick, run_number, Scenario,
                                                  working_for = workers_working_at_essential_shop,
  )
  
  df_workers_working_for_essential_shop_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(tick, Scenario,
                                                                                                  mean = mean(workers_working_at_essential_shop)
                                                                                                  ,std = sd(workers_working_at_essential_shop)
  )
  
  df_workers_working_for_non_essential_shop <- df_economy %>% select(tick, run_number, Scenario,
                                                   working_for = workers_working_at_non_essential_shop,
  )
  
  df_workers_working_for_non_essential_shop_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(tick, Scenario,
                                                                                                   mean = mean(workers_working_at_non_essential_shop)
                                                                                                   ,std = sd(workers_working_at_non_essential_shop)
  )
  
  df_workers_working_for_workplace <- df_economy %>% select(tick, run_number, Scenario,
                                                  working_for = workers_working_at_workplace,
  )
  
  df_workers_working_for_workplace_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(tick, Scenario,
                                                                                                  mean = mean(workers_working_at_workplace)
                                                                                                  ,std = sd(workers_working_at_workplace)
  )
  
  # ----- convert to days
  df_workers_working_for_essential_shop_mean_std_day <- df_workers_working_for_essential_shop_mean_std
  df_workers_working_for_essential_shop_mean_std_day$day <- dmfConvertTicksToDay(df_workers_working_for_essential_shop_mean_std_day$tick)
  df_workers_working_for_essential_shop_mean_std_day <- df_workers_working_for_essential_shop_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  df_workers_working_for_non_essential_shop_mean_std_day <- df_workers_working_for_non_essential_shop_mean_std
  df_workers_working_for_non_essential_shop_mean_std_day$day <- dmfConvertTicksToDay(df_workers_working_for_non_essential_shop_mean_std_day$tick)
  df_workers_working_for_non_essential_shop_mean_std_day <- df_workers_working_for_non_essential_shop_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  df_workers_working_for_workplace_mean_std_day <- df_workers_working_for_workplace_mean_std
  df_workers_working_for_workplace_mean_std_day$day <- dmfConvertTicksToDay(df_workers_working_for_workplace_mean_std_day$tick)
  df_workers_working_for_workplace_mean_std_day <- df_workers_working_for_workplace_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  #seg_people_calpital <- gather(df_mean_std, variable, measurement, mean, std)
  
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_workers_working_for_essential_shop_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_workers_working_for_non_essential_shop_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_workers_working_for_workplace_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_workers_working_for_essential_shop_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_workers_working_for_non_essential_shop_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_workers_working_for_workplace_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  #seg_people_calpital <- gather(df_people_captial, variable, measurement, workers, retired)
  
  print(paste(name, " making plots", sep=""))
  
  dmfPdfOpen(output_dir, "eco_essential_shop_working_for")
  print(plot_ggplot(df_workers_working_for_essential_shop_mean_std, "tick", "essential shop"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_essential_shop_working_for_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_workers_working_for_essential_shop_mean_std, "tick", "essential shop"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_non_essential_shop_working_for")
  print(plot_ggplot(df_workers_working_for_non_essential_shop_mean_std, "tick", "non-essential shop"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_non_essential_shop_working_for_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_workers_working_for_non_essential_shop_mean_std, "tick", "non essential-shop"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_workplace_working_for")
  print(plot_ggplot(df_workers_working_for_workplace_mean_std, "tick", "workplace"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_workplace_smooth")
  print(plot_ggplot_smooth_uncertainty_std(df_workers_working_for_workplace_mean_std, "tick", "workplace"))
  dmfPdfClose()
  
  #-- Days
  dmfPdfOpen(output_dir, "eco_essential_shop_working_for_day")
  print(plot_ggplot(df_workers_working_for_essential_shop_mean_std_day, "day", "essential shop"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_essential_shop_working_for_smooth_day")
  print(plot_ggplot_smooth_uncertainty_std(df_workers_working_for_essential_shop_mean_std_day, "day", "essential shop"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_non_essential_shop_working_for_day")
  print(plot_ggplot(df_workers_working_for_non_essential_shop_mean_std_day, "day", "non-essential shop"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_non_essential_shop_working_for_smooth_day")
  print(plot_ggplot_smooth_uncertainty_std(df_workers_working_for_non_essential_shop_mean_std_day, "day", "non essential-shop"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_workplace_working_for_day")
  print(plot_ggplot(df_workers_working_for_workplace_mean_std_day, "day", "workplace"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_workplace_smooth_day")
  print(plot_ggplot_smooth_uncertainty_std(df_workers_working_for_workplace_mean_std_day, "day", "workplace"))
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
    ylab("Workers") + 
    labs(title=paste("Workers working for", type_of_people, "", sep = " "),
         subtitle=paste("Workers working for", type_of_people, sep = " "), 
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
    ylab("Workers") + 
    labs(title=paste("Workers working for", type_of_people, "", sep = " "),
         subtitle=paste("Workers working for", type_of_people, "(smoothed)", sep = " "), 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}
