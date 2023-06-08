#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotEcoPoverty <- function(df_economy, output_dir, one_plot) {
  
  name = "Poverty_plot"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  #  ----------------------------

  #group_by(tick) %>% summarise(total = mean(count_people_with_not_is_young_and_is_in_poverty))

  df_poverty_total <- df_economy %>% select(tick, run_number, Scenario, poverty = count_people_with_not_is_young_and_is_in_poverty)
  
  df_poverty_total_mean_std <- df_economy %>% group_by(tick, Scenario) %>%
    summarise(tick, Scenario,
              mean = mean(count_people_with_not_is_young_and_is_in_poverty)
              ,std = sd(count_people_with_not_is_young_and_is_in_poverty)
    ) %>% unique()
  
  

  df_poverty_worker <- df_economy %>% select(run_number, random_seed, tick, Scenario, poverty = count_workers_with_is_in_poverty)
  
  df_poverty_worker_mean_std <- df_economy %>% group_by(tick, Scenario) %>%
    summarise(tick, Scenario,
              mean = mean(count_workers_with_is_in_poverty)
              ,std = sd(count_workers_with_is_in_poverty)
    ) %>% unique()
  
  # ---- calculate poverty of all people who get a steady subsidy income from the government
  df_poverty_subsidy <- df_economy %>% mutate(poverty = count_retireds_with_is_in_poverty +
                                                count_students_with_is_in_poverty)
  
  # df_poverty_subsidy <- df_poverty_subsidy %>% select(run_number,
  #                                             random_seed,
  #                                             tick,
  #                                             Scenario,
  #                                             poverty
  #                                             )
  
  df_poverty_subsidy_mean_std <- df_poverty_subsidy %>% group_by(tick, Scenario) %>%
    summarise(tick, Scenario,
              mean = mean(poverty)
              ,std = sd(poverty)
    ) %>% unique()
  
  
  # Add days converted from ticks
  df_poverty_total_day <- df_poverty_total
  df_poverty_total_day$day <- dmfConvertTicksToDay(df_poverty_total_day$tick)
  df_poverty_total_day <- df_poverty_total_day %>% group_by(day, run_number) %>% summarise(day, run_number, Scenario, mean = mean(poverty)) %>% unique()
  
  df_poverty_worker_day <- df_poverty_worker
  df_poverty_worker_day$day <- dmfConvertTicksToDay(df_poverty_worker_day$tick)
  df_poverty_worker_day <- df_poverty_worker_day %>% group_by(day, run_number) %>% summarise(day, run_number, Scenario, mean = mean(poverty)) %>% unique()
  
  df_poverty_subsidy_day <- df_poverty_subsidy
  df_poverty_subsidy_day$day <- dmfConvertTicksToDay(df_poverty_subsidy_day$tick)
  df_poverty_subsidy_day <- df_poverty_subsidy_day %>% group_by(day, run_number) %>% summarise(day, run_number, Scenario, mean = mean(poverty)) %>% unique()
  
  
  # and for averages of scenario
  df_poverty_total_mean_std_day <- df_poverty_total_mean_std
  df_poverty_total_mean_std_day$day <- dmfConvertTicksToDay(df_poverty_total_mean_std_day$tick)
  df_poverty_total_mean_std_day <- df_poverty_total_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  df_poverty_worker_mean_std_day <- df_poverty_worker_mean_std
  df_poverty_worker_mean_std_day$day <- dmfConvertTicksToDay(df_poverty_worker_mean_std_day$tick)
  df_poverty_worker_mean_std_day <- df_poverty_worker_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  df_poverty_subsidy_mean_std_day <- df_poverty_subsidy_mean_std
  df_poverty_subsidy_mean_std_day$day <- dmfConvertTicksToDay(df_poverty_subsidy_mean_std_day$tick)
  df_poverty_subsidy_mean_std_day <- df_poverty_subsidy_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  # And now for weeks.
  #  For this to go properly do remember that you need to have full weeks of ticks.
  #  We do not correct for broken weeks. 
  df_poverty_total_week <- df_poverty_total_day
  df_poverty_total_week$week <- dmfConvertDaysToWeek(df_poverty_total_week$day)
  df_poverty_total_week <- df_poverty_total_week %>% group_by(week, run_number) %>% summarise(week, run_number, Scenario, mean = mean(mean)) %>% unique()
  
  df_poverty_worker_week <- df_poverty_worker_day
  df_poverty_worker_week$week <- dmfConvertDaysToWeek(df_poverty_worker_week$day)
  df_poverty_worker_week <- df_poverty_worker_week %>% group_by(week, run_number) %>% summarise(week, run_number, Scenario, mean = mean(mean)) %>% unique()
  
  
  
  # and for averages of scenario
  df_poverty_total_mean_std_week <- df_poverty_total_mean_std_day
  df_poverty_total_mean_std_week$week <- dmfConvertDaysToWeek(df_poverty_total_mean_std_week$day)
  df_poverty_total_mean_std_week <- df_poverty_total_mean_std_week %>% group_by(week, Scenario) %>% summarise(
    week, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  df_poverty_worker_mean_std_week <- df_poverty_worker_mean_std_day
  df_poverty_worker_mean_std_week$week <- dmfConvertDaysToWeek(df_poverty_worker_mean_std_week$day)
  df_poverty_worker_mean_std_week <- df_poverty_worker_mean_std_week %>% group_by(week, Scenario) %>% summarise(
    week, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  df_poverty_subsidy_mean_std_week <- df_poverty_subsidy_mean_std_day
  df_poverty_subsidy_mean_std_week$week <- dmfConvertDaysToWeek(df_poverty_subsidy_mean_std_week$day)
  df_poverty_subsidy_mean_std_week <- df_poverty_subsidy_mean_std_week %>% group_by(week, Scenario) %>% summarise(
    week, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  
  
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_poverty_worker, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_poverty_worker_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_poverty_worker_week, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_poverty_worker_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_poverty_worker_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_poverty_worker_mean_std_week, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_poverty_subsidy_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_poverty_subsidy_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_poverty_subsidy_mean_std_week, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  print(paste(name, " making plots", sep=""))
  
  seg_poverty_total <- gather(df_poverty_total, variable, measurement, poverty)

  
  dmfPdfOpen(output_dir, "eco_poverty_total_tick")
  print(plot_ggplot(seg_poverty_total, "tick", "People"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_poverty_total_smooth_uncertainty_tick")
  print(plot_ggplot_smooth_uncertainty(df_poverty_total_mean_std, "tick", "People"))
  dmfPdfClose()
  

  
  seg_poverty_worker <- gather(df_poverty_worker, variable, measurement, poverty)
  
  dmfPdfOpen(output_dir, "eco_poverty_worker_tick")
  print(plot_ggplot(seg_poverty_worker, "tick", "Workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_poverty_worker_smooth_uncertainty_tick")
  print(plot_ggplot_smooth_uncertainty(df_poverty_worker_mean_std, "tick", "Workers"))
  dmfPdfClose()

  dmfPdfOpen(output_dir, "eco_poverty_subsidy_smooth_uncertainty_tick")
  print(plot_ggplot_smooth_uncertainty(df_poverty_subsidy_mean_std, "tick", "Subsedized"))
  dmfPdfClose()
  
  #days
  seg_poverty_total_day <- gather(df_poverty_total_day, variable, measurement, mean)
  
  dmfPdfOpen(output_dir, "eco_poverty_total_day")
  print(plot_ggplot(seg_poverty_total_day, "day", "People"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_poverty_total_smooth_uncertainty_day")
  print(plot_ggplot_smooth_uncertainty(df_poverty_total_mean_std_day, "day", "People"))
  dmfPdfClose()
  

  seg_poverty_worker_day <- gather(df_poverty_worker_day, variable, measurement, mean)
  
  dmfPdfOpen(output_dir, "eco_poverty_worker_day")
  print(plot_ggplot(seg_poverty_worker_day, "day", "Workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_poverty_worker_smooth_uncertainty_day")
  print(plot_ggplot_smooth_uncertainty(df_poverty_worker_mean_std_day, "day", "Workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_poverty_subsidy_smooth_day")
  print(plot_ggplot_smooth(df_poverty_subsidy_mean_std_day, "day", "Subsedized"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_poverty_subsidy_smooth_uncertainty_day")
  print(plot_ggplot_smooth_uncertainty(df_poverty_subsidy_mean_std_day, "day", "Subsedized"))
  dmfPdfClose()
  

  #weeks
  seg_poverty_total_week <- gather(df_poverty_total_week, variable, measurement, mean)
  
  dmfPdfOpen(output_dir, "eco_poverty_total_week")
  print(plot_ggplot(seg_poverty_total_week, "week", "People"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_poverty_total_smooth_uncertainty_week")
  print(plot_ggplot_smooth_uncertainty(df_poverty_total_mean_std_week, "week", "People"))
  dmfPdfClose()

  
  seg_poverty_worker_week <- gather(df_poverty_worker_week, variable, measurement, mean)
  
  
  dmfPdfOpen(output_dir, "eco_poverty_worker_week")
  print(plot_ggplot(seg_poverty_worker_week, "week", "Workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_poverty_worker_smooth_uncertainty_week")
  print(plot_ggplot_smooth_uncertainty(df_poverty_worker_mean_std_week, "week", "Workers"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_poverty_subsidy_smooth_uncertainty_week")
  print(plot_ggplot_smooth_uncertainty(df_poverty_subsidy_mean_std_week, "week", "Subsedized"))
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
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("People") + 
    labs(title=paste(type_of_people, "in poverty", sep = " "),
         subtitle="One is 'in poverty' when they can't buy >2 sets of rations", 
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
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("People") + 
    labs(title=paste(type_of_people, "in poverty", sep = " "),
         subtitle="One is 'in poverty' when they can't buy >2 sets of rations (smoothed + uncertainty (std. dev.))", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_smooth <- function(data_to_plot, timeframe, type_of_people) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = mean)) +
    gl_plot_smooth +
    #scale_colour_brewer(palette = "Spectral", name="Infected") +
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("People") + 
    labs(title=paste(type_of_people, "in poverty", sep = " "),
         subtitle="One is 'in poverty' when they can't buy >2 sets of rations (smoothed + uncertainty (std. dev.))", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}
