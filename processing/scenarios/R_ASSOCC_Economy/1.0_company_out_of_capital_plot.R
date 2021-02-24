#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotEcoCompanyOutOfCaptial <- function(df_economy, output_dir, one_plot) {
  
  name = "Company_out_of_capital_plot"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  #  ----------------------------
  
  #group_by(tick) %>% summarise(total = mean(count_people_with_not_is_young_and_is_in_poverty))  
  
  #NOTE: companies are only OOC once every 4 ticks for some reason.
  
  # Essential Shop Out Of Capital
  df_ES_OOC <- df_economy %>% select(tick, run_number, Scenario, OOC = essential_shops_out_of_capital)
  
  df_ES_OOC_mean_std <- df_economy %>% group_by(tick, Scenario) %>%
    summarise(tick, Scenario,
              mean = mean(essential_shops_out_of_capital)
              ,std = sd(essential_shops_out_of_capital)
              ,min = min(essential_shops_out_of_capital)
              ,max = max(essential_shops_out_of_capital)
    ) %>% unique()
  
  df_NES_OOC <- df_economy %>% select(tick, run_number, Scenario, OOC = non_essential_shops_out_of_capital)
  
  df_NES_OOC_mean_std <- df_economy %>% group_by(tick, Scenario) %>%
    summarise(tick, Scenario,
              mean = mean(non_essential_shops_out_of_capital)
              ,std = sd(non_essential_shops_out_of_capital)
              ,min = min(non_essential_shops_out_of_capital)
              ,max = max(non_essential_shops_out_of_capital)
    ) %>% unique()
  
  df_WP_OOC <- df_economy %>% select(tick, run_number, Scenario, OOC = workplaces_out_of_capital)
  
  df_WP_OOC_mean_std <- df_economy %>% group_by(tick, Scenario) %>%
    summarise(tick, Scenario,
              mean = mean(workplaces_out_of_capital)
              ,std = sd(workplaces_out_of_capital)
              ,min = min(workplaces_out_of_capital)
              ,max = max(workplaces_out_of_capital)
    ) %>% unique()
  
  
  
  # Add days converted from ticks
  df_ES_OOC_mean_std_day <- df_ES_OOC_mean_std
  df_ES_OOC_mean_std_day$day <- dmfConvertTicksToDay(df_ES_OOC_mean_std_day$tick)
  df_ES_OOC_mean_std_day <- df_ES_OOC_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = max(mean), # max as "doing something today" should be 1, not 0.25
    std = mean(std),
    max = max(max),
    min = max(min)) %>% unique()
  
  df_NES_OOC_mean_std_day <- df_NES_OOC_mean_std
  df_NES_OOC_mean_std_day$day <- dmfConvertTicksToDay(df_NES_OOC_mean_std_day$tick)
  df_NES_OOC_mean_std_day <- df_NES_OOC_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = max(mean), # max as "doing something today" should be 1, not 0.25
    std = mean(std),
    max = max(max),
    min = max(min)) %>% unique()
  
  df_WP_OOC_mean_std_day <- df_WP_OOC_mean_std
  df_WP_OOC_mean_std_day$day <- dmfConvertTicksToDay(df_WP_OOC_mean_std_day$tick)
  df_WP_OOC_mean_std_day <- df_WP_OOC_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = max(mean), # max as "doing something today" should be 1, not 0.25
    std = mean(std),
    max = max(max),
    min = max(min)) %>% unique()
  
  
  
  # And now for weeks.
  #  For this to go properly do remember that you need to have full weeks of ticks.
  #  We do not correct for broken weeks. 
  # df_poverty_total_week <- df_poverty_total_day
  # df_poverty_total_week$Week <- dmfConvertDaysToWeek(df_poverty_total_week$day)
  # df_poverty_total_week <- df_poverty_total_week %>% group_by(Week, run_number) %>% summarise(Week, run_number, Scenario, mean = mean(mean)) %>% unique()
  # 
  # df_poverty_worker_week <- df_poverty_worker_day
  # df_poverty_worker_week$Week <- dmfConvertDaysToWeek(df_poverty_worker_week$day)
  # df_poverty_worker_week <- df_poverty_worker_week %>% group_by(Week, run_number) %>% summarise(Week, run_number, Scenario, mean = mean(mean)) %>% unique()
  # 
  
  
  # and for averages of scenario
  
  df_ES_OOC_mean_std_week <- df_ES_OOC_mean_std_day
  df_ES_OOC_mean_std_week$Week <- dmfConvertDaysToWeek(df_ES_OOC_mean_std_week$day)
  df_ES_OOC_mean_std_week <- df_ES_OOC_mean_std_week %>% group_by(Week, Scenario) %>% summarise(
    Week, Scenario,
    mean = mean(mean),
    std = mean(std),
    max = mean(max),
    min = mean(min)) %>% unique()
  
  df_NES_OOC_mean_std_week <- df_NES_OOC_mean_std_day
  df_NES_OOC_mean_std_week$Week <- dmfConvertDaysToWeek(df_NES_OOC_mean_std_week$day)
  df_NES_OOC_mean_std_week <- df_NES_OOC_mean_std_week %>% group_by(Week, Scenario) %>% summarise(
    Week, Scenario,
    mean = mean(mean),
    std = mean(std),
    max = mean(max),
    min = mean(min)) %>% unique()
  
  df_WP_OOC_mean_std_week <- df_WP_OOC_mean_std_day
  df_WP_OOC_mean_std_week$Week <- dmfConvertDaysToWeek(df_WP_OOC_mean_std_week$day)
  df_WP_OOC_mean_std_week <- df_WP_OOC_mean_std_week %>% group_by(Week, Scenario) %>% summarise(
    Week, Scenario,
    mean = mean(mean),
    std = mean(std),
    max = mean(max),
    min = mean(min)) %>% unique()
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_ES_OOC, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_NES_OOC, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_WP_OOC, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_ES_OOC_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_NES_OOC_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_WP_OOC_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_ES_OOC_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_NES_OOC_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_WP_OOC_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_ES_OOC_mean_std_week, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_NES_OOC_mean_std_week, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_WP_OOC_mean_std_week, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  print(paste(name, " making plots", sep=""))
  
  
  #----- ticks --------------------
  
  seg_ES_OOC <- gather(df_ES_OOC, variable, measurement, OOC)
  
  
  dmfPdfOpen(output_dir, "eco_company_OOC_ES_tick")
  print(plot_ggplot(seg_ES_OOC, "tick", "Essential Shop"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_company_OOC_ES_smooth_uncertainty_tick")
  print(plot_ggplot_smooth_uncertainty(df_ES_OOC_mean_std, "tick", "Essential Shop"))
  dmfPdfClose()
  
  
  seg_NES_OOC <- gather(df_NES_OOC, variable, measurement, OOC)
  
  dmfPdfOpen(output_dir, "eco_company_OOC_NES_tick")
  print(plot_ggplot(seg_NES_OOC, "tick", "Non-Essential Shop"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_company_OOC_NES_smooth_uncertainty_tick")
  print(plot_ggplot_smooth_uncertainty(df_NES_OOC_mean_std, "tick", "Non-Essential Shop"))
  dmfPdfClose()
  
  
  seg_WP_OOC <- gather(df_WP_OOC, variable, measurement, OOC)
  
  dmfPdfOpen(output_dir, "eco_company_OOC_WP_tick")
  print(plot_ggplot(seg_WP_OOC, "tick", "Workplaces"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_company_OOC_WP_smooth_uncertainty_tick")
  print(plot_ggplot_smooth_uncertainty(df_WP_OOC_mean_std, "tick", "Workplaces"))
  dmfPdfClose()
  
  #----- days --------------------
 
  # seg_poverty_worker_day <- gather(df_poverty_worker_day, variable, measurement, mean)
  # 
  # dmfPdfOpen(output_dir, "eco_poverty_worker_day")
  # print(plot_ggplot(seg_poverty_worker_day, "day", "Workers"))
  # dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_company_OOC_ES_smooth_uncertainty_day")
  print(plot_ggplot_smooth_uncertainty(df_ES_OOC_mean_std_day, "day", "Essential shops"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_company_OOC_NES_smooth_uncertainty_day")
  print(plot_ggplot_smooth_uncertainty(df_NES_OOC_mean_std_day, "day", "Non-essential shops"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_company_OOC_WP_smooth_uncertainty_day")
  print(plot_ggplot_smooth_uncertainty(df_WP_OOC_mean_std_day, "day", "Workplaces"))
  dmfPdfClose()
  
  
  #----- weeks --------------------
  # seg_poverty_total_week <- gather(df_poverty_total_week, variable, measurement, mean)
  # 
  # dmfPdfOpen(output_dir, "eco_poverty_total_week")
  # print(plot_ggplot(seg_poverty_total_week, "Week", "People"))
  # dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_company_OOC_ES_smooth_uncertainty_week")
  print(  plot_ggplot_smooth_uncertainty(df_ES_OOC_mean_std_week, "Week", "Essential shops"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_company_OOC_NES_smooth_uncertainty_week")
  print(  plot_ggplot_smooth_uncertainty(df_NES_OOC_mean_std_week, "Week", "Non-essential shops"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_company_OOC_WP_smooth_uncertainty_week")
  print(  plot_ggplot_smooth_uncertainty(df_WP_OOC_mean_std_week, "Week", "Workplaces"))
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
    ylab("Companies") + 
    labs(title=paste(type_of_people, "out of capital", sep = " "),
         subtitle="Amount out of capital", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = c('#391c1c', '#16727e', '#de9236', '#cedecb')) +
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
    ylab("Companies") + 
    labs(title=paste(type_of_people, "out of capital", sep = " "),
         subtitle="Amount out of capital (smoothed + uncertainty (std. dev.))", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    #scale_colour_viridis_d(option = "d") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_smooth_uncertainty <- function(data_to_plot, timeframe, type_of_people) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = mean)) +
    gl_plot_smooth +
    gl_plot_ribbon_minmax +
    xlab(paste(timeframe, "s", sep = "")) +
    ylab("Companies") + 
    labs(title=paste(type_of_people, "out of capital", sep = " "),
         subtitle="Amount out of capital (smoothed + uncertainty (min & max))", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    #scale_colour_viridis_d(option = "d") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}


