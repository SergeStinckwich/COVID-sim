#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotEcoCompanyGoods <- function(df_economy, output_dir, one_plot) {
  
  name = "Company_goods_plot"
  
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
  
  df_essential_shop_goods <- df_economy %>% select(tick, run_number, Scenario,
                                              goods = essential_shop_amount_of_goods_in_stock,
  )
  
  df_essential_shop_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(tick, Scenario,
                                                                                      mean = mean(essential_shop_amount_of_goods_in_stock)
                                                                                      ,std = sd(essential_shop_amount_of_goods_in_stock)
  )
  
  df_non_essential_shop_goods <- df_economy %>% select(tick, run_number, Scenario,
                                              goods = non_essential_shop_amount_of_goods_in_stock,
  )
  
  df_non_essential_shop_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(tick, Scenario,
                                                                                      mean = mean(non_essential_shop_amount_of_goods_in_stock)
                                                                                      ,std = sd(non_essential_shop_amount_of_goods_in_stock)
  )
  
  df_workplace_goods <- df_economy %>% select(tick, run_number, Scenario,
                                               goods = workplace_amount_of_goods_in_stock,
  )

  df_workplace_mean_std <- df_economy %>% group_by(tick, Scenario) %>% summarise(tick, Scenario,
                                                                                       mean = mean(workplace_amount_of_goods_in_stock)
                                                                                       ,std = sd(workplace_amount_of_goods_in_stock)
  )
  
  #seg_people_calpital <- gather(df_mean_std, variable, measurement, mean, std)
  
  # ----- convert to days
  df_essential_shop_mean_std_day <- df_essential_shop_mean_std
  df_essential_shop_mean_std_day$day <- dmfConvertTicksToDay(df_essential_shop_mean_std_day$tick)
  df_essential_shop_mean_std_day <- df_essential_shop_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  df_non_essential_shop_mean_std_day <- df_non_essential_shop_mean_std
  df_non_essential_shop_mean_std_day$day <- dmfConvertTicksToDay(df_non_essential_shop_mean_std_day$tick)
  df_non_essential_shop_mean_std_day <- df_non_essential_shop_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  df_workplace_mean_std_day <- df_workplace_mean_std
  df_workplace_mean_std_day$day <- dmfConvertTicksToDay(df_workplace_mean_std_day$tick)
  df_workplace_mean_std_day <- df_workplace_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = mean(mean),
    std = mean(std))
  
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_essential_shop_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_non_essential_shop_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_workplace_mean_std, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_essential_shop_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_non_essential_shop_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_workplace_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  #seg_people_calpital <- gather(df_people_captial, variable, measurement, workers, retired)
  
  print(paste(name, " making plots", sep=""))
  
  dmfPdfOpen(output_dir, "eco_essential_shop_goods")
  print(plot_ggplot(df_essential_shop_mean_std, "essential shop", "tick"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_essential_shop_goods_smooth")
  print(plot_ggplot_smooth(df_essential_shop_mean_std, "essential shop", "tick"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_non_essential_shop_goods")
  print(plot_ggplot(df_non_essential_shop_mean_std, "non-essential shop", "tick"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_non_essential_shop_goods_smooth")
  print(plot_ggplot_smooth(df_non_essential_shop_mean_std, "non-essential shop", "tick"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_workplace_goods")
  print(plot_ggplot(df_workplace_mean_std, "workplace", "tick"))
  dmfPdfClose()

  dmfPdfOpen(output_dir, "eco_workplace_smooth_goodd")
  print(plot_ggplot_smooth(df_workplace_mean_std, "workplace", "tick"))
  dmfPdfClose()
  
  # --- days
  dmfPdfOpen(output_dir, "eco_essential_shop_goods_smooth_day")
  print(plot_ggplot_smooth(df_essential_shop_mean_std_day, "essential shop", "day"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_non_essential_shop_goods_smooth_day")
  print(plot_ggplot_smooth(df_non_essential_shop_mean_std_day, "non-essential shop", "day"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "eco_workplace_capital_goods_day")
  print(plot_ggplot_smooth(df_workplace_mean_std_day, "workplace", "day"))
  dmfPdfClose()
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================


plot_ggplot <- function(data_to_plot, type_of_people, timeframe) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = mean)) +
    geom_line(size=1,alpha=0.8,aes(color=Scenario, group = Scenario)) +
    #geom_errorbar(aes(ymin = mean - std, ymax = mean + std,
    #                  color=Scenario, group = Scenario)) +
    #continues_colour_brewer(palette = "Spectral", name="Infected") +
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("Goods") + 
    labs(title=paste("Average", type_of_people, "goods in stock", sep = " "),
         subtitle=paste("Average goods in stock at", type_of_people, sep = " "), 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}


plot_ggplot_smooth <- function(data_to_plot, type_of_people, timeframe) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = mean)) +
    gl_plot_smooth +
    geom_ribbon(aes(ymin = mean - std, ymax = mean + std,
                    color= Scenario), alpha=0.025) +
    #scale_colour_brewer(palette = "Spectral", name="Infected") +
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("Goods") + 
    labs(title=paste("Average", type_of_people, "goods in stock", sep = " "),
         subtitle=paste("Average goods in stock at", type_of_people, "(smoothed + uncertainty (std. dev.))", sep = " "), 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}
