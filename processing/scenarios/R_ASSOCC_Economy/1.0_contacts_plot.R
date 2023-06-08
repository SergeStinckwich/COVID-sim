#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotContacts <- function(df_economy, output_dir, one_plot) {
  
  name = "Contacts_plot"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  #  ----------------------------
  
  #group_by(tick) %>% summarise(total = mean(count_people_with_not_is_young_and_is_in_poverty))  
  
  #NOTE: companies are only OOC once every 4 ticks for some reason.
  
  # Essential Shop Out Of Capital
  df_contacts <- df_economy %>% select(tick, run_number, Scenario, starts_with("contacts"))
  
  #Scenario 1 --------------------------------------------
  df_contacts_1 <- df_contacts %>% filter(Scenario == "1-baseline") 

  df_contacts_1_summarized <- df_contacts_1 %>% group_by(tick) %>% select(starts_with("contacts")) %>% summarise_all(
    list(mean = mean, sd = sd, min = min, max = max)
  )
  
  df_contacts_1_summarized_mean <- df_contacts_1 %>% group_by(tick) %>% select(starts_with("contacts")) %>% summarise_all(
    list(mean = mean)
  )
  
  df_contacts_1_summarized_mean_no_total <- df_contacts_1_summarized_mean %>% select(-"contacts_last_tick_mean")
  
  df_contacts_1_summarized_mean_no_total <- df_contacts_1_summarized_mean_no_total %>% rename_with( ~ gsub("contacts_in_", "", .x, fixed = TRUE))
  df_contacts_1_summarized_mean_no_total <- df_contacts_1_summarized_mean_no_total %>% rename_with( ~ gsub("_mean", "", .x, fixed = TRUE))
  
  df_contacts_1_summarized_mean_no_total_pivot <- df_contacts_1_summarized_mean_no_total %>%
    pivot_longer(!tick, names_to = "type", values_to = "count")
  
  
  #days
  df_contacts_1_day <- df_contacts_1_summarized_mean
  df_contacts_1_day$day <- dmfConvertTicksToDay(df_contacts_1_day$tick)
  df_contacts_1_day_sum_mean_nototal <- df_contacts_1_day %>% group_by(day) %>% summarise_all(
    list(max = max) # max as "doing something today" should be 1, not 0.25
  )  %>% select(-"contacts_last_tick_mean_max", -"tick_max") %>%
    rename_with( ~ gsub("contacts_in_", "", .x, fixed = TRUE)) %>%
    rename_with( ~ gsub("_mean_max", "", .x, fixed = TRUE))
  
  df_contacts_1_day_sum_mean_nototall_pivot <- df_contacts_1_day_sum_mean_nototal %>%
    pivot_longer(!day, names_to = "type", values_to = "count")
  

  #Scenario 2 --------------------------------------------
  df_contacts_2 <- df_contacts %>% filter(Scenario == "2-infections") 
  
  df_contacts_2_summarized <- df_contacts_2 %>% group_by(tick) %>% select(starts_with("contacts")) %>% summarise_all(
    list(mean = mean, sd = sd, min = min, max = max)
  )
  
  df_contacts_2_summarized_mean <- df_contacts_2 %>% group_by(tick) %>% select(starts_with("contacts")) %>% summarise_all(
    list(mean = mean)
  )
  
  df_contacts_2_summarized_mean_no_total <- df_contacts_2_summarized_mean %>% select(-"contacts_last_tick_mean")
  
  df_contacts_2_summarized_mean_no_total <- df_contacts_2_summarized_mean_no_total %>% rename_with( ~ gsub("contacts_in_", "", .x, fixed = TRUE))
  df_contacts_2_summarized_mean_no_total <- df_contacts_2_summarized_mean_no_total %>% rename_with( ~ gsub("_mean", "", .x, fixed = TRUE))
  
  df_contacts_2_summarized_mean_no_total_pivot <- df_contacts_2_summarized_mean_no_total %>%
    pivot_longer(!tick, names_to = "type", values_to = "count")
  
  
  #days
  df_contacts_2_day <- df_contacts_2_summarized_mean
  df_contacts_2_day$day <- dmfConvertTicksToDay(df_contacts_2_day$tick)
  df_contacts_2_day_sum_mean_nototal <- df_contacts_2_day %>% group_by(day) %>% summarise_all(
    list(max = max) # max as "doing something today" should be 2, not 0.25
  )  %>% select(-"contacts_last_tick_mean_max", -"tick_max") %>%
    rename_with( ~ gsub("contacts_in_", "", .x, fixed = TRUE)) %>%
    rename_with( ~ gsub("_mean_max", "", .x, fixed = TRUE))
  
  df_contacts_2_day_sum_mean_nototall_pivot <- df_contacts_2_day_sum_mean_nototal %>%
    pivot_longer(!day, names_to = "type", values_to = "count")
  
  
  #Scenario 3 --------------------------------------------
  df_contacts_3 <- df_contacts %>% filter(Scenario == "3-lockdown") 
  
  df_contacts_3_summarized <- df_contacts_3 %>% group_by(tick) %>% select(starts_with("contacts")) %>% summarise_all(
    list(mean = mean, sd = sd, min = min, max = max)
  )
  
  df_contacts_3_summarized_mean <- df_contacts_3 %>% group_by(tick) %>% select(starts_with("contacts")) %>% summarise_all(
    list(mean = mean)
  )
  
  df_contacts_3_summarized_mean_no_total <- df_contacts_3_summarized_mean %>% select(-"contacts_last_tick_mean")
  
  df_contacts_3_summarized_mean_no_total <- df_contacts_3_summarized_mean_no_total %>% rename_with( ~ gsub("contacts_in_", "", .x, fixed = TRUE))
  df_contacts_3_summarized_mean_no_total <- df_contacts_3_summarized_mean_no_total %>% rename_with( ~ gsub("_mean", "", .x, fixed = TRUE))
  
  df_contacts_3_summarized_mean_no_total_pivot <- df_contacts_3_summarized_mean_no_total %>%
    pivot_longer(!tick, names_to = "type", values_to = "count")
  
  
  #days
  df_contacts_3_day <- df_contacts_3_summarized_mean
  df_contacts_3_day$day <- dmfConvertTicksToDay(df_contacts_3_day$tick)
  df_contacts_3_day_sum_mean_nototal <- df_contacts_3_day %>% group_by(day) %>% summarise_all(
    list(max = max) # max as "doing something today" should be 3, not 0.35
  )  %>% select(-"contacts_last_tick_mean_max", -"tick_max") %>%
    rename_with( ~ gsub("contacts_in_", "", .x, fixed = TRUE)) %>%
    rename_with( ~ gsub("_mean_max", "", .x, fixed = TRUE))
  
  df_contacts_3_day_sum_mean_nototall_pivot <- df_contacts_3_day_sum_mean_nototal %>%
    pivot_longer(!day, names_to = "type", values_to = "count")
  
  #Scenario 4 --------------------------------------------
  df_contacts_4 <- df_contacts %>% filter(Scenario == "4-lockdown-wages") 
  
  df_contacts_4_summarized <- df_contacts_4 %>% group_by(tick) %>% select(starts_with("contacts")) %>% summarise_all(
    list(mean = mean, sd = sd, min = min, max = max)
  )
  
  df_contacts_4_summarized_mean <- df_contacts_4 %>% group_by(tick) %>% select(starts_with("contacts")) %>% summarise_all(
    list(mean = mean)
  )
  
  df_contacts_4_summarized_mean_no_total <- df_contacts_4_summarized_mean %>% select(-"contacts_last_tick_mean")
  
  df_contacts_4_summarized_mean_no_total <- df_contacts_4_summarized_mean_no_total %>% rename_with( ~ gsub("contacts_in_", "", .x, fixed = TRUE))
  df_contacts_4_summarized_mean_no_total <- df_contacts_4_summarized_mean_no_total %>% rename_with( ~ gsub("_mean", "", .x, fixed = TRUE))
  
  df_contacts_4_summarized_mean_no_total_pivot <- df_contacts_4_summarized_mean_no_total %>%
    pivot_longer(!tick, names_to = "type", values_to = "count")
  
  
  #days
  df_contacts_4_day <- df_contacts_4_summarized_mean
  df_contacts_4_day$day <- dmfConvertTicksToDay(df_contacts_4_day$tick)
  df_contacts_4_day_sum_mean_nototal <- df_contacts_4_day %>% group_by(day) %>% summarise_all(
    list(max = max) # max as "doing something today" should be 4, not 0.45
  )  %>% select(-"contacts_last_tick_mean_max", -"tick_max") %>%
    rename_with( ~ gsub("contacts_in_", "", .x, fixed = TRUE)) %>%
    rename_with( ~ gsub("_mean_max", "", .x, fixed = TRUE))
  
  df_contacts_4_day_sum_mean_nototall_pivot <- df_contacts_4_day_sum_mean_nototal %>%
    pivot_longer(!day, names_to = "type", values_to = "count")
  
  # non-essential-shops ------------------------------------------
  df_contacts_NES <- df_contacts %>% select(tick, run_number, Scenario, contacts_in_non_essential_shops)
  
  #df_contacts_NES <- df_contacts_NES %>% filter(Scenario == "1-baseline") 
  
  df_contacts_NES_mean_std <- df_contacts_NES %>% group_by(tick, Scenario) %>%
    summarise(tick, Scenario,
              mean = mean(contacts_in_non_essential_shops)
              ,std = sd(contacts_in_non_essential_shops)
              ,min = min(contacts_in_non_essential_shops)
              ,max = max(contacts_in_non_essential_shops)
    ) %>% unique()
  
  # Add days converted from ticks
  df_contacts_NES_mean_std_day <- df_contacts_NES_mean_std
  df_contacts_NES_mean_std_day$day <- dmfConvertTicksToDay(df_contacts_NES_mean_std_day$tick)
  df_contacts_NES_mean_std_day <- df_contacts_NES_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = max(mean), # max as "doing something today" should be 1, not 0.25
    std = mean(std),
    max = max(max),
    min = max(min)) %>% unique()
  
  # non-essential-shops ------------------------------------------
  df_contacts_ES <- df_contacts %>% select(tick, run_number, Scenario, contacts_in_essential_shops)
  
  #df_contacts_ES <- df_contacts_ES %>% filter(Scenario == "1-baseline") 
  
  df_contacts_ES_mean_std <- df_contacts_ES %>% group_by(tick, Scenario) %>%
    summarise(tick, Scenario,
              mean = mean(contacts_in_essential_shops)
              ,std = sd(contacts_in_essential_shops)
              ,min = min(contacts_in_essential_shops)
              ,max = max(contacts_in_essential_shops)
    ) %>% unique()
  
  # Add days converted from ticks
  df_contacts_ES_mean_std_day <- df_contacts_ES_mean_std
  df_contacts_ES_mean_std_day$day <- dmfConvertTicksToDay(df_contacts_ES_mean_std_day$tick)
  df_contacts_ES_mean_std_day <- df_contacts_ES_mean_std_day %>% group_by(day, Scenario) %>% summarise(
    day, Scenario,
    mean = max(mean), # max as "doing something today" should be 1, not 0.25
    std = mean(std),
    max = max(max),
    min = max(min)) %>% unique()
  
  

  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_contacts_NES_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_contacts_ES_mean_std_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_contacts_1_day_sum_mean_nototall_pivot, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_contacts_2_day_sum_mean_nototall_pivot, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_contacts_3_day_sum_mean_nototall_pivot, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_contacts_4_day_sum_mean_nototall_pivot, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
 
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  print(paste(name, " making plots", sep=""))
  
  
  #----- days --------------------
  dmfPdfOpen(output_dir, "df_contacts_ES_mean_std_day")
  print(plot_ggplot_smooth(df_contacts_ES_mean_std_day, "day", "Essential shops"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "df_contacts_NES_mean_std_day")
  print(plot_ggplot_smooth(df_contacts_NES_mean_std_day, "day", "Non-essential shops"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "df_contacts_1_day_sum_mean_nototall_pivot")
  print(plot_ggplot_stacked_allcontacts(df_contacts_1_day_sum_mean_nototall_pivot, "day", "People, scenario 1"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "df_contacts_2_day_sum_mean_nototall_pivot")
  print(plot_ggplot_stacked_allcontacts(df_contacts_2_day_sum_mean_nototall_pivot, "day", "People, scenario 2"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "df_contacts_3_day_sum_mean_nototall_pivot")
  print(plot_ggplot_stacked_allcontacts(df_contacts_3_day_sum_mean_nototall_pivot, "day", "People, scenario 3"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "df_contacts_4_day_sum_mean_nototall_pivot")
  print(plot_ggplot_stacked_allcontacts(df_contacts_4_day_sum_mean_nototall_pivot, "day", "People, scenario 4"))
  dmfPdfClose()

}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================


plot_ggplot_stacked_allcontacts <- function(data_to_plot, timeframe, type_of_people) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = count,
               fill = type)) +
    geom_area(position = "stack", aes(color = type)) +
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("Contacts") + 
    labs(title=paste(type_of_people, "contacts", sep = " "),
         subtitle="Amount of contacts, stacked", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_fill_viridis_d(option = "D") +
    #scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + 
    theme_bw() + theme(legend.position="right",
                       axis.text = element_text(size = rel(1.3)),
                       axis.title = element_text(size = rel(1.3)),
                       legend.text = element_text(size = rel(1.2)),
                       legend.title = element_text(size = rel(1.2)),
                       title = element_text(size = rel(1.3)) )
}


plot_ggplot_smooth <- function(data_to_plot, timeframe, type_of_people) {
  
  timeframe <- sym(timeframe)
  
  data_to_plot %>%
    ggplot(aes(x = !!timeframe, 
               y = mean)) +
    #geom_line(aes(color = Scenario), size=2,alpha=0.8) + 
    gl_plot_smooth +
    #gl_plot_ribbon_minmax +
    xlab(paste(toupper(substring(timeframe, 1,1)), substring(timeframe, 2), "s", sep = "")) +
    ylab("Contacts") + 
    labs(title=paste(type_of_people, "contacts", sep = " "),
         subtitle="Amount of contacts per scenario, smoothed", 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    #scale_colour_viridis_d(option = "d") +
    scale_color_manual(values = gl_plot_colours) +
    gl_plot_guides + gl_plot_theme
}
