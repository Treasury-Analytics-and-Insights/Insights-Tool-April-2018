#Migration by country of origin for non-NZ migrants
output$mig_all_ext_other <- renderHighchart({
  vt.param_agg_level <- input$mig_all_agg_level
  vt.param_perc <- input$mig_all_param_map_perc
  
  vt.mig_all_col_in <- df.mig_all_flow_type_col_mapping %>%
    filter(FLOW_TYPE == '3') %>% 
    .[["COL_IN"]]
  vt.mig_all_col_out <- df.mig_all_flow_type_col_mapping %>%
    filter(FLOW_TYPE == '3') %>% 
    .[["COL_OUT"]]
  
  if(vt.param_agg_level == "Total NZ"){
    #Plot non-NZ Migration
    tmp.plot_data <- mig_all_panel$base_data %>%
      filter(
        TA_CODE %in% c(999),
        FLOW_TYPE == 3,
        FLOW_DEMOGRAPHIC == "Country",
        FLOW_DEMOGRAPHIC_VALUE != "NZ",
        FLOW_DIR != "net"
      ) %>% 
      mutate(VALUE = ifelse(FLOW_DIR == "in", VALUE, - VALUE)) %>% 
      group_by(FLOW_DIR, FLOW_DEMOGRAPHIC_VALUE, TOTAL_PREV) %>% 
      summarise(VALUE = sum(VALUE)) %>% 
      mutate(COL_VAR = ifelse(FLOW_DIR == "in",
                              vt.mig_all_col_in,
                              vt.mig_all_col_out)) %>% 
      ungroup() %>% 
      inner_join(df.mig_natural_change_flow_dir_mapping %>% 
                   rename(FLOW_DIR_NAME = FLOW_DIR),
                 by = c("FLOW_DIR" = "FLOW_DIR_VALUE")) %>% 
      inner_join(df.region_country_mapping_table,
                 by = c("FLOW_DEMOGRAPHIC_VALUE" = "CODE")) %>% 
      mutate(REGION = factor(REGION, levels = vt.mig_all_region_levels),
             COUNTRY = factor(COUNTRY, levels = vt.mig_all_country_levels)) %>% 
      arrange(FLOW_DIR, REGION, COUNTRY) %>% 
      mutate(LABEL = paste0(COUNTRY, " ", FLOW_DIR_NAME, ": ", comma(abs(VALUE))))
    
    if(!vt.param_perc){
      
      hc <- highchart() %>%
        #Need to add in and out series sepearately to restore stacking behaviour
        #when there is an additional scatter series
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "out"), 
                      "bar",
                      name = "Outflow",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            color = COL_VAR,
                            label = LABEL)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "in"), 
                      "bar",
                      name = "Inflow",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            color = COL_VAR,
                            label = LABEL)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        group_by(REGION, COUNTRY) %>%  
                        summarise(VALUE = sum(VALUE)) %>% 
                        mutate(LABEL =  paste0(COUNTRY, " Net Inflow: ", comma(VALUE))),
                      "scatter",
                      name = "Net Inflow",
                      hcaes(x = COUNTRY,
                            y = VALUE,
                            label = LABEL)) %>% 
        hc_plotOptions(
          bar = list(
            stacking = "normal"
          ),
          scatter = list(
            color = vt.mig_all_col_neutral
          )
        ) %>% 
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in, '#000000')) %>% 
        hc_xAxis(
          categories = list.mig_all_country_region_map,
          labels = list(groupedOptions = list(list(rotation = 0,
                                                   distance = 0,
                                                   x = -2)),
                        rotation = 0,
                        distance = 0,
                        x = -5)
        ) %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label)
    }")) %>%  
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return(Math.abs(this.value))
    }")))
      
    }
    
    if(vt.param_perc){
      
      tmp.plot_data = tmp.plot_data %>% 
        mutate(VALUE_PERC = paste0(formatC(100 * VALUE/TOTAL_PREV,
                                           format = 'f',
                                           digits = 2),
                                   "%"),
               VALUE = VALUE/TOTAL_PREV) %>% 
        mutate(LABEL = paste0(COUNTRY, " ", FLOW_DIR_NAME, ": ", VALUE_PERC))
      
      hc <- highchart() %>%
        #Need to add in and out series sepearately to restore stacking behaviour
        #when there is an additional scatter series
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "out"), 
                      "bar",
                      name = "Outflow",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            color = COL_VAR,
                            label = LABEL)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "in"), 
                      "bar",
                      name = "Inflow",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            color = COL_VAR,
                            label = LABEL)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        group_by(REGION, COUNTRY) %>%  
                        summarise(VALUE = sum(VALUE)) %>% 
                        mutate(LABEL =  paste0(COUNTRY, " Net Inflow: ", paste0(formatC(100 * VALUE,
                                                                                        format = 'f',
                                                                                        digits = 2),
                                                                                "%"))),
                      "scatter",
                      name = "Net Inflow",
                      hcaes(x = COUNTRY,
                            y = VALUE,
                            label = LABEL)) %>% 
        hc_plotOptions(
          bar = list(
            stacking = "normal"
          ),
          scatter = list(
            color = vt.mig_all_col_neutral
          )
        ) %>% 
        hc_xAxis(
          categories = list.mig_all_country_region_map,
          labels = list(groupedOptions = list(list(rotation = 0,
                                                   distance = 0,
                                                   x = -2)),
                        rotation = 0,
                        distance = 0,
                        x = -5)
        ) %>%
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return(Math.abs(100 * this.value).toFixed(2) + '%')
    }"))) %>% 
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in, '#000000')) %>% 
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label)
    }"))
      
    }
    
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    isolate({
      vt.param_ta <- input$mig_all_param_ta
    })
    
    #Plot non-NZ Migration
    tmp.plot_data <- mig_all_panel$base_data %>%
      filter(
        FLOW_TYPE == 3,
        FLOW_DEMOGRAPHIC == "Country",
        FLOW_DEMOGRAPHIC_VALUE != "NZ",
        FLOW_DIR != "net"
      ) %>% 
      mutate(VALUE = ifelse(FLOW_DIR == "in", VALUE, - VALUE)) %>% 
      group_by(FLOW_DIR, FLOW_DEMOGRAPHIC_VALUE, TOTAL_PREV) %>% 
      summarise(VALUE = sum(VALUE)) %>% 
      mutate(COL_VAR = ifelse(FLOW_DIR == "in",
                              vt.mig_all_col_in,
                              vt.mig_all_col_out))  %>%
      inner_join(df.mig_natural_change_flow_dir_mapping %>% 
                   rename(FLOW_DIR_NAME = FLOW_DIR),
                 by = c("FLOW_DIR" = "FLOW_DIR_VALUE")) %>% 
      inner_join(df.region_country_mapping_table,
                 by = c("FLOW_DEMOGRAPHIC_VALUE" = "CODE")) %>% 
      mutate(REGION = factor(REGION, levels = vt.mig_all_region_levels),
             COUNTRY = factor(COUNTRY, levels = vt.mig_all_country_levels)) %>% 
      arrange(FLOW_DIR, REGION, COUNTRY) %>% 
      mutate(LABEL = paste0(COUNTRY, " ", FLOW_DIR_NAME, ": ", comma(abs(VALUE))))
    
    if(!vt.param_perc){
      
      hc <- highchart() %>%
        #Need to add in and out series sepearately to restore stacking behaviour
        #when there is an additional scatter series
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "out"), 
                      "bar",
                      name = "Outflow",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            color = COL_VAR,
                            label = LABEL)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "in"), 
                      "bar",
                      name = "Inflow",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            color = COL_VAR,
                            label = LABEL)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        group_by(REGION, COUNTRY) %>%  
                        summarise(VALUE = sum(VALUE)) %>% 
                        mutate(LABEL =  paste0(COUNTRY, " Net Inflow: ", comma(VALUE))),
                      name = "Net Inflow",
                      "scatter",
                      hcaes(x = COUNTRY,
                            y = VALUE,
                            label = LABEL)) %>% 
        hc_plotOptions(
          bar = list(
            stacking = "normal"
          ),
          scatter = list(
            color = vt.mig_all_col_neutral
          )
        ) %>% 
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in, '#000000')) %>% 
        hc_xAxis(
          categories = list.mig_all_country_region_map,
          labels = list(groupedOptions = list(list(rotation = 0,
                                                   distance = 0,
                                                   x = -2)),
                        rotation = 0,
                        distance = 0,
                        x = -5)
        ) %>%
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return(Math.abs(this.value))
    }"))) %>% 
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label)
    }"))
      
    }
    
    if(vt.param_perc){
      
      tmp.plot_data = tmp.plot_data %>% 
        mutate(VALUE_PERC = paste0(formatC(100 * VALUE/TOTAL_PREV,
                                           format = 'f',
                                           digits = 2),
                                   "%"),
               VALUE = VALUE/TOTAL_PREV) %>% 
        mutate(LABEL = paste0(COUNTRY, " ", FLOW_DIR_NAME, ": ", VALUE_PERC))
      
      hc <- highchart() %>%
        #Need to add in and out series sepearately to restore stacking behaviour
        #when there is an additional scatter series
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "out"), 
                      "bar",
                      name = "Outflow",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            color = COL_VAR,
                            label = LABEL)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "in"), 
                      "bar",
                      name = "Inflow",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            color = COL_VAR,
                            label = LABEL)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        group_by(REGION, COUNTRY) %>%  
                        summarise(VALUE = sum(VALUE)) %>% 
                        mutate(LABEL =  paste0(COUNTRY, " Net Inflow: ", paste0(formatC(100 * VALUE,
                                                                                        format = 'f',
                                                                                        digits = 2),
                                                                                "%"))),
                      "scatter",
                      name = "Net Inflow",
                      hcaes(x = COUNTRY,
                            y = VALUE,
                            label = LABEL)) %>% 
        hc_plotOptions(
          bar = list(
            stacking = "normal"
          ),
          scatter = list(
            color = vt.mig_all_col_neutral
          )
        ) %>% 
        hc_xAxis(
          categories = list.mig_all_country_region_map,
          labels = list(groupedOptions = list(list(rotation = 0,
                                                   distance = 0,
                                                   x = -2)),
                        rotation = 0,
                        distance = 0,
                        x = -5)
        ) %>%
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return(Math.abs(100 * this.value).toFixed(2) + '%')
    }"))) %>% 
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in, '#000000')) %>% 
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label)
    }"))
      
    }
  }
  
  hc <- hc %>% 
    hc_xAxis(tickAmount = n_distinct(tmp.plot_data$COUNTRY))
  
  return(hc)
  
})

#Migration by country of origin aggregated to NZ/All other
output$mig_all_ext_nz <- renderHighchart({
  vt.param_agg_level <- input$mig_all_agg_level
  vt.param_perc <- input$mig_all_param_map_perc
  vt.param_start_year <- input$mig_all_param_year[1]
  vt.param_end_year <- input$mig_all_param_year[2]

  validate(need(vt.mig_all_year_combn %>% 
                  filter(PREV_YEAR %in% vt.param_start_year, CURR_YEAR %in% vt.param_end_year) %>%
                  dim() %>%
                  .[1] >= 1,
                "Updating..."))
  
  vt.mig_all_col_in <- df.mig_all_flow_type_col_mapping %>%
    filter(FLOW_TYPE == '3') %>% 
    .[["COL_IN"]]
  vt.mig_all_col_out <- df.mig_all_flow_type_col_mapping %>%
    filter(FLOW_TYPE == '3') %>% 
    .[["COL_OUT"]]
  
  if(vt.param_agg_level == "Total NZ"){
    
    #Plot non-NZ Migration
    tmp.plot_data <- mig_all_panel$base_data %>%
      filter(
        TA_CODE %in% c(999),
        FLOW_TYPE == 3,
        FLOW_DEMOGRAPHIC == "Country",
        FLOW_DIR != "net"
      ) %>% 
      mutate(VALUE = ifelse(FLOW_DIR == "in", VALUE, - VALUE),
             ALL_OTHER = ifelse(FLOW_DEMOGRAPHIC_VALUE == "NZ",
                                "NZ",
                                "All Other")) %>% 
      group_by(FLOW_DIR, ALL_OTHER, TOTAL_PREV) %>% 
      summarise(VALUE = sum(VALUE)) %>% 
      mutate(COL_VAR = ifelse(FLOW_DIR == "in",
                              vt.mig_all_col_in,
                              vt.mig_all_col_out)) %>% 
      ungroup()
    
    tmp.plot_data <- tmp.plot_data %>% 
      bind_rows(tmp.plot_data %>%
                  group_by(FLOW_DIR, COL_VAR, TOTAL_PREV) %>% 
                  summarise(VALUE = sum(VALUE)) %>% 
                  mutate(ALL_OTHER = "Total")) %>% 
      mutate(ALL_OTHER = factor(ALL_OTHER,
                                levels = c("NZ", "All Other", "Total"))) %>% 
      arrange(ALL_OTHER, FLOW_DIR)
    
    title_str <- glue("International migration by country of origin from {vt.param_start_year} to {vt.param_end_year}, Total NZ")
    
    if(!vt.param_perc){
      
      hc <- highchart() %>%
        #Need to add in and out series sepearately to restore stacking behaviour
        #when there is an additional scatter series
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "out"), 
                      "bar",
                      name = "Outflow",
                      hcaes(x = ALL_OTHER,
                            y = VALUE,
                            color = COL_VAR)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "in"), 
                      "bar",
                      name = "Inflow",
                      hcaes(x = ALL_OTHER,
                            y = VALUE,
                            color = COL_VAR)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        group_by(ALL_OTHER) %>%  
                        summarise(VALUE = sum(VALUE)),
                      "scatter",
                      name = "Net Inflow",
                      hcaes(x = ALL_OTHER,
                            y = VALUE)) %>% 
        hc_plotOptions(
          bar = list(
            stacking = "normal"
          ),
          scatter = list(
            color = vt.mig_all_col_neutral
          )
        ) %>% 
        hc_xAxis(
          categories = unique(tmp.plot_data$ALL_OTHER),
          tickAmount = n_distinct(tmp.plot_data$ALL_OTHER)
        ) %>% 
        hc_title(text = title_str,
			   style= list(fontSize="14px", fontFamily="Helvetica")) %>% 
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in, '#000000')) %>% 
        hc_tooltip(formatter = JS("function(){
                                return(this.series.name + ': ' + Math.abs(this.y))
    }")) %>%
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return(Math.abs(this.value))
        }"),
                               rotation = 0))
      
    }
    
    if(vt.param_perc){
      
      tmp.plot_data = tmp.plot_data %>% 
        mutate(VALUE_PERC = paste0(formatC(100 * VALUE/TOTAL_PREV,
                                           format = 'f',
                                           digits = 1),
                                   "%"),
               VALUE = VALUE/TOTAL_PREV)
      
      hc <- highchart() %>%
        #Need to add in and out series sepearately to restore stacking behaviour
        #when there is an additional scatter series
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "out"), 
                      "bar",
                      name = "Outflow",
                      hcaes(x = ALL_OTHER,
                            y = VALUE,
                            color = COL_VAR)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "in"), 
                      "bar",
                      name = "Inflow",
                      hcaes(x = ALL_OTHER,
                            y = VALUE,
                            color = COL_VAR)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        group_by(ALL_OTHER) %>%  
                        summarise(VALUE = sum(VALUE)),
                      "scatter",
                      name = "Net Inflow",
                      hcaes(x = ALL_OTHER,
                            y = VALUE)) %>% 
        hc_plotOptions(
          bar = list(
            stacking = "normal"
          ),
          scatter = list(
            color = vt.mig_all_col_neutral
          )
        ) %>% 
        hc_xAxis(
          categories = unique(tmp.plot_data$ALL_OTHER),
          tickAmount = n_distinct(tmp.plot_data$ALL_OTHER)
        ) %>%
        hc_yAxis(labels = list(formatter = JS("function(){
                                                return(Math.abs(100 * this.value).toFixed(2) + '%')
                                                }"),
                               rotation = 0)) %>% 
        hc_title(text = title_str,
			   style= list(fontSize="14px", fontFamily="Helvetica")) %>% 
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in, '#000000')) %>% 
        hc_tooltip(formatter = JS("function(){
                                    return(this.series.name + ': ' + (100 * this.y).toFixed(2) + '%')
          }"))
    }
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    isolate({
      vt.param_ta <- input$mig_all_param_ta
    })
    
    title_str <- glue("International migration by country of origin from {vt.param_start_year} to {vt.param_end_year}, {vt.param_ta}")
    
    
    #Plot non-NZ Migration
    tmp.plot_data <- mig_all_panel$base_data %>%
      filter(
        FLOW_TYPE == 3,
        FLOW_DEMOGRAPHIC == "Country",
        FLOW_DIR != "net"
      ) %>% 
      mutate(VALUE = ifelse(FLOW_DIR == "in", VALUE, - VALUE),
             ALL_OTHER = ifelse(FLOW_DEMOGRAPHIC_VALUE == "NZ",
                                "NZ",
                                "All Other")) %>% 
      group_by(FLOW_DIR, ALL_OTHER, TOTAL_PREV) %>% 
      summarise(VALUE = sum(VALUE)) %>% 
      mutate(COL_VAR = ifelse(FLOW_DIR == "in",
                              vt.mig_all_col_in,
                              vt.mig_all_col_out)) %>%
      ungroup()
    
    
    tmp.plot_data <- tmp.plot_data %>% 
      bind_rows(tmp.plot_data %>%
                  group_by(FLOW_DIR, COL_VAR, TOTAL_PREV) %>% 
                  summarise(VALUE = sum(VALUE)) %>% 
                  mutate(ALL_OTHER = "Total")) %>% 
      mutate(ALL_OTHER = factor(ALL_OTHER,
                                levels = c("NZ", "All Other", "Total"))) %>% 
      arrange(ALL_OTHER, FLOW_DIR)
    
    if(!vt.param_perc){
      
      hc <- highchart() %>%
        #Need to add in and out series sepearately to restore stacking behaviour
        #when there is an additional scatter series
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "out"), 
                      "bar",
                      name = 'Outflow',
                      hcaes(x = ALL_OTHER,
                            y = VALUE,
                            color = COL_VAR
                      )) %>% 
        hc_add_series(data = tmp.plot_data %>% 
                        filter(FLOW_DIR == "in"), 
                      "bar",
                      name = 'Inflow',
                      hcaes(x = ALL_OTHER,
                            y = VALUE,
                            color = COL_VAR)) %>% 
        hc_add_series(tmp.plot_data %>%
                        group_by(ALL_OTHER) %>%  
                        summarise(VALUE = sum(VALUE)),
                      "scatter",
                      name = 'Net Inflow',
                      hcaes(x = ALL_OTHER,
                            y = VALUE)) %>% 
        hc_plotOptions(
          bar = list(
            stacking = "normal"
          ),
          scatter = list(
            color = vt.mig_all_col_neutral
          )
        ) %>% 
        hc_xAxis(
          categories = unique(tmp.plot_data$ALL_OTHER),
          tickAmount = n_distinct(tmp.plot_data$ALL_OTHER)
        ) %>% 
        hc_title(text = title_str,
			   style= list(fontSize="14px", fontFamily="Helvetica")) %>% 
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in, '#000000')) %>% 
        hc_tooltip(formatter = JS("function(){
                                  return(this.series.name + ': ' + Math.abs(this.y))
    }")) %>%
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return(Math.abs(this.value))
        }"),
                               rotation = 0))
      
    }
    
    if(vt.param_perc){
      
      tmp.plot_data = tmp.plot_data %>% 
        mutate(VALUE_PERC = paste0(formatC(100 * VALUE/TOTAL_PREV,
                                           format = 'f',
                                           digits = 1),
                                   "%"),
               VALUE = VALUE/TOTAL_PREV)
      
      hc <- highchart() %>%
        #Need to add in and out series sepearately to restore stacking behaviour
        #when there is an additional scatter series
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "out"), 
                      "bar",
                      name = "Outflow",
                      hcaes(x = ALL_OTHER,
                            y = VALUE,
                            color = COL_VAR)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        filter(FLOW_DIR == "in"), 
                      "bar",
                      name = "Inflow",
                      hcaes(x = ALL_OTHER,
                            y = VALUE,
                            color = COL_VAR)) %>% 
        hc_add_series(tmp.plot_data %>% 
                        group_by(ALL_OTHER) %>%  
                        summarise(VALUE = sum(VALUE)),
                      "scatter",
                      name = "Net Inflow",
                      hcaes(x = ALL_OTHER,
                            y = VALUE)) %>% 
        hc_plotOptions(
          bar = list(
            stacking = "normal"
          ),
          scatter = list(
            color = vt.mig_all_col_neutral
          )
        ) %>% 
        hc_xAxis(
          categories = unique(tmp.plot_data$ALL_OTHER),
          tickAmount = n_distinct(tmp.plot_data$ALL_OTHER)
        ) %>%
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return(Math.abs(100 * this.value).toFixed(2) + '%')
                                              }"),
                               rotation = 0)) %>% 
        hc_title(text =  title_str,
			   style= list(fontSize="14px", fontFamily="Helvetica")) %>% 
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in, '#000000')) %>% 
        hc_tooltip(formatter = JS("function(){
                                  return(this.series.name + ': ' + Math.abs(100 * this.y).toFixed(2) + '%')
        }"))
    }
  }
  return(hc)
})

#Population pyramid showing the effects of different kinds of migration
output$mig_all_popn_changes <- renderHighchart({
  
  isolate({
    vt.param_agg_level <- input$mig_all_agg_level
    vt.param_ta <- input$mig_all_param_ta
  })
  
  mig_all_panel$base_data
  vt.param_perc <- input$mig_all_param_map_perc    
  vt.param_start_year <- input$mig_all_param_year[1]
  vt.param_end_year <- input$mig_all_param_year[2]
  
  validate(need(vt.mig_all_year_combn %>% 
                  filter(PREV_YEAR %in% vt.param_start_year, CURR_YEAR %in% vt.param_end_year) %>%
                  dim() %>%
                  .[1] >= 1,
                "Updating..."))
  
  
  if(vt.param_agg_level == "Total NZ"){
    
    vt.title_str <- glue("The age profile of NZ in {vt.param_start_year} and {vt.param_end_year}, and the effects of migration, ageing, and natural increase")
    
    #Total Popn age pyramid data
    #Here tot prev_age outflow is the previous population and their age at the start year
    #tot curr_age inflow is the current population and their age at the end year
    tmp.plot_data1 <-  mig_all_panel$base_data %>%
      filter(
        FLOW_DIR != "net",
        FLOW_TYPE == "tot",
        FLOW_DEMOGRAPHIC == "Previous Age" & FLOW_DIR == "out" |
          FLOW_DEMOGRAPHIC == "Current Age" & FLOW_DIR == "in",
        TA_CODE == 999
      ) %>% 
      mutate(
        POPN_PERC =  100 * VALUE / TOTAL_PREV,
        x_test = paste0(FLOW_DEMOGRAPHIC_VALUE, FLOW_DIR),
        LABEL = ifelse(FLOW_DIR == "in",
                       paste(CURR_YEAR, "Population"),
                       paste(PREV_YEAR, "Population")),
        COL_VAR = case_when(.$FLOW_DIR == "in" ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == 'tot') %>% 
                              .[["COL_IN"]],
                            T ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == 'tot') %>% 
                              .[["COL_OUT"]])
      )
    
    #International migrant popn data
    tmp.plot_data3 <-  mig_all_panel$base_data %>%
      filter(
        FLOW_TYPE == 3,
        FLOW_DIR != "net",
        FLOW_DEMOGRAPHIC == "Previous Age" & FLOW_DIR == "out" |
          FLOW_DEMOGRAPHIC == "Current Age" & FLOW_DIR == "in",
        TA_CODE == 999
      ) %>% 
      mutate(
        POPN_PERC =  100 * VALUE / TOTAL_PREV,
        x_test = paste0(FLOW_DEMOGRAPHIC_VALUE, FLOW_DIR),
        LABEL = ifelse(FLOW_DIR == "in",
                       paste(FLOW_DEMOGRAPHIC_VALUE, "Immigration"),
                       paste(FLOW_DEMOGRAPHIC_VALUE, "Emmigration")),
        COL_VAR = case_when(.$FLOW_DIR == "in" ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == '3') %>% 
                              .[["COL_IN"]],
                            T ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == '3') %>% 
                              .[["COL_OUT"]])
      )
    
    #Effect of natural change including aging
    #Calculated as births - deaths + (out_1_curr_age - out_1_prev_age)
    tmp.plot_data4 <-  mig_all_panel$base_data %>%
      filter(
        FLOW_TYPE %in% c(1,4),
        FLOW_DIR != "net",
        (FLOW_DEMOGRAPHIC == "Previous Age" & FLOW_DIR == "out") |
          (FLOW_DEMOGRAPHIC == "Current Age" & (FLOW_DIR == "in" | FLOW_TYPE == 1)),
        TA_CODE == 999
      ) %>% 
      mutate(VALUE = case_when(.$FLOW_TYPE == 4 & .$FLOW_DIR == 'in' ~ .$VALUE,
                               .$FLOW_TYPE == 4 & .$FLOW_DIR == 'out' ~ -.$VALUE,
                               .$FLOW_TYPE == 1 & .$FLOW_DEMOGRAPHIC == "Current Age" ~ .$VALUE,
                               .$FLOW_TYPE == 1 & .$FLOW_DEMOGRAPHIC == "Previous Age" ~ -.$VALUE)) %>%
      group_by(FLOW_DEMOGRAPHIC_VALUE, TOTAL_PREV) %>%
      summarise(VALUE = sum(VALUE)) %>% 
      #Pseudo Flow Dir to reuse the same adjustment technique  
      ungroup %>%
      mutate(FLOW_DIR = ifelse(VALUE < 0, 'out', 'in')) %>%
      mutate(
        POPN_PERC = 100 * VALUE / TOTAL_PREV,
        COL_VAR = case_when(.$FLOW_DIR == "in" ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == '4') %>% 
                              .[["COL_IN"]],
                            .$FLOW_DIR == "out" ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == '4') %>% 
                              .[["COL_OUT"]])
      )
    
    list.plot_df <- list(tmp.plot_data1, tmp.plot_data3)
    
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    vt.title_str <- glue("The age profile of {vt.param_ta} in {vt.param_start_year} and {vt.param_end_year}, and the effects of migration, ageing, and natural increase")
    
    #Total Popn age pyramid data
    tmp.plot_data1 <-  mig_all_panel$base_data %>%
      filter(
        FLOW_TYPE == "tot",
        FLOW_DIR != "net",
        FLOW_DEMOGRAPHIC == "Previous Age" & FLOW_DIR == "out" |
          FLOW_DEMOGRAPHIC == "Current Age" & FLOW_DIR == "in",
        TA == vt.param_ta
      ) %>% 
      mutate(
        POPN_PERC = 100 * VALUE / TOTAL_PREV,
        x_test = paste0(FLOW_DEMOGRAPHIC_VALUE, FLOW_DIR),
        LABEL = ifelse(FLOW_DIR == "in",
                       paste(CURR_YEAR, "Population"),
                       paste(PREV_YEAR, "Population")),
        COL_VAR = case_when(.$FLOW_DIR == "in" ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == 'tot') %>% 
                              .[["COL_IN"]],
                            T ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == 'tot') %>% 
                              .[["COL_OUT"]])
      )
    
    #Migration within NZ migrant popn data
    tmp.plot_data2 <-  mig_all_panel$base_data %>%
      filter(
        FLOW_TYPE == 2,
        FLOW_DIR != "net",
        FLOW_DEMOGRAPHIC == "Previous Age" & FLOW_DIR == "out" |
          FLOW_DEMOGRAPHIC == "Current Age" & FLOW_DIR == "in",
        TA == vt.param_ta
      ) %>% 
      mutate(
        POPN_PERC = 100 * VALUE / TOTAL_PREV,
        x_test = paste0(FLOW_DEMOGRAPHIC_VALUE, FLOW_DIR),
        LABEL = ifelse(FLOW_DIR == "in",
                       paste(FLOW_DEMOGRAPHIC_VALUE, "Migration within NZ inflow"),
                       paste(FLOW_DEMOGRAPHIC_VALUE, "Migration within NZ outflow")),
        COL_VAR = case_when(.$FLOW_DIR == "in" ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == '2') %>% 
                              .[["COL_IN"]],
                            T ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == '2') %>% 
                              .[["COL_OUT"]])
      )
    
    #International migrant popn data
    tmp.plot_data3 <-  mig_all_panel$base_data %>%
      filter(
        FLOW_TYPE == 3,
        FLOW_DIR != "net",
        FLOW_DEMOGRAPHIC == "Previous Age" & FLOW_DIR == "out" |
          FLOW_DEMOGRAPHIC == "Current Age" & FLOW_DIR == "in",
        TA == vt.param_ta
      ) %>% 
      mutate(
        POPN_PERC = 100 * VALUE / TOTAL_PREV,
        x_test = paste0(FLOW_DEMOGRAPHIC_VALUE, FLOW_DIR),
        LABEL = ifelse(FLOW_DIR == "in",
                       paste(FLOW_DEMOGRAPHIC_VALUE, "Immigration"),
                       paste(FLOW_DEMOGRAPHIC_VALUE, "Emmigration")),
        COL_VAR = case_when(.$FLOW_DIR == "in" ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == '3') %>% 
                              .[["COL_IN"]],
                            T ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == '3') %>% 
                              .[["COL_OUT"]])
      )
    
    #Effect of natural change including aging
    # Calculated as births - deaths + (out_1_curr_age - out_1_prev_age)
    tmp.plot_data4 <-  mig_all_panel$base_data %>%
      filter(
        FLOW_TYPE %in% c(1,4),
        FLOW_DIR != "net",
        (FLOW_DEMOGRAPHIC == "Previous Age" & FLOW_DIR == "out") |
          (FLOW_DEMOGRAPHIC == "Current Age" & (FLOW_DIR == "in" | FLOW_TYPE == 1)),
        TA == vt.param_ta
      ) %>% 
      mutate(VALUE = case_when(.$FLOW_TYPE == 4 & .$FLOW_DIR == 'in' ~ .$VALUE,
                               .$FLOW_TYPE == 4 & .$FLOW_DIR == 'out' ~ -.$VALUE,
                               .$FLOW_TYPE == 1 & .$FLOW_DEMOGRAPHIC == "Current Age" ~ .$VALUE,
                               .$FLOW_TYPE == 1 & .$FLOW_DEMOGRAPHIC == "Previous Age" ~ -.$VALUE)) %>%
      group_by(FLOW_DEMOGRAPHIC_VALUE, TOTAL_PREV) %>%
      summarise(VALUE = sum(VALUE)) %>% 
      ungroup %>% 
      #Pseudo Flow Dir to reuse the same adjustment technique  
      mutate(FLOW_DIR = ifelse(VALUE < 0, 'out', 'in')) %>%   
      mutate(
        POPN_PERC = 100 * VALUE / TOTAL_PREV,
        COL_VAR = case_when(.$FLOW_DIR == "in" ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == '4') %>% 
                              .[["COL_IN"]],
                            T ~ df.mig_all_flow_type_col_mapping  %>% 
                              filter(FLOW_TYPE == '4') %>% 
                              .[["COL_OUT"]])
      ) 
    
    list.plot_df <- list(tmp.plot_data1, 
                         tmp.plot_data2,
                         tmp.plot_data3)
  }
  
  if(!vt.param_perc){
    
    df.hc_adjustment <- lapply(c(list.plot_df, list(tmp.plot_data4)),
                               function(x){
                                 tmp.left_vector <- x %>% 
                                   filter(FLOW_DIR == 'out') %>% 
                                   .[["VALUE"]] %>% 
                                   abs()
                                 tmp.right_vector <- x %>% 
                                   filter(FLOW_DIR == 'in') %>% 
                                   .[["VALUE"]] %>% 
                                   abs()
                                 
                                 data_frame(left_value = ifelse(length(tmp.left_vector) == 0,
                                                                0,
                                                                max(tmp.left_vector)),
                                            right_value = ifelse(length(tmp.right_vector) == 0,
                                                                 0,
                                                                 max(tmp.right_vector)))
                               }) %>% 
      bind_rows() %>% 
      mutate(shift_value = lag(right_value + lead(left_value))) %>% 
      mutate(shift_value = ifelse(is.na(shift_value), 0 , shift_value)) %>% 
      mutate(shift_value = cumsum(shift_value))
    
    if(vt.param_agg_level == "Total NZ"){
      
      df.hc_labels <- data_frame(list_pos = 1:2,
                                 name_right = c(glue("{vt.param_end_year} Population"),  "International Inflow"),
                                 name_left = c(glue("{vt.param_start_year} Population"),  "International Outflow"),
                                 name_net = c("Population Change",  "International Migration Net Inflow"))
      
    }
    
    if(vt.param_agg_level == "Territorial authority"){
      
      df.hc_labels <- data_frame(list_pos = 1:4,
                                 name_right = c(glue("{vt.param_end_year} Population"), "Migration within NZ Inflow", "International Inflow", "Births"),
                                 name_left = c(glue("{vt.param_start_year} Population"), "Migration within NZ Outflow", "International Outflow", "Deaths"),
                                 name_net = c("Population Change", "Migration within NZ Net Inflow", "International Migration Net Inflow", "Natural increase and ageing"))
      
    }
    
    #Have to define colour character vector for hc legend as the legend can't inherit from color aesthetic
    #because natural change has two colours for one series
    tmp.hc_colour <- character(0)
    
    hc <- highchart()
    
    #Add the in-out series
    for(i in 1:(length(list.plot_df))){
      
      hc = hc %>% 
        hc_add_series(data = list.plot_df[[i]] %>%
                        filter(FLOW_DIR == 'out')  %>% 
                        mutate(HIGH_VALUE = df.hc_adjustment[[i,'shift_value']],
                               NAME = df.hc_labels[[i,'name_left']]),
                      name = df.hc_labels[[i,'name_left']],
                      type = 'columnrange',
                      id = paste0('in', i),
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            low = HIGH_VALUE - VALUE,
                            high = HIGH_VALUE,
                            color = COL_VAR,
                            label = paste0(NAME, ": ", scales::comma(VALUE))
                      )) %>% 
        hc_add_series(data = list.plot_df[[i]] %>%
                        filter(FLOW_DIR == 'in')  %>% 
                        mutate(LOW_VALUE = df.hc_adjustment[[i,'shift_value']],
                               NAME = df.hc_labels[[i,'name_right']]),
                      name = df.hc_labels[[i,'name_right']],
                      type = 'columnrange',
                      id = 'out',
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            low = LOW_VALUE,
                            high = LOW_VALUE + VALUE,
                            color = COL_VAR,
                            label = paste0(NAME, ": ", scales::comma(VALUE))
                      )) %>%
        hc_add_series(data = list.plot_df[[i]] %>% 
                        group_by(FLOW_DEMOGRAPHIC_VALUE) %>% 
                        summarise(VALUE = sum(ifelse(FLOW_DIR == 'in', VALUE, -VALUE))) %>%
                        mutate(
                          LABEL = paste(FLOW_DEMOGRAPHIC_VALUE, "- Net Inflow"),
                          NAME = df.hc_labels[[i,'name_net']],
                          COL_VAR = vt.mig_all_col_neutral,
                          VALUE_pos = VALUE + df.hc_adjustment[[i,'shift_value']]),
                      name = df.hc_labels[[i,'name_net']],
                      type = "line",
                      id = 'net',
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE_pos,
                            label = paste0(NAME, ": ", scales::comma(VALUE)),
                            color = COL_VAR,
                            value = VALUE))
      
      tmp.hc_colour <- c(tmp.hc_colour, 
                         list.plot_df[[i]] %>% 
                           filter(FLOW_DIR == 'out') %>% 
                           .[["COL_VAR"]] %>% 
                           unique,
                         list.plot_df[[i]] %>% 
                           filter(FLOW_DIR == 'in') %>% 
                           .[["COL_VAR"]] %>% 
                           unique,
                         vt.mig_all_col_neutral)
      
    }
    
    #Add the natural change bar
    vt.natural_change_bar_adjust <- df.hc_adjustment %>% 
      .[['shift_value']] %>% 
      max()
    
    hc = hc %>% 
      hc_add_series(type = "columnrange",
                    name = "Natural increase and ageing",
                    data = tmp.plot_data4 %>% 
                      group_by(FLOW_DEMOGRAPHIC_VALUE) %>% 
                      mutate(NAME = "Natural increase and ageing",
                             low = min(VALUE + vt.natural_change_bar_adjust, 
                                       vt.natural_change_bar_adjust),
                             high = max(VALUE + vt.natural_change_bar_adjust, 
                                        vt.natural_change_bar_adjust)),
                    hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                          low = low,
                          high = high,
                          name = NAME,
                          color = COL_VAR,
                          label = paste0(NAME, ": ", scales::comma(VALUE)),
                          value = VALUE))
    
    tmp.hc_colour <- c(tmp.hc_colour, df.mig_all_flow_type_col_mapping %>% 
                         filter(FLOW_TYPE == 4) %>% 
                         .[["COL_IN"]])
    
    hc = hc %>% 
      hc_plotOptions(columnrange = list(stacking = 'normal'),
                     line = list(lineWidth = 0,
                                 states = list(hover = list(lineWidthPlus = 0)))) %>%
      hc_chart(inverted = T) %>% 
      hc_xAxis(
        categories = tmp.plot_data1[["FLOW_DEMOGRAPHIC_VALUE"]],
        reversed = F,
        labels = list(enabled = T,
                      rotation = 0,
                      autoRotation = F),
        minorTickLength = 1,
        tickLength = 1,
        lineWidth = 1,
        minorGridLineWidth = 1,
        lineColor = 'transparent'
      ) %>% 
      hc_yAxis(visible = F) %>% 
      hc_tooltip(
        shared = T,
        pointFormatter = JS("function(){
                       return(this.label + '<br>')
                            }")
      ) %>% 
      hc_colors(colors = tmp.hc_colour) %>% 
      hc_title(text = vt.title_str,
			   style= list(fontSize="14px", fontFamily="Helvetica"))
    
  }
  
  if(vt.param_perc){
    
    df.hc_adjustment <- lapply(c(list.plot_df, list(tmp.plot_data4)),
                               function(x){
                                 data_frame(left_value = x %>% 
                                              filter(FLOW_DIR == 'out') %>% 
                                              .[["POPN_PERC"]] %>% 
                                              max() %>% 
                                              abs(),
                                            right_value = x %>% 
                                              filter(FLOW_DIR == 'in') %>% 
                                              .[["POPN_PERC"]] %>% 
                                              max() %>% 
                                              abs())
                               }) %>% 
      bind_rows() %>% 
      mutate(shift_value = lag(right_value + lead(left_value))) %>% 
      mutate(shift_value = ifelse(is.na(shift_value), 0 , shift_value)) %>% 
      mutate(shift_value = cumsum(shift_value))
    
    if(vt.param_agg_level == "Total NZ"){
      
      df.hc_labels <- data_frame(list_pos = 1:2,
                                 name_right = c(glue("{vt.param_end_year} Population"),  "International Inflow"),
                                 name_left = c(glue("{vt.param_start_year} Population"),  "International Outflow"),
                                 name_net = c("Population Change",  "International Migration Net Inflow"))
      
    }
    
    if(vt.param_agg_level == "Territorial authority"){
      
      df.hc_labels <- data_frame(list_pos = 1:3,
                                 name_right = c(glue("{vt.param_end_year} Population"), "Migration within NZ Inflow", "International Inflow"),
                                 name_left = c(glue("{vt.param_start_year} Population"), "Migration within NZ Outflow", "International Outflow"),
                                 name_net = c("Population Change", "Migration within NZ Migration Net Inflow", "International Migration Net Inflow"))
      
      
    }
    
    #Have to define colour character vector for hc legend as the legend can't inherit from color aesthetic
    #because natural change has two colours for one series
    tmp.hc_colour <- character(0)
    
    hc <- highchart()
    
    for(i in 1:length(list.plot_df)){
      
      hc = hc %>% 
        hc_add_series(data = list.plot_df[[i]] %>%
                        filter(FLOW_DIR == 'out')  %>% 
                        mutate(HIGH_VALUE = df.hc_adjustment[[i,'shift_value']],
                               NAME = df.hc_labels[[i,'name_left']]),
                      name = df.hc_labels[[i,'name_left']],
                      type = 'columnrange',
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            low = HIGH_VALUE - POPN_PERC,
                            high = HIGH_VALUE,
                            color = COL_VAR,
                            label = paste0(NAME, ": ", paste0(formatC(POPN_PERC,
                                                                      format = 'f',
                                                                      digits = 2),
                                                              "%")))) %>% 
        hc_add_series(data = list.plot_df[[i]] %>%
                        filter(FLOW_DIR == 'in')  %>% 
                        mutate(LOW_VALUE = df.hc_adjustment[[i,'shift_value']],
                               NAME = df.hc_labels[[i,'name_right']]),
                      name = df.hc_labels[[i,'name_right']],
                      type = 'columnrange',
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            low = LOW_VALUE,
                            high = LOW_VALUE + POPN_PERC,
                            color = COL_VAR,
                            label = paste0(NAME, ": ", paste0(formatC(POPN_PERC,
                                                                      format = 'f',
                                                                      digits = 2),
                                                              "%")))) %>%
        hc_add_series(data = list.plot_df[[i]] %>% 
                        group_by(FLOW_DEMOGRAPHIC_VALUE) %>% 
                        summarise(POPN_PERC = sum(ifelse(FLOW_DIR == 'in', POPN_PERC, -POPN_PERC))) %>%
                        mutate(LABEL = paste(FLOW_DEMOGRAPHIC_VALUE, "- Net Inflow"),
                               VALUE_pos = POPN_PERC + df.hc_adjustment[[i,'shift_value']],
                               NAME = df.hc_labels[[i,'name_net']],
                               COL_VAR = vt.mig_all_col_neutral),
                      name = df.hc_labels[[i,'name_net']],
                      type = "line",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE_pos,
                            color = COL_VAR,
                            label = paste0(NAME, ": ", paste0(formatC(POPN_PERC,
                                                                      format = 'f',
                                                                      digits = 2),
                                                              "%")),
                            value = POPN_PERC))
      
      tmp.hc_colour <- c(tmp.hc_colour,
                         list.plot_df[[i]] %>% 
                           filter(FLOW_DIR == 'out') %>% 
                           .[["COL_VAR"]] %>% 
                           unique,
                         list.plot_df[[i]] %>% 
                           filter(FLOW_DIR == 'in') %>% 
                           .[["COL_VAR"]] %>% 
                           unique,
                         vt.mig_all_col_neutral)
      
    }
    
    #Add the natural change bar
    vt.natural_change_bar_adjust <- df.hc_adjustment %>% 
      filter(row_number() == max(row_number())) %>% 
      mutate(value = shift_value + right_value) %>% 
      .[["value"]]
    
    tmp.hc_colour <- c(tmp.hc_colour, df.mig_all_flow_type_col_mapping %>% 
                         filter(FLOW_TYPE == 4) %>% 
                         .[["COL_IN"]])
    
    hc = hc %>% 
      hc_add_series(type = "columnrange",
                    name = "Natural increase and ageing",
                    data = tmp.plot_data4 %>% 
                      group_by(FLOW_DEMOGRAPHIC_VALUE) %>% 
                      mutate(NAME = "Natural increase and ageing",
                             low = min(POPN_PERC + vt.natural_change_bar_adjust, 
                                       vt.natural_change_bar_adjust),
                             high = max(POPN_PERC + vt.natural_change_bar_adjust, 
                                        vt.natural_change_bar_adjust)),
                    hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                          low = low,
                          high = high,
                          name = NAME,
                          color = COL_VAR,
                          label = paste0(NAME, ": ", paste0(formatC(POPN_PERC,
                                                                    format = 'f',
                                                                    digits = 2),
                                                            "%"))
                    ))
    
    hc = hc %>% 
      hc_plotOptions(columnrange = list(stacking = 'normal'),
                     line = list(lineWidth = 0,
                                 states = list(hover = list(lineWidthPlus = 0)))) %>%
      hc_chart(inverted = T) %>% 
      hc_xAxis(
        categories = tmp.plot_data1[["FLOW_DEMOGRAPHIC_VALUE"]],
        reversed = F,
        labels = list(enabled = T,
                      rotation = 0,
                      autoRotation = F),
        minorTickLength = 1,
        tickLength = 1,
        lineWidth = 1,
        minorGridLineWidth = 1,
        lineColor = 'transparent'
      ) %>% 
      hc_yAxis(visible = F) %>% 
      hc_tooltip(
        shared = T,
        pointFormatter = JS("function(){
                            return(this.label + '<br>')
  }")
      ) %>%
      hc_colors(colors = tmp.hc_colour) %>% 
      hc_title(text = vt.title_str,
			   style= list(fontSize="14px", fontFamily="Helvetica"))
    
  }
  
  return(hc)
  
})

#Grouped Bar comparison of domestic movers and non-movers 
output$mig_all_domestic_mover_comparison <- renderHighchart({
  
  vt.mig_all_col_in <- df.mig_all_flow_type_col_mapping %>%
    filter(FLOW_TYPE == '2') %>% 
    .[["COL_IN"]]
  vt.mig_all_col_out <- df.mig_all_flow_type_col_mapping %>%
    filter(FLOW_TYPE == '2') %>% 
    .[["COL_OUT"]]
  
  vt.param_perc <- input$mig_all_param_map_perc
  vt.param_start_year <- input$mig_all_param_year[1]
  vt.param_end_year <- input$mig_all_param_year[2]
  
  isolate({
    vt.param_ta <- input$mig_all_param_ta
    vt.param_agg_level <- input$mig_all_agg_level
  })
  
  validate(need(vt.mig_all_year_combn %>% 
                  filter(PREV_YEAR %in% vt.param_start_year, CURR_YEAR %in% vt.param_end_year) %>%
                  dim() %>%
                  .[1] >= 1,
                "Updating..."))
  
  if(vt.param_agg_level == "Total NZ"){
    
    tmp.plot_data <- mig_all_panel$base_data  %>%
      filter(
        FLOW_TYPE == 2 & FLOW_DIR == "in" |
          FLOW_TYPE == 1,
        FLOW_DIR != 'net',
        TA_CODE == 999,
        FLOW_DEMOGRAPHIC %in% c("Previous Age",
                                "Sex",
                                "Country",
                                "Ethnicity")
      ) %>% 
      mutate(
        MOVER = ifelse(FLOW_TYPE != 1, "Mover", "Non-Mover")
      ) %>% 
      #Aggregate to NZ/All other for countries
      mutate(
        FLOW_DEMOGRAPHIC_VALUE = ifelse(FLOW_DEMOGRAPHIC == "Country" & FLOW_DEMOGRAPHIC_VALUE != "NZ",
                                        "AA",
                                        FLOW_DEMOGRAPHIC_VALUE)
      ) %>% 
      #Aggregate age brackets
      left_join(df.mig_all_fdv_mapping_table,
                by = c("FLOW_DEMOGRAPHIC", "FLOW_DEMOGRAPHIC_VALUE")) %>% 
      mutate(FLOW_DEMOGRAPHIC_VALUE = ifelse(!is.na(FDV_AGG),
                                             FDV_AGG,
                                             FLOW_DEMOGRAPHIC_VALUE)) %>%
      #Sum in/out flows to get total movers and calculate distribution
      group_by(MOVER, FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE) %>% 
      summarise(VALUE = sum(VALUE)) %>% 
      group_by(MOVER, FLOW_DEMOGRAPHIC) %>% 
      mutate(VALUE_PERC = VALUE/sum(VALUE)) %>% 
      ungroup %>% 
      mutate(FLOW_DEMOGRAPHIC = ifelse(FLOW_DEMOGRAPHIC == "Previous Age",
                                       glue("Age at {vt.param_start_year}"),
                                       FLOW_DEMOGRAPHIC))
    
    #Generate categoriy grouping map
    list.categories_grouped <- map(unique(tmp.plot_data$FLOW_DEMOGRAPHIC),
                                   function(x){list(name = x,
                                                    categories = tmp.plot_data$FLOW_DEMOGRAPHIC_VALUE[tmp.plot_data$FLOW_DEMOGRAPHIC == x] %>% 
                                                      unique())})
    
    if(!vt.param_perc){
      
      hc <- highchart() %>%
        hc_add_series(data = tmp.plot_data,
                      type = "bar",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            NAME = FLOW_DEMOGRAPHIC,
                            group = MOVER)) %>% 
        hc_xAxis(
          categories = list.categories_grouped,
          labels = list(groupedOptions = list(list(rotation = 0,
                                                   distance = 0,
                                                   x = -2)),
                        rotation = 0,
                        distance = 0,
                        x = -5)
        ) %>% 
        hc_title(text = glue("Characteristics of Total NZ movers/non-movers between TAs from {vt.param_start_year} to {vt.param_end_year}"),
				 style= list(fontSize="14px", fontFamily="Helvetica")) %>%
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in))
      
    }
    
    if(vt.param_perc){
      
      tmp.plot_data = tmp.plot_data %>% 
        mutate(label = paste0("% of ", MOVER, "s split by ",
                              FLOW_DEMOGRAPHIC, ": ", 
                              formatC(100 * VALUE_PERC,
                                      format = 'f',
                                      digits = 2), "%"))
      
      hc <- highchart() %>% 
        hc_add_series(data = tmp.plot_data,
                      type = "bar",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE_PERC,
                            NAME = FLOW_DEMOGRAPHIC,
                            group = MOVER,
                            LABEL = label)) %>% 
        hc_xAxis(
          categories = list.categories_grouped,
          labels = list(groupedOptions = list(list(rotation = 0,
                                                   distance = 0,
                                                   x = -2)),
                        rotation = 0,
                        distance = 0,
                        x = -5)
        ) %>% 
        hc_title(text = glue("Characteristics of Total NZ movers/non-movers between TAs from {vt.param_start_year} to {vt.param_end_year} (% of movers/non-movers)"),
				 style= list(fontSize="14px", fontFamily="Helvetica")) %>% 
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.LABEL)
    }"))  %>% 
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in)) %>% 
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return(100 * this.value + '%')
    }")))
      
    }
    
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    tmp.plot_data <- mig_all_panel$base_data  %>%
      filter(
        FLOW_TYPE %in% 1:2,
        FLOW_DIR != 'net',
        TA == vt.param_ta,
        FLOW_DEMOGRAPHIC %in% c("Previous Age",
                                "Sex",
                                "Country",
                                "Ethnicity")
      ) %>% 
      mutate(
        MOVER = ifelse(FLOW_TYPE != 1, "Mover", "Non-Mover")
      ) %>% 
      #Aggregate to NZ/All other for countries
      mutate(
        FLOW_DEMOGRAPHIC_VALUE = ifelse(FLOW_DEMOGRAPHIC == "Country" & FLOW_DEMOGRAPHIC_VALUE != "NZ",
                                        "AA",
                                        FLOW_DEMOGRAPHIC_VALUE)
      ) %>% 
      #Aggregate age brackets
      left_join(df.mig_all_fdv_mapping_table,
                by = c("FLOW_DEMOGRAPHIC", "FLOW_DEMOGRAPHIC_VALUE")) %>% 
      mutate(FLOW_DEMOGRAPHIC_VALUE = ifelse(!is.na(FDV_AGG),
                                             FDV_AGG,
                                             FLOW_DEMOGRAPHIC_VALUE)) %>%
      #Sum in/out flows to get total movers and calculate distribution
      group_by(MOVER, FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE) %>% 
      summarise(VALUE = sum(VALUE)) %>% 
      group_by(MOVER, FLOW_DEMOGRAPHIC) %>% 
      mutate(VALUE_PERC = VALUE/sum(VALUE))%>% 
      ungroup %>% 
      mutate(FLOW_DEMOGRAPHIC = ifelse(FLOW_DEMOGRAPHIC == "Previous Age",
                                       glue("Age at {vt.param_start_year}"),
                                       FLOW_DEMOGRAPHIC))
    
    #Generate categoriy grouping map
    list.categories_grouped <- map(unique(tmp.plot_data$FLOW_DEMOGRAPHIC),
                                   function(x){list(name = x,
                                                    categories = tmp.plot_data$FLOW_DEMOGRAPHIC_VALUE[tmp.plot_data$FLOW_DEMOGRAPHIC == x] %>% 
                                                      unique())})
    
    if(!vt.param_perc){
      
      hc <- highchart() %>% 
        hc_add_series(data = tmp.plot_data,
                      type = "bar",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            group = MOVER)) %>% 
        hc_xAxis(
          categories = list.categories_grouped,
          labels = list(groupedOptions = list(list(rotation = 0,
                                                   distance = 0,
                                                   x = -2)),
                        rotation = 0,
                        distance = 0,
                        x = -5)
        ) %>% 
        hc_title(text = glue("Characteristics of {vt.param_ta} movers/non-movers between TAs from {vt.param_start_year} to {vt.param_end_year}"),
				 style= list(fontSize="14px", fontFamily="Helvetica")) %>%
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in))
      
    }
    
    if(vt.param_perc){
      
      tmp.plot_data = tmp.plot_data %>% 
        mutate(label = paste0("% of ", MOVER, "s split by ",
                              FLOW_DEMOGRAPHIC, ": ", 
                              formatC(100 * VALUE_PERC,
                                      format = 'f',
                                      digits = 2), "%"))
      
      hc <- highchart() %>% 
        hc_add_series(data = tmp.plot_data,
                      type = "bar",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE_PERC,
                            group = MOVER,
                            LABEL = label)) %>% 
        hc_xAxis(
          categories = list.categories_grouped,
          labels = list(groupedOptions = list(list(rotation = 0,
                                                   distance = 0,
                                                   x = -2)),
                        rotation = 0,
                        distance = 0,
                        x = -5)
        ) %>% 
        hc_title(text = glue("Characteristics of {vt.param_ta} movers/non-movers between TAs from {vt.param_start_year} to {vt.param_end_year} (% of movers/non-movers)"),
				 style= list(fontSize="14px", fontFamily="Helvetica")) %>% 
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.LABEL)
                                  }")) %>% 
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in)) %>% 
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return(100 * this.value + '%')
        }")))
      
    }
  }
  
  return(hc)
  
})

#Grouped International migration demographic breakdown
output$mig_all_international_migrant_demo <- renderHighchart({
  
  vt.param_perc <- input$mig_all_param_map_perc
  vt.param_start_year <- input$mig_all_param_year[1]
  vt.param_end_year <- input$mig_all_param_year[2]
  
  isolate({
    vt.param_ta <- input$mig_all_param_ta
    vt.param_agg_level <- input$mig_all_agg_level
  })
  
  validate(need(vt.mig_all_year_combn %>% 
                  filter(PREV_YEAR %in% vt.param_start_year, CURR_YEAR %in% vt.param_end_year) %>%
                  dim() %>%
                  .[1] >= 1,
                "Updating..."))
  
  vt.mig_all_col_in <- df.mig_all_flow_type_col_mapping %>%
    filter(FLOW_TYPE == '3') %>% 
    .[["COL_IN"]]
  vt.mig_all_col_out <- df.mig_all_flow_type_col_mapping %>%
    filter(FLOW_TYPE == '3') %>% 
    .[["COL_OUT"]]
  
  if(vt.param_agg_level == "Total NZ"){
    
    tmp.plot_data <- mig_all_panel$base_data  %>%
      filter(
        FLOW_DIR != 'net',
        FLOW_TYPE == 3,
        TA_CODE == 999,
        FLOW_DEMOGRAPHIC %in% c("Previous Age",
                                "Sex",
                                "Ethnicity",
                                "Visa")
      ) %>% 
      #Aggregate to NZ/All other for countries
      mutate(
        FLOW_DEMOGRAPHIC_VALUE = ifelse(FLOW_DEMOGRAPHIC == "Country" & FLOW_DEMOGRAPHIC_VALUE != "NZ",
                                        "AA",
                                        FLOW_DEMOGRAPHIC_VALUE)
      ) %>% 
      #Aggregate age brackets
      left_join(df.mig_all_fdv_mapping_table,
                by = c("FLOW_DEMOGRAPHIC", "FLOW_DEMOGRAPHIC_VALUE")) %>% 
      mutate(FLOW_DEMOGRAPHIC_VALUE = ifelse(!is.na(FDV_AGG),
                                             FDV_AGG,
                                             FLOW_DEMOGRAPHIC_VALUE)) %>%
      #Sum in/out flows to get total movers and calculate distribution
      group_by(FLOW_DIR, FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE, TOTAL_PREV, TOTAL_CURR) %>% 
      summarise(VALUE = sum(VALUE)) %>%
      mutate(VALUE = ifelse(FLOW_DIR == 'in',
                            VALUE,
                            -VALUE)) %>% 
      mutate(VALUE_PERC = VALUE/TOTAL_PREV) %>% 
      # group_by(FLOW_DIR, FLOW_DEMOGRAPHIC) %>% 
      # mutate(VALUE_PERC = VALUE/sum(abs(VALUE))) %>% 
      inner_join(df.mig_all_param_flow_map,
                 by = c("FLOW_DIR" = "VALUE"))%>% 
      ungroup %>% 
      mutate(FLOW_DEMOGRAPHIC = ifelse(FLOW_DEMOGRAPHIC == "Previous Age",
                                       glue("Age at {vt.param_start_year}"),
                                       FLOW_DEMOGRAPHIC),
             COL_VAR = ifelse(FLOW_DIR == "in",
                              vt.mig_all_col_in,
                              vt.mig_all_col_out)) %>% 
      mutate(LABEL = paste0(FLOW_DEMOGRAPHIC_VALUE, " ", NAME, ": ", comma(abs(VALUE)))) %>% 
      mutate(FLOW_ARRANGE = paste0(FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE, sep = "_")) %>% 
      mutate(FLOW_ARRANGE = factor(FLOW_ARRANGE, levels = unique(.$FLOW_ARRANGE)))
    
    #Generate categoriy grouping map
    list.categories_grouped <- map(unique(tmp.plot_data$FLOW_DEMOGRAPHIC),
                                   function(x){list(name = x,
                                                    categories = tmp.plot_data$FLOW_DEMOGRAPHIC_VALUE[tmp.plot_data$FLOW_DEMOGRAPHIC == x] %>% 
                                                      unique())})
    
    if(!vt.param_perc){
      
      hc <- highchart() %>% 
        hc_add_series(data = tmp.plot_data %>% 
                        filter(FLOW_DIR == "out"),
                      type = "bar",
                      name = df.mig_all_param_flow_map %>% 
                        filter(VALUE == "out") %>% 
                        .[["NAME"]],
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            color = COL_VAR,
                            label = LABEL)) %>%
        hc_add_series(data = tmp.plot_data %>% 
                        filter(FLOW_DIR == "in"),
                      type = "bar",
                      name = df.mig_all_param_flow_map %>% 
                        filter(VALUE == "in") %>% 
                        .[["NAME"]],
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            color = COL_VAR,
                            label = LABEL)) %>% 
        hc_add_series(data = tmp.plot_data %>% 
                        group_by(FLOW_ARRANGE, FLOW_DEMOGRAPHIC_VALUE, FLOW_DEMOGRAPHIC) %>% 
                        summarise(VALUE = sum(VALUE)) %>% 
                        mutate(LABEL = paste0(FLOW_DEMOGRAPHIC_VALUE, " Net Inflow: ", comma(VALUE)),
                               COL_VAR = "#000000"),
                      type = "scatter",
                      name = "Net Inflow",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            label = LABEL,
                            color = COL_VAR)) %>%
        hc_xAxis(
          categories = list.categories_grouped,
          labels = list(groupedOptions = list(list(rotation = 0,
                                                   distance = 0,
                                                   x = -2)),
                        rotation = 0,
                        distance = 0,
                        x = -5)
        ) %>% 
        hc_title(text = glue("International migration by migrant characteristics from {vt.param_start_year} to {vt.param_end_year}, Total NZ"),
			   style= list(fontSize="14px", fontFamily="Helvetica")) %>% 
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label)}")) %>%
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in, '#000000')) %>% 
        hc_plotOptions(bar = list(stacking = 'normal'))
      
    }
    
    if(vt.param_perc){
      
      tmp.plot_data <- tmp.plot_data %>% 
        mutate(LABEL = paste0(FLOW_DEMOGRAPHIC_VALUE, " ", NAME, ": ", paste0(formatC(100 * VALUE_PERC,
                                                                                      format = 'f',
                                                                                      digits = 1),
                                                                              "%")))
      
      hc <- highchart() %>% 
        hc_add_series(data = tmp.plot_data %>% 
                        filter(FLOW_DIR == "out"),
                      type = "bar",
                      name = df.mig_all_param_flow_map %>% 
                        filter(VALUE == "out") %>% 
                        .[["NAME"]],
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE_PERC,
                            color = COL_VAR,
                            label = LABEL)) %>%
        hc_add_series(data = tmp.plot_data %>% 
                        filter(FLOW_DIR == "in"),
                      type = "bar",
                      name = df.mig_all_param_flow_map %>% 
                        filter(VALUE == "in") %>% 
                        .[["NAME"]],
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE_PERC,
                            color = COL_VAR,
                            label = LABEL)) %>% 
        hc_add_series(data = tmp.plot_data %>% 
                        group_by(FLOW_ARRANGE, FLOW_DEMOGRAPHIC_VALUE, FLOW_DEMOGRAPHIC) %>% 
                        summarise(VALUE_PERC = sum(VALUE_PERC)) %>% 
                        mutate(LABEL = paste0(FLOW_DEMOGRAPHIC_VALUE,
                                              " Net Inflow: ",
                                              paste0(formatC(100 * VALUE_PERC,
                                                             format = 'f',
                                                             digits = 1),
                                                     "%")),
                               COL_VAR = "#000000"),
                      type = "scatter",
                      name = "Net Inflow",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE_PERC,
                            label = LABEL,
                            color = COL_VAR)) %>%
        hc_xAxis(
          categories = list.categories_grouped,
          labels = list(groupedOptions = list(list(rotation = 0,
                                                   distance = 0,
                                                   x = -2)),
                        rotation = 0,
                        distance = 0,
                        x = -5)
        ) %>% 
        hc_title(text = glue("International migration by migrant characteristics from {vt.param_start_year} to {vt.param_end_year}, Total NZ"),
			   style= list(fontSize="14px", fontFamily="Helvetica")) %>% 
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label)}")) %>% 
        hc_colors(colors = c(vt.mig_all_col_in, vt.mig_all_col_out))%>% 
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in, '#000000')) %>% 
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return(100 * this.value + '%')
        }"))) %>% 
        hc_plotOptions(bar = list(stacking = 'normal'))
      
    }
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    tmp.plot_data <- mig_all_panel$base_data  %>%
      filter(
        FLOW_DIR != 'net',
        FLOW_TYPE == 3,
        TA == vt.param_ta,
        FLOW_DEMOGRAPHIC %in% c("Previous Age",
                                "Sex",
                                "Ethnicity",
                                "Visa")
      ) %>% 
      #Aggregate to NZ/All other for countries
      mutate(
        FLOW_DEMOGRAPHIC_VALUE = ifelse(FLOW_DEMOGRAPHIC == "Country" & FLOW_DEMOGRAPHIC_VALUE != "NZ",
                                        "AA",
                                        FLOW_DEMOGRAPHIC_VALUE)
      ) %>% 
      #Aggregate age brackets
      left_join(df.mig_all_fdv_mapping_table,
                by = c("FLOW_DEMOGRAPHIC", "FLOW_DEMOGRAPHIC_VALUE")) %>% 
      mutate(FLOW_DEMOGRAPHIC_VALUE = ifelse(!is.na(FDV_AGG),
                                             FDV_AGG,
                                             FLOW_DEMOGRAPHIC_VALUE)) %>%
      group_by(FLOW_DIR, FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE, TOTAL_PREV, TOTAL_CURR) %>% 
      summarise(VALUE = sum(VALUE)) %>%
      mutate(VALUE = ifelse(FLOW_DIR == 'in',
                            VALUE,
                            -VALUE)) %>% 
      mutate(VALUE_PERC = VALUE/TOTAL_PREV) %>% 
      inner_join(df.mig_all_param_flow_map,
                 by = c("FLOW_DIR" = "VALUE"))%>% 
      ungroup %>% 
      mutate(FLOW_DEMOGRAPHIC = ifelse(FLOW_DEMOGRAPHIC == "Previous Age",
                                       glue("Age at {vt.param_start_year}"),
                                       FLOW_DEMOGRAPHIC),
             COL_VAR = ifelse(FLOW_DIR == "in",
                              vt.mig_all_col_in,
                              vt.mig_all_col_out)) %>% 
      mutate(LABEL = paste0(FLOW_DEMOGRAPHIC_VALUE, " ", NAME, ": ", comma(abs(VALUE)))) %>% 
      mutate(FLOW_ARRANGE = paste0(FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE, sep = "_")) %>% 
      mutate(FLOW_ARRANGE = factor(FLOW_ARRANGE, levels = unique(.$FLOW_ARRANGE)))
    
    #Generate category grouping map
    list.categories_grouped <- map(unique(tmp.plot_data$FLOW_DEMOGRAPHIC),
                                   function(x){list(name = x,
                                                    categories = tmp.plot_data$FLOW_DEMOGRAPHIC_VALUE[tmp.plot_data$FLOW_DEMOGRAPHIC == x] %>% 
                                                      unique())})
    
    if(vt.param_perc){
      
      tmp.plot_data <- tmp.plot_data %>% 
        mutate(LABEL = paste0(FLOW_DEMOGRAPHIC_VALUE, " ", NAME, ": ", paste0(formatC(100 * VALUE_PERC,
                                                                                      format = 'f',
                                                                                      digits = 2),
                                                                              "%")))
      
      hc <- highchart() %>%
        hc_add_series(data = tmp.plot_data %>% 
                        filter(FLOW_DIR == "out"),
                      type = "bar",
                      name = df.mig_all_param_flow_map %>% 
                        filter(VALUE == "out") %>% 
                        .[["NAME"]],
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE_PERC,
                            color = COL_VAR,
                            label = LABEL)) %>%
        hc_add_series(data = tmp.plot_data %>% 
                        filter(FLOW_DIR == "in"),
                      type = "bar",
                      name = df.mig_all_param_flow_map %>% 
                        filter(VALUE == "in") %>% 
                        .[["NAME"]],
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE_PERC,
                            color = COL_VAR,
                            label = LABEL)) %>% 
        hc_add_series(data = tmp.plot_data %>% 
                        group_by(FLOW_ARRANGE, FLOW_DEMOGRAPHIC_VALUE, FLOW_DEMOGRAPHIC) %>% 
                        summarise(VALUE_PERC = sum(VALUE_PERC)) %>% 
                        mutate(LABEL = paste0(FLOW_DEMOGRAPHIC_VALUE, " Net Inflow: ", paste0(formatC(100 * VALUE_PERC,
                                                                                                      format = 'f',
                                                                                                      digits = 1),
                                                                                              "%")),
                               COL_VAR = "#000000"),
                      type = "scatter",
                      name = 'Net Inflow',
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE_PERC,
                            label = LABEL,
                            color = COL_VAR)) %>%
        hc_xAxis(
          categories = list.categories_grouped,
          labels = list(groupedOptions = list(list(rotation = 0,
                                                   distance = 0,
                                                   x = -2)),
                        rotation = 0,
                        distance = 0,
                        x = -5)
        ) %>%  
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label)}")) %>%
        hc_title(text = glue("International migration by migrant characteristics from {vt.param_start_year} to {vt.param_end_year}, {vt.param_ta}"),
			   style= list(fontSize="14px", fontFamily="Helvetica")) %>%
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in, '#000000')) %>% 
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return(100 * this.value + '%')
    }"))) %>% 
        hc_plotOptions(bar = list(stacking = 'normal'))
      
    }
    
    if(!vt.param_perc){
      
      hc <- highchart() %>%
        hc_add_series(data = tmp.plot_data %>% 
                        filter(FLOW_DIR == "out"),
                      type = "bar",
                      name = df.mig_all_param_flow_map %>% 
                        filter(VALUE == "out") %>% 
                        .[["NAME"]],
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            color = COL_VAR,
                            label = LABEL)) %>%
        hc_add_series(data = tmp.plot_data %>% 
                        filter(FLOW_DIR == "in"),
                      type = "bar",
                      name = df.mig_all_param_flow_map %>% 
                        filter(VALUE == "in") %>% 
                        .[["NAME"]],
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            color = COL_VAR,
                            label = LABEL)) %>% 
        hc_add_series(data = tmp.plot_data %>% 
                        group_by(FLOW_ARRANGE, FLOW_DEMOGRAPHIC_VALUE, FLOW_DEMOGRAPHIC) %>% 
                        summarise(VALUE = sum(VALUE)) %>% 
                        mutate(LABEL = paste0(FLOW_DEMOGRAPHIC_VALUE, 
                                              " Net Inflow: ", comma(VALUE)),
                               COL_VAR = "#000000"),
                      type = "scatter",
                      name = "Net Inflow",
                      hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                            y = VALUE,
                            label = LABEL,
                            color = COL_VAR)) %>%
        hc_xAxis(
          categories = list.categories_grouped,
          labels = list(groupedOptions = list(list(rotation = 0,
                                                   distance = 0,
                                                   x = -2)),
                        rotation = 0,
                        distance = 0,
                        x = -5)
        ) %>% 
        hc_title(text = glue("International migration by migrant characteristics from {vt.param_start_year} to {vt.param_end_year}, {vt.param_ta}"),
			   style= list(fontSize="14px", fontFamily="Helvetica")) %>% 
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label)}")) %>%
        hc_colors(colors = c(vt.mig_all_col_out, vt.mig_all_col_in, '#000000')) %>% 
        hc_plotOptions(bar = list(stacking = 'normal'))
      
    }
  }
  
  return(hc)
  
})
