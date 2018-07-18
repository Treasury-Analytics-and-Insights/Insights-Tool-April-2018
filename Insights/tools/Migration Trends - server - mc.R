mig_trend_panel <- reactiveValues(
  base_data = df.init_mig_trend_base_data,
  demographic_plot_data = NULL,
  source_target_plot_data = NULL,
  population_plot_data = NULL
)

#when the flow type changes
#Update second select input for source/target
#Update the migration demographic selection for the location
observe({
  
  vt.param_flow_type <- input$mig_trend_param_st_flow_type
  vt.param_flow_type <- df.mig_trend_flow_type_mapping %>% 
    filter(FLOW_TYPE_LABEL %in% vt.param_flow_type) %>% 
    .[["FLOW_TYPE"]]
  
  if(vt.param_flow_type == 2){
    vt.mig_trends_select_update <- vt.init_mig_trend_param_source_select
    vt.mig_trends_select_update_selected <- vt.init_mig_trend_param_target
    vt.mig_trends_target_label <- vt.mig_trend_target_loc_name
  }
  
  if(vt.param_flow_type == 3){
    vt.mig_trends_select_update <- vt.init_mig_trends_param_country_select
    vt.mig_trends_select_update_selected <- vt.init_mig_trends_param_country
    vt.mig_trends_target_label <- vt.mig_trend_target_international_loc_name
  }
  
  vt.param_flow_type <- input$mig_trend_param_demo_flow_type
  vt.param_flow_type <- df.mig_trend_flow_type_mapping %>% 
    filter(FLOW_TYPE_LABEL %in% vt.param_flow_type) %>% 
    .[["FLOW_TYPE"]]
  
  vt.mig_trends_demographic_update <- mig_trend_panel$base_data %>% 
    filter(FLOW_TYPE %in% vt.param_flow_type) %>%
    mutate(FLOW_DEMOGRAPHIC_LABEL = ifelse(is.na(FLOW_DEMOGRAPHIC),
                                           "Total",
                                           FLOW_DEMOGRAPHIC)) %>% 
    .[["FLOW_DEMOGRAPHIC_LABEL"]] %>% 
    unique
  
  #If we have tot flow_type the total level needs to be readded 
  if(!("Total" %in% vt.mig_trends_demographic_update)){
    vt.mig_trends_demographic_update <- c("Total", vt.mig_trends_demographic_update)
  }
  
  #Find the previous demo level
  #The update will select all available levels that were in the previous selection if possible
  isolate({
    vt.param_demo_select <- input$mig_trend_param_demo
  })
  
  #Update the demographic input
  #if none of the previous levels are available, take the first level of the update 
  if(length(vt.mig_trends_demographic_update[which(vt.mig_trends_demographic_update %in% vt.param_demo_select)]) > 0){
    updateSelectizeInput(session,
                         inputId = "mig_trend_param_demo",
                         label = "Characteristic of interest",
                         choices = vt.mig_trends_demographic_update,
                         selected = vt.mig_trends_demographic_update[which(vt.mig_trends_demographic_update %in% vt.param_demo_select)],
                         options = NULL)
  } else{
    updateSelectizeInput(session,
                         inputId = "mig_trend_param_demo",
                         label = "Characteristic of interest",
                         choices = vt.mig_trends_demographic_update,
                         selected = vt.mig_trends_demographic_update[1],
                         options = NULL)
  }
  
  #Update the target input
  updateSelectizeInput(session,
                       inputId = "mig_trend_param_target",
                       label = vt.mig_trends_target_label,
                       choices = vt.mig_trends_select_update,
                       selected = vt.mig_trends_select_update_selected,
                       options = NULL)
  
})

#Data for demographic plot
observe({
  
  vt.param_flow_dir <- input$mig_trend_param_flow
  vt.param_flow_dir <- df.mig_trend_param_flow_map %>% 
    filter(NAME %in% vt.param_flow_dir) %>% 
    .[["VALUE"]]
  
  vt.param_flow_type_label <- input$mig_trend_param_demo_flow_type
  vt.param_sources <- input$mig_trend_param_location
  vt.param_perc <- input$mig_trend_param_perc
  
  vt.param_flow_type <- df.mig_trend_flow_type_mapping %>% 
    filter(FLOW_TYPE_LABEL %in% vt.param_flow_type_label) %>% 
    .[["FLOW_TYPE"]]
  
  vt.param_flow_demo <- input$mig_trend_param_demo
  
  #Relabel demographic variable if necessary
  if("Total" %in% vt.param_flow_demo){
    vt.param_flow_demo[which(vt.param_flow_demo == "Total")] <- NA
  }
  
  #Create plot data
  
  tmp.plot_data <- mig_trend_panel$base_data %>%
    filter(
      FLOW_DIR == vt.param_flow_dir,
      (FLOW_TYPE %in% vt.param_flow_type) &
        (FLOW_TYPE != "tot"),
      TA %in% vt.param_sources,
      FLOW_DEMOGRAPHIC %in% vt.param_flow_demo
    )
  
  #Create plot
  tmp.plot_data <- tmp.plot_data %>% 
    inner_join(df.metric_mapping_table, by = "METRIC") %>% 
    mutate(PLOT_LABEL = glue("{.$TA} - {.$METRIC_NAME}"))
  
  #Filter to top 10 flows for each Location of interest, demographic combination (Only affects flows TA)
  tmp.data_filter <- tmp.plot_data %>% 
    group_by(TA, FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE) %>% 
    summarise(VALUE = sum(VALUE)) %>% 
    group_by(TA, FLOW_DEMOGRAPHIC) %>% 
    mutate(rank = rank(-VALUE)) %>% 
    filter(rank <= 10)
  
  tmp.plot_data = tmp.plot_data %>% 
    inner_join(tmp.data_filter %>% 
                 select(TA, FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE),
               by = c("TA", "FLOW_DEMOGRAPHIC", "FLOW_DEMOGRAPHIC_VALUE"))
  
  tmp.plot_data = tmp.plot_data %>% 
    inner_join(df.mig_trend_flow_type_mapping,
               by = "FLOW_TYPE") %>% 
    inner_join(df.mig_trend_param_flow_map %>% 
                 rename(FLOW_DIR = VALUE,
                        FLOW_DIR_LABEL = NAME),
               by = c("FLOW_DIR")) %>% 
    select(-FLOW_DIR, -FLOW_TYPE) %>% 
    rename(FLOW_DIR = FLOW_DIR_LABEL,
           FLOW_TYPE = FLOW_TYPE_LABEL) %>% 
    mutate(FDV_LABEL = str_extract(METRIC_NAME, "(?<=- )(.)*")) %>% 
    mutate(FDV_LABEL = gsub("Births |Deaths |WithinNZ |International |Natural increase |Births|Deaths|WithinNZ|International|Natural increase",
                            "",
                            FDV_LABEL))
  
  #The Total Category requires aggregation
  if('tot' %in% vt.param_flow_type){
    
    tmp.plot_data_tot <- mig_trend_panel$base_data %>%
      filter(TA %in% vt.param_sources,
             FLOW_TYPE %in% 2:4,
             FLOW_DIR == vt.param_flow_dir,
             FLOW_DEMOGRAPHIC %in% vt.param_flow_demo) %>%
      group_by(TA, TA_CODE, TOTAL_CURR, TOTAL_PREV, PREV_YEAR, CURR_YEAR, FLOW_DIR,
               METRIC,
               FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE) %>%
      summarise(VALUE = sum(VALUE)) %>% 
      ungroup %>% 
      inner_join(df.mig_trend_param_flow_map %>% 
                   rename(FLOW_DIR_NAME =  NAME),
                 by = c("FLOW_DIR" = "VALUE")) %>%
      inner_join(df.mig_trends_total_metric_names,
                 by = c("FLOW_DEMOGRAPHIC", "FLOW_DEMOGRAPHIC_VALUE")) %>% 
      group_by(TA, CURR_YEAR, TOTAL_CURR, TOTAL_PREV, FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE,
               FDV_LABEL, TOTAL_METRIC_NAME, FLOW_DIR_NAME) %>% 
      summarise(VALUE = sum(VALUE)) %>% 
      ungroup %>% 
      mutate(PLOT_LABEL = glue("{.$TA} - {.$FLOW_DIR_NAME} - {ifelse(is.na(.$FLOW_DEMOGRAPHIC), '', .$FLOW_DEMOGRAPHIC)} {.$TOTAL_METRIC_NAME}"),
             FLOW_TYPE = vt.mig_trend_tot_ft_label)
    
    #Filter to top 10 flows for each Location of interest, demographic combination (Only affects flows TA)
    tmp.data_filter <- tmp.plot_data_tot %>% 
      group_by(TA, FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE) %>% 
      summarise(VALUE = sum(VALUE)) %>% 
      group_by(TA, FLOW_DEMOGRAPHIC) %>% 
      mutate(rank = rank(-VALUE)) %>% 
      filter(rank <= 10)
    
    tmp.plot_data_tot = tmp.plot_data_tot %>% 
      inner_join(tmp.data_filter %>% 
                   select(TA, FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE),
                 by = c("TA", "FLOW_DEMOGRAPHIC", "FLOW_DEMOGRAPHIC_VALUE"))
    
    #If we also have non-aggregated data, we need to bind them
    if(dim(tmp.plot_data)[1] >= 1){
      
      tmp.plot_data <- tmp.plot_data %>% 
        bind_rows(tmp.plot_data_tot) %>% 
        mutate(METRIC_NAME = coalesce(METRIC_NAME, TOTAL_METRIC_NAME),
               FLOW_DIR = coalesce(FLOW_DIR, FLOW_DIR_NAME))
    } else{
      tmp.plot_data <- tmp.plot_data_tot %>% 
        rename(METRIC_NAME = TOTAL_METRIC_NAME,
               FLOW_DIR = FLOW_DIR_NAME)
    }
    
  }
  
  if(vt.param_perc){
    tmp.plot_data = tmp.plot_data %>% 
      mutate(VALUE = VALUE/TOTAL_CURR)
  }
  
  mig_trend_panel$demographic_plot_data <- tmp.plot_data
  
})

#Data for source target graph
observe({
  
  vt.param_perc <- input$mig_trend_param_perc
  vt.param_flow_dir <- input$mig_trend_param_flow
  vt.param_flow_dir <- df.mig_trend_param_flow_map %>% 
    filter(NAME %in% vt.param_flow_dir) %>% 
    .[["VALUE"]]
  vt.param_flow_type_label <- input$mig_trend_param_st_flow_type
  vt.param_source <- input$mig_trend_param_source
  vt.param_target <- input$mig_trend_param_target
  
  vt.param_flow_type <- df.mig_trend_flow_type_mapping %>% 
    filter(FLOW_TYPE_LABEL == vt.param_flow_type_label) %>% 
    .[["FLOW_TYPE"]]
  
  #Switch from TA labels to flow_demographic_value if flow_type 2/domestic
  if(vt.param_flow_type == 2){
    
    tmp.param_target <- df.mig_ta_code_mapping %>% 
      filter(TA %in% vt.param_target) %>% 
      .[["FLOW_DEMOGRAPHIC_VALUE"]]
    
    #Total NZ is a special case
    if("Total NZ" %in% vt.param_target){
      tmp.param_target <- c(tmp.param_target, "Total NZ")
    }
    
    vt.param_target <- tmp.param_target
  }
  
  #Switch from country labels to country codes for flow_demographic_value if flow_type 3/international
  if(vt.param_flow_type == 3){
    tmp.param_target <-  df.region_country_mapping_table %>% 
      filter(COUNTRY %in% vt.param_target) %>% 
      .[["CODE"]]
    
    if("Total International" %in% vt.param_target){
      tmp.param_target <- c(tmp.param_target, "Total International")
    }
    
    vt.param_target <- tmp.param_target
  }
  
  #Only have target for int/ext not natural
  if(vt.param_flow_type %in% 2:3){
    tmp.plot_data <- mig_trend_panel$base_data %>%
      filter(
        FLOW_DIR == vt.param_flow_dir,
        FLOW_TYPE == vt.param_flow_type,
        TA %in% vt.param_source,
        FLOW_DEMOGRAPHIC_VALUE %in% vt.param_target
      )
    
    #Need to add Total NZ outflow represented by (flow dir)_2
    if("Total NZ" %in% vt.param_target &
       vt.param_flow_type %in% 2){
      tmp.plot_data <- bind_rows(tmp.plot_data,
                                 mig_trend_panel$base_data %>%
                                   filter(
                                     FLOW_DIR == vt.param_flow_dir,
                                     FLOW_TYPE == vt.param_flow_type,
                                     TA %in% vt.param_source,
                                     is.na(FLOW_DEMOGRAPHIC)
                                   ))
    }
    
    #Need to add Total NZ outflow represented by (flow dir)_2
    if("Total International" %in% vt.param_target &
       vt.param_flow_type %in% 3){
      tmp.plot_data <- bind_rows(tmp.plot_data,
                                 mig_trend_panel$base_data %>%
                                   filter(
                                     FLOW_DIR == vt.param_flow_dir,
                                     FLOW_TYPE == vt.param_flow_type,
                                     TA %in% vt.param_source,
                                     is.na(FLOW_DEMOGRAPHIC)
                                   ))
    }
  }
  
  #Change to percentage of source popn
  if(vt.param_perc){
    tmp.plot_data = tmp.plot_data %>% 
      mutate(VALUE = VALUE/TOTAL_CURR)
  }
  
  tmp.plot_data <- tmp.plot_data %>% 
    inner_join(df.metric_mapping_table, by = "METRIC") %>% 
    #Total NZ flows from (flow dir)_2 are renamed
    mutate(METRIC_NAME = gsub("WithinNZ$", "Total NZ", METRIC_NAME)) %>% 
    #Total int flows from (flow dir)_3 are renamed
    mutate(METRIC_NAME = gsub("International$", "Total International", METRIC_NAME)) %>%
    mutate(PLOT_LABEL = glue("{.$TA} - {.$METRIC_NAME}"))
  
  if(df.mig_trend_flow_type_mapping %>% 
     filter(FLOW_TYPE_LABEL %in% vt.param_flow_type_label) %>% 
     .[["FLOW_TYPE"]] == 2){
    
    tmp.plot_data = tmp.plot_data %>% 
      left_join(spldf.nz_ta_migration@data,
                by = c("FLOW_DEMOGRAPHIC_VALUE" = "code")) %>% 
      rename(LOCATION_2 = ID)
    
  } else{
    
    tmp.plot_data = tmp.plot_data %>% 
      left_join(df.region_country_mapping_table,
                by = c("FLOW_DEMOGRAPHIC_VALUE" = "CODE")) %>% 
      rename(LOCATION_2 = COUNTRY)
    
  }
  
  mig_trend_panel$source_target_plot_data <- tmp.plot_data
  
})

#data for population graph
observe({
  
  vt.param_sources <- input$mig_trend_param_popn_location
  vt.param_flow_demo <- input$mig_trend_param_popn_demo
  vt.param_popn = input$mig_trend_param_popn_perc
  
  if("Age" %in% vt.param_flow_demo){
    vt.param_flow_demo <- c(vt.param_flow_demo,
                            "Current Age",
                            "Previous Age")
  }
  
  #Create plot data
  tmp.plot_data <- mig_trend_panel$base_data %>%
    filter(
      TA %in% vt.param_sources,
      FLOW_DEMOGRAPHIC %in% vt.param_flow_demo,
      FLOW_TYPE == 'tot',
      FLOW_DIR == "in" | (FLOW_DIR == "out" &
                             PREV_YEAR == min(PREV_YEAR))
    ) 
  
  if("Age" %in% vt.param_flow_demo & 
     dim(tmp.plot_data)[1] > 1){
    tmp.plot_data <- tmp.plot_data %>% 
      #Filter to one Age
      mutate(MIN_YEAR = PREV_YEAR == min(PREV_YEAR)) %>%
      mutate(AGE_FILTER = case_when(grepl("Current Age", .$FLOW_DEMOGRAPHIC) &
                                      .$FLOW_DIR == "in" ~ T,
                                    grepl("Previous Age", .$FLOW_DEMOGRAPHIC) &
                                      .$FLOW_DIR == "out" &
                                      .$MIN_YEAR ~ T,
                                    !grepl("Age", .$FLOW_DEMOGRAPHIC) ~ T,
                                    T ~ F)) %>% 
      filter(AGE_FILTER)
  }
  
  tmp.total_plot_data = mig_trend_panel$base_data %>%
    filter(
      TA %in% vt.param_sources,
      FLOW_DIR == "in" | (FLOW_DIR == "out" &
                            PREV_YEAR == min(PREV_YEAR))
    ) 
  
    #Create total plot data
  if("Total" %in% vt.param_flow_demo &
     dim(tmp.total_plot_data)[1] >= 1){
    tmp.total_plot_data = tmp.total_plot_data %>%
      select(TA, PREV_YEAR, CURR_YEAR, TOTAL_CURR, TOTAL_PREV, FLOW_DIR) %>% 
      distinct %>% 
      mutate(VALUE = case_when(.$PREV_YEAR == min(.$PREV_YEAR) & .$FLOW_DIR == "out" ~ .$TOTAL_PREV,
                               T ~ .$TOTAL_CURR))
    
    tmp.plot_data = tmp.plot_data %>% 
          bind_rows(tmp.total_plot_data)
    
  }
  
  tmp.plot_data <- tmp.plot_data %>% 
    mutate(MIN_YEAR = PREV_YEAR == min(PREV_YEAR)) %>% 
    mutate(YEAR = case_when(.$FLOW_DIR == "out" & 
                              .$MIN_YEAR ~ .$PREV_YEAR,
                            .$FLOW_DIR == "in" ~ .$CURR_YEAR)) %>% 
    left_join(df.metric_mapping_table %>% 
                mutate(METRIC_NAME = gsub(" at end year| at start year",
                            "",
                            METRIC_NAME)),
              by = "METRIC") %>% 
    mutate(METRIC_NAME = ifelse(!is.na(METRIC), 
                                gsub("Current |Previous ", "", METRIC_NAME), 
                                paste0("Total population"))) %>% 
    mutate(PLOT_LABEL = glue("{.$TA} - {.$METRIC_NAME}"))
  
  if(vt.param_popn){
    tmp.plot_data = tmp.plot_data %>% 
      left_join(df.mig_trends_year_popn,
                by = "YEAR") %>% 
      mutate(VALUE = VALUE/POPULATION)
  }
  
  mig_trend_panel$population_plot_data = tmp.plot_data
  
})

#Download button, get the current tab's data and download
output$mig_trend_data_download <- downloadHandler(
  filename = function(){
    
    vt.file_name = input$salmon_tabBox
    
    return(paste0(gsub(" ", "_", vt.file_name), "_data_", format(Sys.time(), "%x_%H:%M"), ".csv"))
    },
  content = function(file) {
    
    vt.param_perc = input$mig_trend_param_perc
    vt.param_popn_perc = input$mig_trend_param_popn_perc
    vt.param_flow_dir = input$mig_trend_param_flow
    
    vt.param_flow_dir_val = df.mig_trend_param_flow_map %>% 
      filter(NAME %in% vt.param_flow_dir) %>% 
      .[["VALUE"]]
    
    if(input$salmon_tabBox == vt.mig_trend_panel_1){
      
      df = mig_trend_panel$population_plot_data
      
      if(!vt.param_popn_perc){
        df = popn_characteristic_csv_reformat(df, df.mig_trend_population_rename_absolute)
      } else{
        df = df %>% 
          mutate(VALUE = paste0(formatC(100 * VALUE,
            format = 'f',
            digits = 2),
            "%"))
        
        df = popn_characteristic_csv_reformat(df, df.mig_trend_population_rename_percentage)
      }
    }
    
    if(input$salmon_tabBox == vt.mig_trend_panel_2){
      df = mig_trend_panel$demographic_plot_data 
      
      if(!vt.param_perc){
        df = popn_characteristic_csv_reformat(df, df.mig_trend_loi_rename_absolute)
      } else{
        df = df %>% 
          mutate(VALUE = paste0(formatC(100 * VALUE,
                                        format = 'f',
                                        digits = 2),
                                "%"))
        
        df = popn_characteristic_csv_reformat(df, df.mig_trend_loi_rename_percentage)
      }
    }

    if(input$salmon_tabBox == vt.mig_trend_panel_3){
      
      df = mig_trend_panel$source_target_plot_data
      
      if(vt.param_flow_dir_val %in% vt.mig_trend_param_flow_dir_in){
        if(!vt.param_perc){
          df = location_csv_reformat(df, df.mig_trend_locations_in_absolute)
        } else{
          df = df %>% 
            mutate(VALUE = paste0(formatC(100 * VALUE,
                                          format = 'f',
                                          digits = 2),
                                  "%"))
          
          df = location_csv_reformat(df, df.mig_trend_locations_in_percentage) 
        }
      } else{
        if(!vt.param_perc){
          df = location_csv_reformat(df, df.mig_trend_locations_out_absolute)
        } else{
          df = df %>% 
            mutate(VALUE = paste0(formatC(100 * VALUE,
                                          format = 'f',
                                          digits = 2),
                                  "%"))
          
          df = location_csv_reformat(df, df.mig_trend_locations_out_percentage) 
        }
      }
    }
    
    write.csv(df, file, row.names = F) 
  })

#Reverse the selected fields
#Note action button is only available for domestic migration
observeEvent(input$mig_trend_action_reversal,
             {
               
               vt.location_1_selected <- input$mig_trend_param_source
               vt.location_2_selected <- input$mig_trend_param_target
               
               updateSelectizeInput(session,
                                    inputId = "mig_trend_param_source",
                                    label = vt.mig_trend_source_loc_name,
                                    choices = vt.init_mig_trend_param_source_select,
                                    selected = vt.location_2_selected)
               
               updateSelectizeInput(session,
                                    inputId = "mig_trend_param_target",
                                    label = vt.mig_trend_target_loc_name,
                                    choices = vt.init_mig_trend_param_source_select,
                                    selected = vt.location_1_selected)
               
             })

#Help dialogue for the page
observeEvent({input$mig_trends_page_modal_help},
             {
               
               vt.help_dialogue1 <- glue("Population directions presents new data about population change in New Zealand. It uses interactive graphs to show changes over time in, and movements between, selected locations. You can use this to investigate population change in different territorial authorities, as well as for all of New Zealand, between {vt.mig_trend_param_year_1_min} and {vt.mig_trend_param_year_2_max}.")
               
               vt.help_dialogue2 <-  glue("The different tabs allow different graphs to be explored. The 'Population trends' tab looks at the population of areas over time. The 'Population change by type of change' tab allows the contribution of different population flows to be explored. The 'Population flows between TAs and for specific countries' tab lets the user look at movements between specific territorial authorities, or to and from specific countries over time to be examined.")
               
               vt.help_dialogue3 <-  glue("There are a number of controls on the left-hand side of the page you can use to manipulate the data displayed. The direction of population change and type of population change button apply filters to the data. The chart type button changes the visual representation of the data, but not the information displayed. The 'Display as a percentage of the population?' checkbox allows you to view figures as a percentage of the population of an area. Finally, the download button will download the data behind your current selection.")
               
               vt.help_dialogue4 <-  glue("Categories can be removed from the graph by clicking on the legend at the bottom. Hovering over the points in a time series with your mouse pointer will reveal a tooltip containing additional information.")
               
               showModal(modalDialog(
                 title = "About this tool",
                 vt.help_dialogue1,
                 br(),
                 br(),
                 vt.help_dialogue2,
                 br(),
                 br(),
                 vt.help_dialogue3,
                 br(),
                 br(),
                 vt.help_dialogue4,
                 easyClose = TRUE,
                 footer = NULL
               ))
               
             })