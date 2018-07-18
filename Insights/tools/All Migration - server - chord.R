output$mig_all_chord <- renderChorddiag({
  
  isolate({
    tmp.tootip_unit <- ""
    vt.param_agg_level <- input$mig_all_agg_level
  })
  
  mig_all_panel$base_data
  vt.param_perc <- input$mig_all_param_map_perc
  
  if(vt.param_agg_level == "Total NZ"){
    
    tmp.plot_mat <- mig_all_panel$base_data %>%
      filter(
        FLOW_DIR == "in",
        TA_LEVEL,
        FLOW_DEMOGRAPHIC == "TA",
        FLOW_TYPE == 2
      ) %>% 
      mutate(FLOW_DEMOGRAPHIC_VALUE = as.numeric(FLOW_DEMOGRAPHIC_VALUE)) %>% 
      inner_join(vt.mig_all_chord_grouping, by = c("TA_CODE", "TA")) %>% 
      inner_join(vt.mig_all_chord_grouping %>% 
                   rename(TA_CODE_GROUP_2 = TA_CODE_GROUP), by = c("FLOW_DEMOGRAPHIC_VALUE" = "TA_CODE")) %>% 
      group_by(TA_CODE_GROUP, TA_CODE_GROUP_2) %>% 
      summarise(VALUE = sum(VALUE)) %>% 
      mutate(VALUE = ifelse(TA_CODE_GROUP == TA_CODE_GROUP_2, 0, VALUE)) %>%
      ungroup() %>% 
      mutate(TA_CODE_GROUP = factor(TA_CODE_GROUP,
                                    levels = vt.mig_all_chord_levels),
             TA_CODE_GROUP_2 = factor(TA_CODE_GROUP_2,
                                      levels = vt.mig_all_chord_levels))
    
    validate(
      need(
        dim(tmp.plot_mat)[1] > 0,
        message = "Updating..."
      )
    )
    
    if(vt.param_perc){
      tmp.total_popn <- mig_all_panel$base_data %>%
        inner_join(vt.mig_all_chord_grouping, by = c("TA_CODE", "TA")) %>% 
        select(TA_CODE_GROUP, TOTAL_PREV) %>% 
        distinct %>% 
        group_by(TA_CODE_GROUP) %>% 
        summarise(TOTAL_PREV = sum(TOTAL_PREV)) %>% 
        ungroup %>% 
        mutate(TA_CODE_GROUP = factor(TA_CODE_GROUP,
                                      levels = vt.mig_all_chord_levels))
      
      tmp.plot_mat = tmp.plot_mat %>% 
        inner_join(tmp.total_popn,
                   by = c("TA_CODE_GROUP_2" = "TA_CODE_GROUP")) %>% 
        mutate(VALUE = round(100 * VALUE/TOTAL_PREV, 2)) %>% 
        select(-TOTAL_PREV)
      
      tmp.tootip_unit <- "%"
    }
    
    tmp.plot_mat <- tmp.plot_mat %>% 
      spread(TA_CODE_GROUP, VALUE) 
    
    #Get the code groupings to use as labels
    tmp.dim_labels <- tmp.plot_mat$TA_CODE_GROUP_2
    
    #Convert to numeric matrix
    tmp.plot_mat <- tmp.plot_mat %>% 
      select(-TA_CODE_GROUP_2) %>% 
      data.matrix()
    
    #Set labels
    dimnames(tmp.plot_mat) <- list(from = tmp.dim_labels,
                                   to = tmp.dim_labels)
    
    return(chorddiag(tmp.plot_mat,
                     showGroupnames = F,
                     showTicks = F,
                     tooltipUnit = tmp.tootip_unit,
                     margin = 10,
                     groupColors = vt.mig_all_chord_colouring,
                     groupnameFontsize = 12))
    
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    
    isolate({
      vt.param_ta <- input$mig_all_param_ta
      vt.param_start_year <- input$mig_all_param_year[1]
      vt.param_end_year <- input$mig_all_param_year[2]
    })
    
    #Filter data %>% 
    tmp.plot_data <- mig_all_panel$base_data  %>%
      filter(
        TA_LEVEL,
        FLOW_DEMOGRAPHIC == "TA",
        FLOW_TYPE == 2,
        FLOW_DIR != 'net'
      )
    
    #Take the top 5 to/from TLAs to display in diagram individually
    #Aggregate the remaining into other NI/SI
    vt.mig_all_chord_grouping <- tmp.plot_data %>% 
      group_by(FLOW_DEMOGRAPHIC_VALUE) %>% 
      summarise(VALUE = sum(VALUE)) %>% 
      arrange(desc(VALUE)) %>%
      mutate(INCLUDE = rank(-VALUE) <= 5) %>% 
      mutate(TA_CODE_GROUP_RAW = case_when(.$INCLUDE ~ .$FLOW_DEMOGRAPHIC_VALUE,
                                           as.numeric(.$FLOW_DEMOGRAPHIC_VALUE) <= 50 |
                                             .$FLOW_DEMOGRAPHIC_VALUE == 76 ~ "oni",
                                           T ~ "osi")) %>% 
      inner_join(df.mig_all_chord_mapping, 
                 by = c("TA_CODE_GROUP_RAW" = "code")) %>% 
      select(TA_CODE = FLOW_DEMOGRAPHIC_VALUE, TA_CODE_GROUP = ID, TA_CODE_GROUP_RAW) 
    
    #Plot them in north to south order
    
    #just reuse df.mig_ta_chord_ordering
    vt.mig_all_chord_order <- df.mig_ta_chord_ordering %>%
      inner_join(vt.mig_all_chord_grouping,
                 by = c("id" = "TA_CODE_GROUP")) %>% 
      bind_rows(df.mig_ta_chord_ordering %>% 
                  filter(id %in% vt.param_ta)) %>% 
      arrange(chord_order) %>% 
      .[['id']] %>% 
      unique()
    
    #Join the grouping and convert to the format (from, to, value) used for chordDiagram
    tmp.plot_data = tmp.plot_data %>% 
      inner_join(vt.mig_all_chord_grouping,
                 by = c("FLOW_DEMOGRAPHIC_VALUE" = "TA_CODE")) %>% 
      mutate(SOURCE = ifelse(FLOW_DIR != "in",
                             TA,
                             TA_CODE_GROUP),
             TARGET = ifelse(FLOW_DIR == "in",
                             TA,
                             TA_CODE_GROUP)) %>%
      mutate(SOURCE = factor(SOURCE, 
                             levels = vt.mig_all_chord_order),
             TARGET = factor(TARGET,
                             levels = vt.mig_all_chord_order)) %>% 
      group_by(SOURCE, TARGET) %>% 
      summarise(VALUE = sum(VALUE))
    
    if(vt.param_perc){
      
      #Get total previous populations for all other (not selected) TA
      #summarise to total populations for aggregated areas
      tmp.total_popn = df.mig_all_chord_ta_popn %>% 
        filter(PREV_YEAR %in% vt.param_start_year,
               CURR_YEAR %in% vt.param_end_year) %>% 
        inner_join(vt.mig_all_chord_grouping %>% 
                     mutate(TA_CODE = as.numeric(TA_CODE)),
                   by = "TA_CODE") %>% 
        group_by(TA_CODE_GROUP) %>% 
        summarise(TOTAL_PREV = sum(TOTAL_PREV)) %>% 
        #Add population for selected TA
        bind_rows(df.mig_all_chord_ta_popn %>% 
                    filter(PREV_YEAR %in% vt.param_start_year,
                           CURR_YEAR %in% vt.param_end_year,
                           TA %in% vt.param_ta) %>% 
                    select(TA_CODE_GROUP = TA, TOTAL_PREV)) %>% 
        ungroup %>% 
        mutate(TA_CODE_GROUP = factor(TA_CODE_GROUP,
                                      levels = vt.mig_all_chord_order))
      
      tmp.plot_data = tmp.plot_data %>% 
        inner_join(tmp.total_popn,
                   by = c("SOURCE" = "TA_CODE_GROUP")) %>% 
        mutate(VALUE = round(100 * VALUE/TOTAL_PREV, 2)) %>% 
        select(-TOTAL_PREV)
      
      tmp.tootip_unit <- "%"
    }
    
    tmp.plot_data = tmp.plot_data %>%             
      spread(TARGET, VALUE) 
    
    #create chorddiag matrix and plot
    tmp.plot_names <- tmp.plot_data$SOURCE
    
    tmp.plot_data = tmp.plot_data %>% 
      ungroup %>% 
      select(-SOURCE) %>% 
      data.matrix()
    
    dimnames(tmp.plot_data) <- list(from = tmp.plot_names,
                                    to = tmp.plot_names)
    
    tmp.plot_data[is.na(tmp.plot_data)] <- 0
    
    return(chorddiag(tmp.plot_data,
                     showGroupnames = F,
                     showTicks = F,
                     tooltipUnit = tmp.tootip_unit,
                     margin = 10,
                     groupColors = vt.mig_all_chord_colouring,
                     groupnameFontsize = 12))
    
  }
  
})