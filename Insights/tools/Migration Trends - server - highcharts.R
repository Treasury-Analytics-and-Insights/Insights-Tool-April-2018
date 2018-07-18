output$mig_trend_population <- renderHighchart({
  
  vt.param_chart_type <- input$mig_trend_param_chart_type
  tmp.plot_data <- mig_trend_panel$population_plot_data
  
  validate(
    need(dim(tmp.plot_data)[1] > 0,
         "No data selected.")
   )
  
  isolate({
    vt.param_location <- input$mig_trend_param_popn_location
    vt.param_popn_perc <- input$mig_trend_param_popn_perc
  })
  
  vt.chart_title <- glue("Population of {ifelse(length(vt.param_location) > 1, paste(paste(vt.param_location[1:(length(vt.param_location) - 1)], collapse = ', '), vt.param_location[length(vt.param_location)], sep = ' and '), vt.param_location)} for selected characteristics")
  
  if(vt.param_chart_type == "Bar"){
    
    hc <- highchart() %>% 
      hc_add_series(
        data = tmp.plot_data,
        type = "column",
        hcaes(x = YEAR,
              y = VALUE,
              group = PLOT_LABEL,
              label = PLOT_LABEL)
      ) %>% 
      hc_plotOptions(inverted = T) %>% 
      hc_colors(vt.mig_trends_hc_colouring) %>% 
      hc_title(text = vt.chart_title,
			   style= list(fontSize="16px", fontFamily="Helvetica")) %>% 
      hc_xAxis(min = min(tmp.plot_data$PREV_YEAR) - 1,
               max = max(tmp.plot_data$CURR_YEAR) + 1,
               showFirstLabel = F,
               showLastLabel = F)
    
    if(!as.logical(vt.param_popn_perc)){
      hc = hc %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label + ': ' + '<b>' + this.y + '</b>')
    }"))
    }
    
    if(as.logical(vt.param_popn_perc)){
      hc = hc %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label + ': ' + '<b>' + (100 * this.y).toFixed(2)  + '%' + '</b>')
    }")) %>% 
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return((100 * this.value).toFixed(2) + '%')
        }")))
    }
    
  }
  
  if(vt.param_chart_type == "Line"){
    
    hc <- highchart() %>% 
      hc_add_series(
        data = tmp.plot_data,
        type = "line",
        hcaes(x = YEAR,
              y = VALUE,
              group = PLOT_LABEL,
              label = PLOT_LABEL)
      ) %>% 
      hc_colors(vt.mig_trends_hc_colouring) %>% 
      hc_title(text = vt.chart_title,
			   style= list(fontSize="16px", fontFamily="Helvetica"))
    
    if(!as.logical(vt.param_popn_perc)){
      hc = hc %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label + ': ' + '<b>' + this.y + '</b>')
    }"))
    }
    
    if(as.logical(vt.param_popn_perc)){
      hc = hc %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label + ': ' + '<b>' + (100 * this.y).toFixed(2)  + '%' + '</b>')
    }")) %>% 
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return((100 * this.value).toFixed(2) + '%')
    }")))
    }
    
  }
  
  hc = hc %>% 
    hc_legend(layout = 'horizontal',
              maxHeight = "100")
  
  return(hc)

  })

output$mig_trend_location <- renderHighchart({
  
  vt.param_chart_type <- input$mig_trend_param_chart_type
  tmp.plot_data <- mig_trend_panel$demographic_plot_data
  
  validate(
    need(dim(tmp.plot_data)[1] > 0,
         "No data selected.")
  )
  
  #For title
  isolate({
    vt.param_flow <- input$mig_trend_param_flow
    vt.param_flow_type <- input$mig_trend_param_demo_flow_type
    vt.param_popn_perc <- input$mig_trend_param_perc
    vt.param_location <- input$mig_trend_param_location
  })
  
  vt.param_flow_value <- df.mig_trend_param_flow_map %>% 
    filter(NAME %in% vt.param_flow) %>% 
    .[["VALUE"]]
  
  #Generate chart title
  
  #If Natural change is the only selection then use births/deaths in title
  if((df.mig_trend_flow_type_mapping %>%
      filter(FLOW_TYPE == 4) %>%
      .[["FLOW_TYPE_LABEL"]] %in% vt.param_flow_type) &
     (length(vt.param_flow_type) == 1)){
    
    vt.param_flow <- df.mig_natural_change_flow_dir_mapping %>% 
      filter(FLOW_DIR == vt.param_flow) %>% 
      .[["FLOW_DIR_LABEL"]]
    
    vt.chart_title <- glue("{vt.param_flow} in {ifelse(length(vt.param_location) > 1, paste(paste(vt.param_location[1:(length(vt.param_location) - 1)], collapse = ', '), vt.param_location[length(vt.param_location)], sep = ' and '), vt.param_location)} for selected characteristics")
    
  } else{
    
    vt.chart_title <- glue("{vt.param_flow} {ifelse(vt.param_flow_value %in% c('in', 'net'), 'to', 'from')} {ifelse(length(vt.param_location) > 1, paste(paste(vt.param_location[1:(length(vt.param_location) - 1)], collapse = ', '), vt.param_location[length(vt.param_location)], sep = ' and '), vt.param_location)} for selected characteristics - {paste0(vt.param_flow_type, collapse = ', ')}")
  }
  
  if(vt.param_chart_type == "Bar"){
    
    hc <- highchart() %>% 
      hc_add_series(
        data = tmp.plot_data,
        type = "column",
        hcaes(x = CURR_YEAR,
              y = VALUE,
              group = PLOT_LABEL,
              label = PLOT_LABEL)
      ) %>% 
      hc_plotOptions(inverted = T) %>% 
      hc_colors(vt.mig_trends_hc_colouring) %>% 
      hc_title(text = vt.chart_title,
			   style= list(fontSize="16px", fontFamily="Helvetica")) %>% 
      hc_xAxis(min = min(tmp.plot_data$CURR_YEAR) - 1,
               max = max(tmp.plot_data$CURR_YEAR) + 1,
               showFirstLabel = F,
               showLastLabel = F)
    
    if(!as.logical(vt.param_popn_perc)){
      hc = hc %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label + ': ' + '<b>' + this.y + '</b>')
    }"))
    }
    
    if(as.logical(vt.param_popn_perc)){
      hc = hc %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label + ': ' + '<b>' + (100 * this.y).toFixed(2)  + '%' + '</b>')
    }")) %>% 
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return((100 * this.value).toFixed(2) + '%')
        }")))
    }
    
  }
  
  if(vt.param_chart_type == "Line"){
    
    hc <- highchart() %>% 
      hc_add_series(
        data = tmp.plot_data,
        type = "line",
        hcaes(x = CURR_YEAR,
              y = VALUE,
              group = PLOT_LABEL,
              label = PLOT_LABEL)
      ) %>% 
      hc_colors(vt.mig_trends_hc_colouring) %>% 
      hc_title(text = vt.chart_title,
			   style= list(fontSize="16px", fontFamily="Helvetica"))
    
    if(!as.logical(vt.param_popn_perc)){
      hc = hc %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label + ': ' + '<b>' + this.y + '</b>')
    }"))
    }
    
    if(as.logical(vt.param_popn_perc)){
      hc = hc %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label + ': ' + '<b>' + (100 * this.y).toFixed(2)  + '%' + '</b>')
    }")) %>% 
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return((100 * this.value).toFixed(2) + '%')
    }")))
    }
    
  }
  
  hc = hc %>% 
    hc_legend(layout = 'horizontal',
              maxHeight = "100")
  
  return(hc)
  
})

output$mig_trend_source_target <- renderHighchart({
  
  vt.param_chart_type <- input$mig_trend_param_chart_type
  tmp.plot_data <- mig_trend_panel$source_target_plot_data
  
  validate(
    need(dim(tmp.plot_data)[1] > 0,
         "No data selected.")
  )
  
  #For title
  isolate({
    vt.param_flow <- input$mig_trend_param_flow
    vt.param_source <- input$mig_trend_param_source
    vt.param_target <- input$mig_trend_param_target
    vt.param_popn_perc <- input$mig_trend_param_perc
    vt.param_st_mig_type <- input$mig_trend_param_st_flow_type
  })
  
  vt.param_flow_value <- df.mig_trend_param_flow_map %>% 
    filter(NAME %in% vt.param_flow) %>% 
    .[["VALUE"]]
  
  vt.param_st_mig_type_value <- df.mig_trend_flow_type_mapping %>% 
    filter(FLOW_TYPE_LABEL == vt.param_st_mig_type) %>% 
    .[["FLOW_TYPE"]]
  
  vt.chart_title <- glue("{vt.param_flow} {ifelse(vt.param_flow_value %in% c('in', 'net'), 'to', 'from')} {ifelse(length(vt.param_source) > 1, paste0('{', paste(vt.param_source[1:(length(vt.param_source))], collapse = ', '), '}'), vt.param_source)}",
                         " {case_when(vt.param_flow_value == 'out' &  vt.param_st_mig_type_value == '2' ~ 'to',
                                      vt.param_flow_value %in% c('in', 'net') &  vt.param_st_mig_type_value == '2' ~ 'from',
                                      vt.param_st_mig_type_value == '3' & length(vt.param_target) == 1 ~ 'by Country of origin',
                                      vt.param_st_mig_type_value == '3' & length(vt.param_target) > 1 ~ 'by Country of origin')} {ifelse(length(vt.param_target) > 1, paste0('{', paste(vt.param_target[1:(length(vt.param_target))], collapse = ', '), '}'), vt.param_target)}.")
  
  
  if(vt.param_chart_type == "Bar"){
    
    hc <- highchart() %>% 
      hc_add_series(
        data = tmp.plot_data,
        type = "column",
        hcaes(x = CURR_YEAR,
              y = VALUE,
              group = PLOT_LABEL,
              label = PLOT_LABEL)
      ) %>% 
      hc_colors(vt.mig_trends_hc_colouring) %>% 
      hc_title(text = vt.chart_title,
			   style= list(fontSize="16px", fontFamily="Helvetica")) %>%
      hc_xAxis(min = min(tmp.plot_data$CURR_YEAR) - 1,
               max = max(tmp.plot_data$CURR_YEAR) + 1,
               showFirstLabel = F,
               showLastLabel = F)
    
    if(!as.logical(vt.param_popn_perc)){
      hc = hc %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label + ': ' + '<b>' + this.y + '</b>')
    }"))
    }
    
    if(as.logical(vt.param_popn_perc)){
      hc = hc %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label + ': ' + '<b>' + (100 * this.y).toFixed(2)  + '%' + '</b>')
    }")) %>% 
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return((100 * this.value).toFixed(2) + '%')
    }")))
    }
    
  }
  
  if(vt.param_chart_type == "Line"){
    
    hc <- highchart() %>% 
      hc_add_series(
        data = tmp.plot_data,
        type = "line",
        hcaes(x = CURR_YEAR,
              y = VALUE,
              group = PLOT_LABEL,
              label = PLOT_LABEL)
      ) %>% 
      hc_colors(vt.mig_trends_hc_colouring) %>% 
      hc_title(text = vt.chart_title,
			   style= list(fontSize="16px", fontFamily="Helvetica"))
    
    if(!as.logical(vt.param_popn_perc)){
      hc = hc %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label + ': ' + '<b>' + this.y + '</b>')
    }"))
    }
    
    if(as.logical(vt.param_popn_perc)){
      hc = hc %>%
        hc_tooltip(formatter = JS("function(){
                                  return(this.point.label + ': ' + '<b>' + (100 * this.y).toFixed(2)  + '%' + '</b>')
    }")) %>% 
        hc_yAxis(labels = list(formatter = JS("function(){
                                              return((100 * this.value).toFixed(2) + '%')
    }")))
    }
    
  }
  
  hc = hc %>% 
    hc_legend(layout = 'horizontal',
              maxHeight = "100")
  
  return(hc)
  
})