#-------------------------------------------------------------------------------
#NZ Heat Map
#-------------------------------------------------------------------------------

#render the base map
output$mig_all_map <- renderLeaflet({
  
  spldf.plot <- spldf.init_mig_all_map
  
  vt.mig_all_col_out <- df.mig_all_flow_type_col_map_mapping %>% 
    filter(FLOW_TYPE == 'tot') %>% 
    .[["COL_OUT"]]
  vt.mig_all_col_in <- df.mig_all_flow_type_col_map_mapping %>% 
    filter(FLOW_TYPE == 'tot') %>% 
    .[["COL_IN"]]
  
  vt.mig_all_param_flow_name <- vt.init_mig_all_param_flow
  vt.mig_all_param_flow <- df.mig_all_param_flow_map %>% 
    filter(NAME == vt.init_mig_all_param_flow) %>% 
    .[['VALUE']]
  vt.param_perc <- vt.init_mig_all_param_map_perc
  
  #Define the colour palette for the map
  
  pal.vals = spldf.plot@data %>%
    .[['VALUE']]
  
  #center the colour scale on 0
  pal.vals_extended = c(pal.vals, ifelse(sign(pal.vals[which(abs(pal.vals) == max(abs(pal.vals)))][1]) == 1,
                                         -max(pal.vals),
                                         -min(pal.vals)))
  
  pal.map <- colorNumeric(c(vt.mig_all_col_out, vt.mig_all_col_neutral_map, vt.mig_all_col_in),
                          pal.vals_extended)
  
  spldf.plot@data <- spldf.plot@data %>% 
    mutate(COLOUR = pal.map(VALUE)) %>% 
    mutate(COLOUR = ifelse(!is.na(COLOUR), COLOUR, "#808080"))
  
  if(!vt.param_perc){
    spldf.plot@data = spldf.plot@data %>% 
      #need .$ for dplyr 0.5
      mutate(LABEL = glue("{.$ID} {formatC(.$VALUE, format = 'd', big.mark = ',')}"))
  } else{
    spldf.plot@data = spldf.plot@data %>% 
      mutate(LABEL = glue("{.$ID} {signif(.$VALUE * 100, digits = 2)}%"))
  }
  
  withProgress(message = 'Making plot', value = 0, {
    leaflet() %>%
      addProviderTiles(
        providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addPolygons(
        data = spldf.plot,
        fillColor = ~COLOUR,
        fillOpacity = 0.9,
        stroke = TRUE,
        weight = 1,
        color = "black",
        dashArray = c(5, 5),
        smoothFactor = 0.2,
        highlightOptions = highlightOptions(
          color='#ff0000',
          opacity = 1,
          weight = 2,
          fillOpacity = 1,
          sendToBack = FALSE
        ),
        label = ~LABEL,
        layerId = ~code
      ) %>%
      addLegend(position = "bottomright", 
                opacity = 1,
                pal = pal.map,
                values = pal.vals,
                title = glue("{vt.mig_all_param_flow_name} for each TA"), 
                layerId = "Legend",
                na.label = "S")
  })
  
})

#Map update - when panel map data updates
observeEvent({
  mig_all_panel$map
  mig_all_panel$map_init_refresh},
             {
               
               vt.param_agg_level <- input$mig_all_agg_level
               vt.param_flow <- input$mig_all_param_flow
               
               vt.param_flow_name <- vt.param_flow
               vt.param_flow <- df.mig_all_param_flow_map %>% 
                 filter(NAME == vt.param_flow) %>% 
                 .[['VALUE']]
               
               if(vt.param_agg_level == "Total NZ"){
                 
                 #updated data to plot
                 spldf.plot <- mig_all_panel$map
                 
                 if (nrow(spldf.plot@data) > 0) {
                   
                   isolate({
                     vt.mig_all_param_flow <- input$mig_all_param_flow
                     vt.mig_all_param_perc <- input$mig_all_param_map_perc
                     vt.mig_all_param_flow_type <- input$mig_all_param_flow_type
                     vt.param_start_year <- unique(spldf.plot@data$PREV_YEAR)[1]
                   })
                   
                   vt.param_flow_type_value <- df.mig_trend_flow_type_mapping %>% 
                     filter(FLOW_TYPE_LABEL == vt.mig_all_param_flow_type) %>% 
                     .[["FLOW_TYPE"]]
                   
                   vt.mig_all_col_in <- df.mig_all_flow_type_col_map_mapping %>%
                     filter(FLOW_TYPE == vt.param_flow_type_value) %>% 
                     .[["COL_IN"]]
                   vt.mig_all_col_out <- df.mig_all_flow_type_col_map_mapping %>%
                     filter(FLOW_TYPE == vt.param_flow_type_value) %>% 
                     .[["COL_OUT"]]
                   
                   #Rename the flow label for natural change
                   if(vt.param_flow_type_value == 4){
                     
                     vt.param_flow_name <- df.mig_natural_change_flow_dir_mapping %>% 
                       filter(FLOW_DIR_VALUE == vt.param_flow)  %>% 
                       .[["FLOW_DIR_LABEL"]]
                     
                   }
                   
                   vt.mig_all_param_flow <- df.mig_all_param_flow_map %>% 
                     filter(NAME == vt.mig_all_param_flow) %>% 
                     .[['VALUE']]
                   
                   #Define colour palette (different colours for different migration directions)
                   
                   #Inflow
                   if(vt.mig_all_param_flow == 'in'){
                     pal.vals = spldf.plot@data %>%
                       .[['VALUE']]
                     pal.vals_extended = c(pal.vals, 0)
                     pal.map <- colorNumeric(c(vt.mig_all_col_neutral_map, vt.mig_all_col_in),
                                             pal.vals_extended)
                   }
                   
                   #Outflow
                   if(vt.mig_all_param_flow == 'out'){
                     pal.vals = spldf.plot@data %>%
                       .[['VALUE']]
                     pal.vals_extended = c(pal.vals, 0)
                     pal.map <- colorNumeric(c(vt.mig_all_col_neutral_map, vt.mig_all_col_out),
                                             pal.vals_extended)
                   }
                   
                   #Netflow
                   if(vt.mig_all_param_flow == 'net'){
                     pal.vals = spldf.plot@data %>%
                       .[['VALUE']]
                     
                     #center the colour scale on 0
                     pal.vals_extended = c(pal.vals, ifelse(sign(pal.vals[which(abs(pal.vals) == max(abs(pal.vals)))][1]) == 1,
                                                            -max(pal.vals),
                                                            -min(pal.vals)))
                     
                     pal.map <- colorNumeric(c(vt.mig_all_col_out, vt.mig_all_col_neutral_map, vt.mig_all_col_in),
                                             pal.vals_extended)
                   }
                   
                   
                   spldf.plot@data = spldf.plot@data %>%
                     mutate(COLOUR = pal.map(VALUE))
                   
                   vt.area_id <- unique(spldf.plot@data$code)
                   
                   if(!vt.mig_all_param_perc){
                     spldf.plot@data = spldf.plot@data %>% 
                       #need .$ for dplyr 0.5
                       mutate(LABEL = glue("{.$ID} {formatC(.$VALUE, format = 'd', big.mark = ',')}"))
                     
                     #Normal Legend
                     leafletProxy("mig_all_map") %>%
                       removeControl(layerId = "Legend") %>%
                       clearShapes() %>% 
                       addPolygons(
                         data = spldf.plot,
                         fillColor = ~COLOUR,
                         fillOpacity = 0.9,
                         stroke = TRUE,
                         weight = 1,
                         color = "black",
                         dashArray = c(5, 5),
                         smoothFactor = 0.2,
                         highlightOptions = highlightOptions(
                           color='#ff0000',
                           opacity = 1,
                           weight = 2,
                           fillOpacity = 1,
                           sendToBack = FALSE
                         ),
                         label = ~LABEL,
                         layerId = ~code
                       ) %>%
                       addLegend(position = "bottomright", 
                                 opacity = 1,
                                 pal = pal.map,
                                 values = pal.vals,
                                 title = glue(case_when(
                                   vt.mig_all_param_flow == "in" & vt.param_flow_type_value == 4 ~ as.character(glue("Births within each TA")),
                                   vt.mig_all_param_flow == "out" & vt.param_flow_type_value == 4 ~ as.character(glue("Deaths within each TA")),
                                   vt.mig_all_param_flow == "net" & vt.param_flow_type_value == 4 ~ as.character(glue("Natural increase within each TA")),
                                   vt.mig_all_param_flow == "in" ~ as.character(glue("{vt.param_flow_name} to each TA")),
                                   vt.mig_all_param_flow == "out" ~ as.character(glue("{vt.param_flow_name} from each TA")),
                                   vt.mig_all_param_flow == "net" ~ as.character(glue("{vt.param_flow_name} to each TA")))),
                                 layerId = "Legend",
                                 na.label = "S")
                     
                   } else{
                     spldf.plot@data = spldf.plot@data %>% 
                       mutate(LABEL = glue("{.$ID} {signif(.$VALUE * 100, digits = 2)}%"))
                     
                     #Percentage Legend
                     leafletProxy("mig_all_map") %>%
                       removeControl(layerId = "Legend") %>%
                       clearShapes() %>% 
                       addPolygons(
                         data = spldf.plot,
                         fillColor = ~COLOUR,
                         fillOpacity = 0.9,
                         stroke = TRUE,
                         weight = 1,
                         color = "black",
                         dashArray = c(5, 5),
                         smoothFactor = 0.2,
                         highlightOptions = highlightOptions(
                           color='#ff0000',
                           opacity = 1,
                           weight = 2,
                           fillOpacity = 1,
                           sendToBack = FALSE
                         ),
                         label = ~LABEL,
                         layerId = ~code
                       ) %>%
                       addLegend(position = "bottomright", 
                                 opacity = 1,
                                 pal = pal.map,
                                 values = pal.vals,
                                 labFormat = labelFormat(suffix = "%",
                                                         transform = function(x) x * 100),
                                 title = glue("{vt.param_flow_name} as a percentage of the {vt.param_start_year} population"), 
                                 layerId = "Legend",
                                 na.label = "S")
                   }
                 }
               }
               
               if(vt.param_agg_level == "Territorial authority"){
                 
                 #updated data to plot
                 spldf.plot <- mig_all_panel$map
                 
                 if(nrow(spldf.plot@data) > 0){
                   
                   isolate({
                     vt.mig_all_param_ta <- input$mig_all_param_ta
                     vt.mig_all_param_flow <- input$mig_all_param_flow
                     vt.mig_all_param_flow_type <- input$mig_all_param_flow_type
                     vt.param_perc <- input$mig_all_param_map_perc
                     vt.param_start_year <- unique(spldf.plot@data$PREV_YEAR)[1]
                   })
                   
                   vt.mig_all_param_flow_name <- vt.mig_all_param_flow
                   vt.mig_all_param_flow <- df.mig_all_param_flow_map %>% 
                     filter(NAME == vt.mig_all_param_flow) %>% 
                     .[['VALUE']]
                   
                   vt.param_flow_type_value <- df.mig_trend_flow_type_mapping %>% 
                     filter(FLOW_TYPE_LABEL == vt.mig_all_param_flow_type) %>% 
                     .[["FLOW_TYPE"]]
                   
                   #Define colour palette (different colours for different migration directions)
                   vt.mig_all_col_in <- df.mig_all_flow_type_col_map_mapping %>%
                     filter(FLOW_TYPE == vt.param_flow_type_value) %>% 
                     .[["COL_IN"]]
                   vt.mig_all_col_out <- df.mig_all_flow_type_col_map_mapping %>%
                     filter(FLOW_TYPE == vt.param_flow_type_value) %>% 
                     .[["COL_OUT"]]
                   
                   #Inflow
                   if(vt.param_flow == 'in'){
                     pal.vals = spldf.plot@data %>%
                       .[['VALUE']]
                     pal.vals_extended = c(pal.vals, 0)
                     pal.map <- colorNumeric(c(vt.mig_all_col_neutral_map, vt.mig_all_col_in),
                                             pal.vals_extended)
                   }
                   
                   #Outflow
                   if(vt.param_flow == 'out'){
                     pal.vals = spldf.plot@data %>%
                       .[['VALUE']]
                     pal.vals_extended = c(pal.vals, 0)
                     pal.map <- colorNumeric(c(vt.mig_all_col_neutral_map, vt.mig_all_col_out),
                                             pal.vals_extended)
                   }
                   
                   #Netflow
                   if(vt.param_flow == 'net'){
                     pal.vals = spldf.plot@data %>%
                       .[['VALUE']]
                     
                     #center the colour scale on 0
                     pal.vals_extended = c(pal.vals, ifelse(sign(pal.vals[which(abs(pal.vals) == max(abs(pal.vals)))][1]) == 1,
                                                            -max(pal.vals),
                                                            -min(pal.vals)))
                     
                     pal.map <- colorNumeric(c(vt.mig_all_col_out, vt.mig_all_col_neutral_map, vt.mig_all_col_in),
                                             pal.vals_extended)
                   }
                   
                   vt.area_id <- unique(spldf.plot@data$code)
                   
                   spldf.plot@data = spldf.plot@data %>%
                     mutate(COLOUR = pal.map(VALUE)) %>%
                     mutate(COLOUR = ifelse(ID != vt.mig_all_param_ta, COLOUR, vt.mig_all_map_selected_fill)) %>% 
                     mutate(STROKE = "#000000") %>%
                     mutate(STROKE = ifelse(ID != vt.mig_all_param_ta, STROKE, vt.mig_all_map_selected_outline))
                   
                   if(!vt.param_perc){
                     spldf.plot@data = spldf.plot@data %>% 
                       #need .$ for dplyr 0.5
                       mutate(LABEL = ifelse(ID != vt.mig_all_param_ta,
                                             glue("{.$ID} {formatC(.$VALUE, format = 'd', big.mark = ',')}"),
                                             glue("{.$ID} Selected")))                   
                     #Normal Legend
                     leafletProxy("mig_all_map") %>%
                       removeControl(layerId = "Legend") %>%
                       clearShapes() %>% 
                       addPolygons(
                         data = spldf.plot,
                         fillColor = ~COLOUR,
                         fillOpacity = 0.9,
                         stroke = TRUE,
                         weight = 1,
                         color = ~STROKE,
                         dashArray = c(5, 5),
                         smoothFactor = 0.2,
                         highlightOptions = highlightOptions(
                           color='#ff0000',
                           opacity = 1,
                           weight = 2,
                           fillOpacity = 1,
                           sendToBack = FALSE
                         ),
                         label = ~LABEL,
                         layerId = ~code
                       ) %>%
                       addLegend(position = "bottomright", 
                                 opacity = 1,
                                 pal = pal.map,
                                 values = pal.vals,
                                 title = case_when(vt.mig_all_param_flow == "in" ~ as.character(glue("{vt.mig_all_param_flow_name} to {vt.mig_all_param_ta} from other TAs")),
                                                   vt.mig_all_param_flow == "out" ~ as.character(glue("{vt.mig_all_param_flow_name} from {vt.mig_all_param_ta} to other TAs")),
                                                   vt.mig_all_param_flow == "net" ~ as.character(glue("{vt.mig_all_param_flow_name} to {vt.mig_all_param_ta} from other TAs"))),
                                 layerId = "Legend",
                                 na.label = "S")
                     
                   } else{
                     spldf.plot@data = spldf.plot@data %>% 
                       mutate(LABEL = ifelse(ID != vt.mig_all_param_ta,
                                             glue("{.$ID} {signif(.$VALUE * 100, digits = 2)}%"),
                                             glue("{.$ID} Selected")))
                     
                     #Percentage Legend
                     leafletProxy("mig_all_map") %>%
                       removeControl(layerId = "Legend") %>%
                       clearShapes() %>% 
                       addPolygons(
                         data = spldf.plot,
                         fillColor = ~COLOUR,
                         fillOpacity = 0.9,
                         stroke = TRUE,
                         weight = 1,
                         color = ~STROKE,
                         dashArray = c(5, 5),
                         smoothFactor = 0.2,
                         highlightOptions = highlightOptions(
                           color='#ff0000',
                           opacity = 1,
                           weight = 2,
                           fillOpacity = 1,
                           sendToBack = FALSE
                         ),
                         label = ~LABEL,
                         layerId = ~code
                       ) %>%
                       addLegend(position = "bottomright", 
                                 opacity = 1,
                                 pal = pal.map,
                                 values = pal.vals,
                                 labFormat = labelFormat(suffix = "%",
                                                         transform = function(x) x * 100),
                                 title = case_when(vt.mig_all_param_flow == "in" ~ as.character(glue("{vt.mig_all_param_flow_name} to {vt.mig_all_param_ta} as a percentage <br> of the {vt.param_start_year} {vt.mig_all_param_ta} population")),
                                                   vt.mig_all_param_flow == "out" ~ as.character(glue("{vt.mig_all_param_flow_name} from {vt.mig_all_param_ta} as a percentage <br> of the {vt.param_start_year} {vt.mig_all_param_ta} population")),
                                                   vt.mig_all_param_flow == "net" ~ as.character(glue("{vt.mig_all_param_flow_name} to {vt.mig_all_param_ta} as a percentage <br> of the {vt.param_start_year} {vt.mig_all_param_ta} population"))),
                                 layerId = "Legend",
                                 na.label = "S")
                   }
                 }
               }
               
             })

#Click handler - switch to clicked TA, update select input
observeEvent(input$mig_all_map_shape_click,
             {
               
               tmp.ta_code <- input$mig_all_map_shape_click$id %>% 
                 as.numeric()
               
               vt.param_ta <- spldf.nz_ta_migration@data %>% 
                 mutate(code = as.numeric(code)) %>% 
                 filter(code %in% tmp.ta_code) %>%
                 .[["ID"]]
               
               updateSelectInput(
                 session,
                 inputId = "mig_all_param_ta",
                 label = "Territorial authority",
                 choices = vt.init_mig_all_param_ta_select,
                 selected = vt.param_ta
               )
               
               #If at national level zoom to TA level
               vt.param_agg <- input$mig_all_agg_level
               
               if(vt.param_agg == "Total NZ"){
                 
                 updateRadioButtons(
                   session,
                   "mig_all_agg_level",
                   choices = vt.init_mig_all_agg_level_select,
                   selected = "Territorial authority"
                 ) 
               }
             })