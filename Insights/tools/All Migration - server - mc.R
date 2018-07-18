#Server MC 

mig_all_panel <- reactiveValues(
  base_data = df.init_mig_all_base_data,
  map = spldf.init_mig_all_map,
  last_start_year = vt.init_mig_all_param_year_1,
  last_end_year = vt.init_mig_all_param_year_2,
  tmp_to_use = 1,
  last_agg_level = "Total NZ",
  vt.last_flow_dir = 'Inflow',
  vb_1_value = 'test',
  vb_1_text = 'test',
  vb_3_value = 'test',
  vb_3_text = 'test',
  vb_4_value = 'test',
  vb_4_text = 'test',
  vb_5_value = 'test',
  vb_5_text = 'test',
  ta_vb_1_value = 'test',
  ta_vb_1_text = 'test',
  ta_vb_3_value = 'test',
  ta_vb_3_text = 'test',
  ta_vb_4_value = 'test',
  ta_vb_4_text = 'test',
  map_init_refresh = 0
)

#Update the base data when the year slider changes
observeEvent(input$mig_all_param_year,{
  
  #Check for unecessary updates caused by the input being bounced after an invalid input
  #If the two inputs are directly adjacent and the user then puts them together,
  #they will get bounced back to where they were but we don't want to update the data
  if(mig_all_panel$last_start_year != input$mig_all_param_year[1] |
     mig_all_panel$last_end_year != input$mig_all_param_year[2]){             
    
    vt.param_ta <- input$mig_all_param_ta
    vt.param_agg_level <- input$mig_all_agg_level
    vt.param_year_1 <- input$mig_all_param_year[1]
    vt.param_year_2 <- input$mig_all_param_year[2]
    
    #If the user selects the same value for start and end (i.e. pinches the slider together)
    if(input$mig_all_param_year[1] == input$mig_all_param_year[2]){
      #If the first value has changed then move it back one.
      if(input$mig_all_param_year[1] != mig_all_panel$last_start_year &
         (input$mig_all_param_year[1] - 1) %in% vt.init_mig_all_param_year_1_select){
        updateSliderInput(
          session,
          "mig_all_param_year",
          label = "Year range selection",
          min = vt.init_mig_all_year_min,
          max = vt.init_mig_all_year_max,
          value = c(input$mig_all_param_year[1] - 1, input$mig_all_param_year[2]),
          step = 1
        )
        vt.param_year_1 <- input$mig_all_param_year[1] - 1
      }
      
      #If the second value has changed then move it forward one.
      if(input$mig_all_param_year[2] != mig_all_panel$last_end_year &
         (input$mig_all_param_year[2] + 1) %in% vt.init_mig_all_param_year_2_select){
        updateSliderInput(
          session,
          "mig_all_param_year",
          label = "Year range selection",
          min = vt.init_mig_all_year_min,
          max = vt.init_mig_all_year_max,
          value = c(input$mig_all_param_year[1], input$mig_all_param_year[2] + 1),
          step = 1
        )
        vt.param_year_2 <- input$mig_all_param_year[2] + 1
      }
    }
    
    #If the last update was cancelled and the slider was bounced back to the same position,
    #we don't need to update the data
    if((vt.param_year_1 != mig_all_panel$last_start_year |
        vt.param_year_2 != mig_all_panel$last_end_year) & 
       vt.param_year_1 != vt.param_year_2){
      #Update the base data
      if(vt.param_agg_level == "Total NZ"){
        df.base_data <- df.db_mig_data %>% 
          filter(
            PREV_YEAR %in% vt.param_year_1,
            CURR_YEAR %in% vt.param_year_2
          )
      }
      
      if(vt.param_agg_level == "Territorial authority"){
        df.base_data <- df.db_mig_data %>% 
          filter(
            PREV_YEAR %in% vt.param_year_1,
            CURR_YEAR %in% vt.param_year_2,
            TA %in% vt.param_ta
          )
      }
      
      mig_all_panel$base_data  <- df.base_data
    }
    
    #Record the last position of the slider
    mig_all_panel$last_start_year <- vt.param_year_1
    mig_all_panel$last_end_year <- vt.param_year_2
    
  }
})

#Update the base data if the TA changes or TA level
observe({
  vt.param_ta <- input$mig_all_param_ta
  vt.param_agg_level <- input$mig_all_agg_level
  
  isolate({
    vt.param_year_1 <- input$mig_all_param_year[1]
    vt.param_year_2 <- input$mig_all_param_year[2]
  })
  
  if(vt.param_agg_level == "Total NZ"){
    
    df.base_data <- df.db_mig_data %>% 
      filter(
        PREV_YEAR %in% vt.param_year_1,
        CURR_YEAR %in% vt.param_year_2
      )
    
    mig_all_panel$base_data  <- df.base_data
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    df.base_data <- df.db_mig_data %>% 
      filter(
        PREV_YEAR %in% vt.param_year_1,
        CURR_YEAR %in% vt.param_year_2,
        TA %in% vt.param_ta
      )
    
    if(dim(df.base_data)[1] > 0){
      mig_all_panel$base_data  <- df.base_data
    }
    
  }
  
})

#Update the map data when the base data, flow dir, TA, or agg_level updates
observe({
  df.base_data <- mig_all_panel$base_data
  
  vt.param_flow_name <- input$mig_all_param_flow
  vt.param_flow <- input$mig_all_param_flow
  vt.param_flow <- df.mig_all_param_flow_map %>% 
    filter(NAME == vt.param_flow) %>% 
    .[['VALUE']]
  
  vt.param_perc <- input$mig_all_param_map_perc
  vt.param_flow_type <- input$mig_all_param_flow_type
  
  vt.param_flow_type <- df.mig_flow_type_mapping %>% 
    filter(FLOW_TYPE_LABEL == vt.param_flow_type) %>% 
    .[["FLOW_TYPE"]]
  vt.param_agg_level <- input$mig_all_agg_level
  
  isolate({
    vt.last_agg_level <- mig_all_panel$last_agg_level
  })
  
  if(vt.param_agg_level == "Total NZ"){
    
    if(vt.param_agg_level != vt.last_agg_level){
      updateRadioButtons(session,
                         inputId = "mig_all_param_flow_type",
                         label = "Type of Population Change",
                         choices = vt.init_mig_all_parm_flow_type_select,
                         selected = vt.init_mig_all_parm_flow_type)
    }
    
    if(vt.param_flow_type != 'tot'){
      
      df.base_data = df.base_data %>% 
        filter(FLOW_DIR == vt.param_flow,
               FLOW_TYPE == vt.param_flow_type,
               is.na(FLOW_DEMOGRAPHIC),
               TA_LEVEL) %>% 
        inner_join(df.mig_flow_type_mapping,
                   by = "FLOW_TYPE")
      
    }
    
    if(vt.param_flow_type == 'tot'){
      
      df.base_data = df.base_data %>% 
        filter(is.na(FLOW_DEMOGRAPHIC),
               FLOW_DIR == vt.param_flow,
               FLOW_TYPE %in% 2:5) %>% 
        group_by(TA, TA_CODE, TOTAL_PREV, TOTAL_CURR, PREV_YEAR, CURR_YEAR, FLOW_DIR) %>%
        summarise(VALUE = sum(VALUE)) %>% 
        mutate(FLOW_TYPE = vt.param_flow_type)
    }
    
    #Convert to percentage of popn at start period
    if(vt.param_perc){
      df.base_data = df.base_data %>%
        mutate(VALUE = VALUE/TOTAL_PREV)
    }
    
    spldf.map_update <- spldf.nz_ta_migration
    spldf.map_update@data <- spldf.map_update@data %>%
      mutate(code = as.numeric(code)) %>% 
      left_join(df.base_data,
                by = c('code' = 'TA_CODE'))
    
    mig_all_panel$map <- spldf.map_update
    
  }
  
  
  if(vt.param_agg_level == "Territorial authority"){
    
    if(vt.param_agg_level != vt.last_agg_level){
      
      #Remove other choices at a TA level
      updateRadioButtons(session,
                         inputId = "mig_all_param_flow_type",
                         label = "Type of Population Change",
                         choices = vt.init_mig_all_parm_flow_type_ta_select,
                         selected = vt.init_mig_all_parm_flow_type_ta)
      
    }
    
    df.base_data = df.base_data %>% 
      filter(FLOW_DIR == vt.param_flow,
             FLOW_TYPE == 2,
             FLOW_DEMOGRAPHIC == "TA",
             TA_LEVEL)
    
    if(vt.param_perc){
      df.base_data = df.base_data %>%
        mutate(VALUE = VALUE/TOTAL_PREV)
    }
    
    spldf.map_update <- spldf.nz_ta_migration
    spldf.map_update@data <- spldf.map_update@data %>%
      left_join(df.base_data,
                by = c('code' = 'FLOW_DEMOGRAPHIC_VALUE'))
    
    mig_all_panel$map <- spldf.map_update
    
  }
  
  mig_all_panel$last_agg_level <- vt.param_agg_level
  
})

#When the aggregation level is total NZ then disable the TA selection
observeEvent(input$mig_all_agg_level,
             {
               if(input$mig_all_agg_level == "Total NZ"){
                 disable("mig_all_param_ta")
               }
               if(input$mig_all_agg_level != "Total NZ"){
                 enable("mig_all_param_ta")
               }
             })

#Help dialogue for chord diagram
observeEvent(input$mig_all_chord_modal_help,
             {
               
               vt.param_agg_level <- input$mig_all_agg_level
               
               if(vt.param_agg_level == "Total NZ"){
                 
                 vt.help_dialogue <- "The chord diagram shows the size and direction of the population moving between Territorial Authorities within NZ. The six TAs with the largest populations are identified separately, as well as Other North Island and Other South Island. Christchurch City has been combined with the Selwyn and Waimakariri Districts and is described as Greater Christchurch. Wellington City has been combined with Porirua, Lower Hutt, and Upper Hutt Cities and is described as Greater Wellington. The size of each segment around the edge of the circle represents the number of people who left that area to move to another area. The width of the chord at each end represents the number of people who moved to the area at the other end of the chord. Each chord is shaded in the colour representing the direction of the predominant movement. For example, chords are shaded with the same colour as the Auckland segment if more people arrived in Auckland than left. When 'Display as a percentage of the population?' is selected the diagram shows the flows as a percentage of the population at the start year of the areas they originate from e.g. if Christchurch -> Auckland is 0.50% this means that 0.50% of the size of the Christchurch population at the start year moved from Christchurch to Auckland. Move the mouse over a segment at the edge of the circle to see all movements related to that area, or over a chord to see more detail about the movements that chord represents."
                 
               }
               
               if(vt.param_agg_level == "Territorial authority"){
                 
                 vt.param_ta <- input$mig_all_param_ta
                 
                 vt.help_dialogue <- glue("The chord diagram shows the size and direction of the population moving to and from {vt.param_ta}. The five TAs with the largest flows to and from {vt.param_ta} are identified separately, as well as Other North Island and Other South Island. The size of each segment around the edge of the circle represents the number of people who left that area to move to another area. The width of the chord at each end represents the number of people who moved to the area at the other end of the chord. Each chord is shaded in the colour representing the direction of the predominant movement. For example, chords are shaded in with the same colour as the {vt.param_ta} segment if more people arrived in {vt.param_ta} than left. When 'Display as a percentage of the population?' is selected the diagram shows the flows as a percentage of the population at the start year of the areas they originate from e.g. if {vt.param_ta} -> {ifelse(vt.param_ta == 'Auckland', 'Christchurch', 'Auckland')} is 0.50% this means that 0.50% of the size of the {vt.param_ta} population at the start year moved from {vt.param_ta} to {ifelse(vt.param_ta == 'Auckland', 'Christchurch', 'Auckland')}. Move the mouse over a segment at the edge of the circle to see all movements related to that area, or over a chord to see more detail about the movements that chord represents.")
                 
               }
               
               showModal(modalDialog(
                 title = "About this chord diagram",
                 vt.help_dialogue,
                 easyClose = TRUE,
                 footer = NULL
               ))
               
             })

#Help dialogue for chord diagram
observeEvent(input$mig_all_pyramid_modal_help,
             {
               
               vt.param_agg_level <- input$mig_all_agg_level
               vt.param_start_year <- input$mig_all_param_year[1]
               vt.param_end_year <- input$mig_all_param_year[2]
               
               if(vt.param_agg_level == "Total NZ"){
                 
                 vt.help_dialogue <- glue("The leftmost population pyramid shows how the NZ population has changed from {vt.param_start_year} to {vt.param_end_year}. The drivers of this change are represented by the pyramids to the right, showing the effects of; international migration; and natural increase and ageing. Click the corresponding legend entry to select or unselect each factor.")
                 
               }
               
               if(vt.param_agg_level == "Territorial authority"){
                 
                 vt.param_ta <- input$mig_all_param_ta
                 
                 vt.help_dialogue <- glue("The leftmost population pyramid shows how the {vt.param_ta} population has changed from {vt.param_start_year} to {vt.param_end_year}. The drivers of this change are represented by the pyramids to the right, showing the effects of; migration within NZ; international migration; and natural increase and ageing. Click the corresponding legend entry to select or unselect each factor.")
                 
               }
               
               showModal(modalDialog(
                 title = "About this chord diagram",
                 vt.help_dialogue,
                 easyClose = TRUE,
                 footer = NULL
               ))
               
             })

#Help dialogue for the page
observeEvent({input$mig_all_page_modal_help},
             {
               
               vt.help_dialogue1 <- glue("Population explorer presents new data about population change in New Zealand (NZ) between {min(vt.mig_all_year_combn$PREV_YEAR)} and {max(vt.mig_all_year_combn$CURR_YEAR)} through interactive graphs and maps. Using this tool you can investigate changes in the population of New Zealand and specific territorial authorities over time. Different types of population change can be examined, including: internal migration within NZ, international migration to and from NZ, ageing, and births and deaths (natural increase).")
               
               vt.help_dialogue2 <-  glue("The grey boxes at the top of the page present key facts about changes in the population for any selected time period and area. The tabs below these boxes allow different aspects of population change to be examined and mapped.")
               
               vt.help_dialogue3 <-  glue("There are a number of controls on the left-hand side of the page which you can use to manipulate the data. You can select different time periods by adjusting the year range selection slider. The 'Display as a percentage?' checkbox allows you to view figures as percentages instead of numbers of people. The level of analysis buttons allow you to switch between figures for all of NZ and figures for a specific territorial authority within NZ. Hovering over the visualisations in this tool with your mouse pointer will reveal additional information.")
               
               showModal(modalDialog(
                 title = "About this tool",
                 vt.help_dialogue1,
                 br(),
                 br(),
                 vt.help_dialogue2,
                 br(),
                 br(),
                 vt.help_dialogue3,
                 easyClose = TRUE,
                 footer = NULL
               ))
               
             })

#Headlines for value boxes
observe({
  
  df.base_data <- mig_all_panel$base_data
  vt.param_start_year <- input$mig_all_param_year[1]
  vt.param_end_year <- input$mig_all_param_year[2]
  
  validate(need(vt.mig_all_year_combn %>% 
                  filter(PREV_YEAR %in% vt.param_start_year, CURR_YEAR %in% vt.param_end_year) %>%
                  dim() %>%
                  .[1] >= 1,
                "Updating..."))
  
  isolate({
    vt.param_agg_level <- input$mig_all_agg_level
    vt.param_ta <- input$mig_all_param_ta
  })
  
  if(vt.param_agg_level == "Total NZ"){
    
    #Get all different rows for the national vbs
    tmp.vb_data <- mig_all_panel$base_data %>%
      filter(FLOW_TYPE %in% 2:3 &
               FLOW_DIR == "out" &
               is.na(FLOW_DEMOGRAPHIC)  |
               (FLOW_TYPE == 3 &
                  is.na(FLOW_DEMOGRAPHIC) |
                  FLOW_DEMOGRAPHIC == "Country")|
               (FLOW_TYPE == 4))
    
    tmp.vb_2_data <- df.db_mig_data %>% 
      filter(PREV_YEAR == vt.param_start_year,
             CURR_YEAR == vt.param_end_year) %>%
      filter(FLOW_DIR == 'net',
             FLOW_TYPE %in% 2 &
               FLOW_DEMOGRAPHIC == "TA" &
               TA_LEVEL |
               FLOW_TYPE %in% 4 &
               is.na(FLOW_DEMOGRAPHIC_VALUE))
    
    #VB1
    mig_all_panel$vb_1_value <- tmp.vb_data %>% 
      filter(TA_CODE == 999) %>%
      slice(1) %>% 
      mutate(VALUE_PERC = paste0(formatC(100 * (TOTAL_CURR / TOTAL_PREV - 1),
                                         format = 'f',
                                         digits = 1),
                                 "%")) %>% 
      .[["VALUE_PERC"]]
    
    tmp.vb_1_test_1 <- tmp.vb_data %>%
      filter(TA_CODE == 999) %>%
      slice(1) %>% 
      mutate(test = TOTAL_CURR > TOTAL_PREV) %>% 
      .[['test']]
    
    tmp.vb_1_text_1 <- tmp.vb_data %>%
      filter(TA_CODE == 999,
             METRIC == "out_3") %>%
      mutate(VALUE_PERC = paste0(formatC(100 * (VALUE / TOTAL_PREV),
                                         format = 'f',
                                         digits = 1),
                                 "%")) %>% 
      .[["VALUE_PERC"]]
    
    tmp.vb_1_text_2 <- tmp.vb_data %>%
      filter(TA_CODE == 999,
             METRIC == "out_2") %>%
      mutate(VALUE_PERC = paste0(formatC(100 * (VALUE / TOTAL_PREV),
                                         format = 'f',
                                         digits = 1),
                                 "%")) %>% 
      .[["VALUE_PERC"]]
    
    tmp.vb_1_text_3 <- tmp.vb_data %>%
      filter(TA_CODE == 999) %>% 
      .[["TOTAL_PREV"]] %>% 
      unique()
    
    tmp.vb_1_text_4 <-  tmp.vb_data %>%
      filter(TA_CODE == 999) %>% 
      .[["TOTAL_CURR"]] %>% 
      unique()

    tmp.vb_1_text_5 <- tmp.vb_data %>%
      filter(TA_CODE == 999) %>% 
      mutate(DIFF = abs(TOTAL_CURR-TOTAL_PREV)) %>% 
      .[["DIFF"]] %>%
       unique()
   
	  
    mig_all_panel$vb_1_text <- glue({"{ifelse(tmp.vb_1_test_1, 'increase', 'decrease')} in the population of New Zealand between {vt.param_start_year} and {vt.param_end_year} from {comma(tmp.vb_1_text_3)} to {comma(tmp.vb_1_text_4)} ({ifelse(tmp.vb_1_test_1, 'an increase', 'a decrease')} of {comma(tmp.vb_1_text_5)}). Of the {vt.param_start_year} population {tmp.vb_1_text_1} left the country, while {tmp.vb_1_text_2} moved to another area"})
    
    #vb3
    tmp.vb_3_value_val <- (tmp.vb_data %>% 
                             filter(METRIC == "in_3",
                                    TA_CODE == 999) %>% 
                             .[["VALUE"]] - tmp.vb_data %>% 
                             filter(METRIC == "in_3_country_NZ",
                                    TA_CODE == 999) %>% 
                             .[["VALUE"]])
    
    tmp.vb_3_value <- tmp.vb_3_value_val %>% 
#      round(-3) %>% 
      comma()
    
    mig_all_panel$vb_3_value <- tmp.vb_3_value
    
    tmp.vb_3_text_1_val <- (tmp.vb_data %>% 
                              filter(METRIC == "out_3",
                                     TA_CODE == 999) %>% 
                              .[["VALUE"]] - tmp.vb_data %>% 
                              filter(METRIC == "out_3_country_NZ",
                                     TA_CODE == 999) %>% 
                              .[["VALUE"]])
    
    tmp.vb_3_text_1 <- tmp.vb_3_text_1_val %>% 
#      round(-3) %>% 
      comma()
    
    tmp.vb_3_text_2 <- tmp.vb_data %>% 
      filter(METRIC == "out_3",
             TA_CODE == 999) %>% 
      .[["TOTAL_PREV"]]
    
    tmp.vb_3_test <- (tmp.vb_3_value_val - tmp.vb_3_text_1_val) > 0
    
    tmp.vb_3_text_2 <- paste0(formatC(100 * (tmp.vb_3_value_val - tmp.vb_3_text_1_val) / tmp.vb_3_text_2,
                                      format = 'f',
                                      digits = 1),
                              "%")
							  
	tmp.vb_3_text_5 <- abs(tmp.vb_3_value_val - tmp.vb_3_text_1_val)						  
    
    tmp.vb_3_text_3 <- tmp.vb_data %>% 
      filter(FLOW_DEMOGRAPHIC == "Country",
             FLOW_TYPE == 3,
             FLOW_DIR == 'in',
             !(FLOW_DEMOGRAPHIC_VALUE %in% c("NZ", "ZZ")),
             TA_CODE == 999) %>% 
      filter(VALUE == max(VALUE))
    
    tmp.vb_3_text_4 <- tmp.vb_3_text_3 %>% 
      .[["VALUE"]]%>% 
#      round(-3) %>% 
      comma()
    
    tmp.vb_3_text_3 <- tmp.vb_3_text_3 %>% 
      inner_join(df.mig_all_world_country_vb_mapping,
                 by = c("FLOW_DEMOGRAPHIC_VALUE" = "ID")) %>% 
      .[["code"]]
    
    mig_all_panel$vb_3_text <- glue("overseas migrants arrived in New Zealand between {vt.param_start_year} and {vt.param_end_year}, while {tmp.vb_3_text_1} left. This represents {ifelse(tmp.vb_3_test, 'an increase', 'a decrease')} of {comma(tmp.vb_3_text_5)} or {tmp.vb_3_text_2} of the NZ population. The largest source of migrants was {tmp.vb_3_text_3} with {tmp.vb_3_text_4} arrivals.")
    
    #vb 4
    mig_all_panel$vb_4_value <- tmp.vb_data %>% 
      filter(METRIC == "in_3_country_NZ",
             TA_CODE == 999) %>% 
      .[["VALUE"]]%>% 
#      round(-3) %>% 
      comma()
    
    tmp.vb_4_text_1 <- tmp.vb_data %>% 
      filter(METRIC == "out_3_country_NZ",
             TA_CODE == 999) %>% 
      .[["VALUE"]] %>% 
#      round(-3) %>% 
      comma()
    
    tmp.vb_4_text_2 <- tmp.vb_data %>% 
      filter(METRIC == "net_3_country_NZ",
             TA_CODE == 999) %>% 
      mutate(VALUE_PERC = paste0(formatC(100 * abs(VALUE)/TOTAL_PREV,
                                         format = 'f',
                                         digits = 1),
                                 "%"))

    tmp.vb_4_text_5 <- tmp.vb_4_text_2 %>% 
	mutate(VALUE=abs(VALUE)) %>%
      .[["VALUE"]]

    tmp.vb_4_text_2 <- tmp.vb_4_text_2 %>% 
      .[["VALUE_PERC"]]
	  
    tmp.vb_4_test <- tmp.vb_data %>% 
      filter(METRIC == "net_3_country_NZ",
             TA_CODE == 999) %>% 
      mutate(test = VALUE > 0) %>% 
      .[['test']]
    
    mig_all_panel$vb_4_text <- glue("New Zealanders arrived in NZ by {vt.param_end_year} after living overseas in {vt.param_start_year}, while {tmp.vb_4_text_1} departed by {vt.param_end_year} after living in NZ in {vt.param_start_year}. This represents {ifelse(tmp.vb_4_test, 'an increase', 'a decrease')} of {comma(tmp.vb_4_text_5)} or {tmp.vb_4_text_2} of the NZ population.")
    
    mig_all_panel$vb_5_value <- tmp.vb_data %>% 
      filter(METRIC == "in_4",
             TA_CODE == 999) %>% 
      .[["VALUE"]] %>% 
      as.integer() %>% 
#      round(-(nchar(.) - 3)) %>% 
      comma()
    
    tmp.vb_5_text_1 <- tmp.vb_data %>% 
      filter(METRIC == "out_4",
             TA_CODE == 999) %>% 
      .[["VALUE"]] %>% 
      as.integer() %>% 
#      round(-(nchar(.) - 3)) %>% 
      comma()
    
    tmp.vb_5_text_2 <- tmp.vb_data %>% 
      filter(METRIC == "net_4",
             TA_CODE == 999) %>% 
      mutate(VALUE_PERC = paste0(formatC(100 * abs(VALUE) / TOTAL_PREV,
                                         format = 'f',
                                         digits = 1),
                                 "%"))
    
    tmp.vb_5_test <- tmp.vb_data %>% 
      filter(METRIC == "net_4",
             TA_CODE == 999) %>% 
      mutate(test = VALUE > 0) %>% 
      .[['test']]
    
    tmp.vb_5_text_3 <- tmp.vb_2_data %>% 
      filter(FLOW_TYPE == 4) %>% 
      mutate(VALUE_PERC = VALUE / TOTAL_PREV) %>% 
      filter(VALUE_PERC == max(VALUE_PERC, na.rm = T)) %>% 
      mutate(VALUE_PERC = paste0(formatC(100 * VALUE_PERC,
                                         format = 'f',
                                         digits = 1),
                                 "%"))
    
    tmp.vb_5_text_4 <- tmp.vb_5_text_3 %>% 
      .[["TA"]]
    
    tmp.vb_5_text_3 <- tmp.vb_5_text_3 %>% 
      .[["VALUE_PERC"]]

    tmp.vb_5_text_5 <- tmp.vb_5_text_2 %>% 
      .[["VALUE"]]

    tmp.vb_5_text_2 <- tmp.vb_5_text_2 %>% 
      .[["VALUE_PERC"]]
	  
    mig_all_panel$vb_5_text <- glue("children were born in New Zealand between June {vt.param_start_year} and June {vt.param_end_year} while {tmp.vb_5_text_1} people died. This represents a natural increase of {comma(tmp.vb_5_text_5)} or {tmp.vb_5_text_2}. The largest natural increase of {tmp.vb_5_text_3} was in {tmp.vb_5_text_4}.")
    
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    #vb1
    tmp.vb_data <- mig_all_panel$base_data
    
    #National facts for comparison
    tmp.vb_data_nat <- df.db_mig_data %>%
      filter(TA_CODE == 999,
             PREV_YEAR == vt.param_start_year,
             CURR_YEAR == vt.param_end_year,
             METRIC %in% c("net_3",
                           "net_3_country_NZ",
                           "net_4") |
               (FLOW_DEMOGRAPHIC == "Country" &
                  !is.na(FLOW_DEMOGRAPHIC_VALUE) &
                  FLOW_DEMOGRAPHIC_VALUE != "NZ" &
                  FLOW_DIR == 'net' &
                  FLOW_TYPE == 3)
      )
    
    #VB1
    tmp.ta_vb_1_value <- tmp.vb_data %>% 
      slice(1) %>% 
      mutate(VALUE = TOTAL_CURR / TOTAL_PREV - 1,
             VALUE_PERC = paste0(formatC(100 * abs(TOTAL_CURR / TOTAL_PREV - 1),
                                         format = 'f',
                                         digits = 1),
                                 "%"))
    
    tmp.ta_vb_1_value_test <- tmp.ta_vb_1_value %>% 
      .[["VALUE"]] > 0
    
    mig_all_panel$ta_vb_1_value <- tmp.ta_vb_1_value %>% 
      .[["VALUE_PERC"]]
    
    tmp.ta_vb_1_text_1 <- tmp.vb_data_nat %>% 
      slice(1) %>% 
      mutate(VALUE_PERC = paste0(formatC(100 * abs(TOTAL_CURR / TOTAL_PREV - 1),
                                         format = 'f',
                                         digits = 1),
                                 "%")) %>% 
      .[['VALUE_PERC']]
    
    tmp.ta_vb_1_text_1_test <- tmp.vb_data_nat %>% 
      slice(1) %>% 
      mutate(test = (TOTAL_CURR / TOTAL_PREV - 1) > 0) %>% 
      .[["test"]]
    
    tmp.ta_vb_1_text_2 <- tmp.vb_data %>% 
      filter(METRIC == "net_2") %>% 
      mutate(VALUE_PERC = paste0(formatC(100 * (VALUE / TOTAL_PREV),
                                         format = 'f',
                                         digits = 1),
                                 "%")) %>% 
      .[['VALUE_PERC']]

    tmp.ta_vb_1_text_6 <- tmp.vb_data %>% 
      filter(METRIC == "net_2") %>% 
      mutate(VALUE = abs(VALUE)) %>% 
      .[['VALUE']]
	  
    tmp.ta_vb_1_text_2_test <- tmp.vb_data %>% 
      filter(METRIC == "net_2") %>% 
      mutate(test = VALUE > 0) %>% 
      .[["test"]]
    
    tmp.ta_vb_1_text_3 <- tmp.vb_data %>% 
      .[["TOTAL_PREV"]] %>% 
      unique()
    
    tmp.ta_vb_1_text_4 <- tmp.vb_data %>% 
      .[["TOTAL_CURR"]] %>% 
      unique()
    	
    tmp.ta_vb_1_text_5 <- tmp.vb_data %>%
      mutate(DIFF = abs(TOTAL_CURR-TOTAL_PREV)) %>% 
      .[["DIFF"]] %>%
       unique()
	  
    tmp.ta_vb_1_text <- glue("{ifelse(tmp.ta_vb_1_value_test, 'increase', 'decrease')} in the population of {vt.param_ta} between {vt.param_start_year} and {vt.param_end_year} from {comma(tmp.ta_vb_1_text_3)} to {comma(tmp.ta_vb_1_text_4)} ({ifelse(tmp.ta_vb_1_value_test, 'an increase', 'a decrease')} of {comma(tmp.ta_vb_1_text_5)}). This compares to {ifelse(tmp.ta_vb_1_text_1_test, 'an increase', 'a decrease')} of {tmp.ta_vb_1_text_1} for all NZ. Migration within NZ contributed {tmp.ta_vb_1_text_2} ({ifelse(tmp.ta_vb_1_text_2_test, 'an increase', 'a decrease')} of {comma(tmp.ta_vb_1_text_6)}) to this {ifelse(tmp.ta_vb_1_value_test, 'increase', 'decrease')}.")
    
    #VB3
    
    mig_all_panel$ta_vb_3_value <- tmp.vb_data %>% 
      filter(FLOW_DEMOGRAPHIC == "Country",
             !is.na(FLOW_DEMOGRAPHIC_VALUE),
             FLOW_DEMOGRAPHIC_VALUE != "NZ",
             FLOW_DIR == 'in',
             FLOW_TYPE == 3) %>% 
      summarise(VALUE = sum(VALUE)) %>% 
      .[["VALUE"]] %>% 
      as.integer() %>% 
#      round(-(nchar(.) - 3)) %>% 
      comma()
    
    tmp.ta_vb_3_text_1 <-  tmp.vb_data %>% 
      filter(FLOW_DEMOGRAPHIC == "Country",
             !is.na(FLOW_DEMOGRAPHIC_VALUE),
             FLOW_DEMOGRAPHIC_VALUE != "NZ",
             FLOW_DIR == 'out',
             FLOW_TYPE == 3) %>% 
      summarise(VALUE = sum(VALUE)) %>% 
      .[["VALUE"]] %>% 
      as.integer() %>%
#      round(-(nchar(.) - 3)) %>% 
      comma()
    
    tmp.ta_vb_3_text_2 <-  tmp.vb_data %>% 
      filter(FLOW_DEMOGRAPHIC == "Country",
             !is.na(FLOW_DEMOGRAPHIC_VALUE),
             FLOW_DEMOGRAPHIC_VALUE != "NZ",
             FLOW_DIR == 'net',
             FLOW_TYPE == 3) %>% 
      summarise(VALUE = sum(VALUE),
                TOTAL_PREV = TOTAL_PREV[1]) %>%
      mutate(
        test = VALUE > 0,
        VALUE_PERC = paste0(formatC(100 * abs(VALUE/TOTAL_PREV),
                                    format = 'f',
                                    digits = 1),
                            "%"))
    
    tmp.ta_vb_3_text_2_test <- tmp.ta_vb_3_text_2 %>% 
      .[['test']]
    
	tmp.ta_vb_3_text_6 <- tmp.ta_vb_3_text_2 %>% 
      .[["VALUE"]]

    tmp.ta_vb_3_text_2 <- tmp.ta_vb_3_text_2 %>% 
      .[["VALUE_PERC"]]
 
    tmp.ta_vb_3_text_3 <- tmp.vb_data_nat %>% 
      filter(FLOW_DEMOGRAPHIC == "Country",
             !is.na(FLOW_DEMOGRAPHIC_VALUE),
             FLOW_DEMOGRAPHIC_VALUE != "NZ",
             FLOW_DIR == 'net',
             FLOW_TYPE == 3) %>% 
      summarise(VALUE = sum(VALUE),
                TOTAL_PREV = TOTAL_PREV[1]) %>%
      mutate(test = VALUE > 0,
             VALUE_PERC = paste0(formatC(100 * abs(VALUE)/TOTAL_PREV,
                                         format = 'f',
                                         digits = 1),
                                 "%")) 
    
    tmp.ta_vb_3_text_3_test <- tmp.ta_vb_3_text_3 %>% 
      .[['test']]
    
    tmp.ta_vb_3_text_3 <- tmp.ta_vb_3_text_3 %>% 
      .[['VALUE_PERC']]
    
    tmp.ta_vb_3_text_4 <- tmp.vb_data %>% 
      filter(FLOW_DEMOGRAPHIC == "Country",
             !is.na(FLOW_DEMOGRAPHIC_VALUE),
             FLOW_TYPE == 3,
             FLOW_DIR == 'in',
             !(FLOW_DEMOGRAPHIC_VALUE %in% c("NZ", "ZZ"))) %>% 
      filter(VALUE == max(VALUE)) %>%
      #Don't want ties
      slice(1) %>% 
      inner_join(df.mig_all_world_country_vb_mapping,
                 by = c("FLOW_DEMOGRAPHIC_VALUE" = 'ID')) %>% 
      .[["code"]]
    
    tmp.ta_vb_3_text_5 <- tmp.vb_data %>% 
      filter(FLOW_DEMOGRAPHIC == "Country",
             !is.na(FLOW_DEMOGRAPHIC_VALUE),
             FLOW_TYPE == 3,
             FLOW_DIR == 'in',
             !(FLOW_DEMOGRAPHIC_VALUE %in% c("NZ", "ZZ"))) %>% 
      filter(VALUE == max(VALUE)) %>% 
      #No ties
      slice(1) %>%
      .[["VALUE"]] %>% 
      as.integer() %>% 
#      round(-(nchar(.) - 3)) %>% 
      comma()
    
    tmp.ta_vb_3_text <- glue("overseas migrants arrived in {vt.param_ta} between {vt.param_start_year} and {vt.param_end_year}, while {tmp.ta_vb_3_text_1} left. This represents {ifelse(tmp.ta_vb_3_text_2_test, 'an increase', 'a decrease')} of {comma(tmp.ta_vb_3_text_6)} or {tmp.ta_vb_3_text_2} of the population, compared to a {tmp.ta_vb_3_text_3} {ifelse(tmp.ta_vb_3_text_3_test, 'increase', 'decrease')} nationally. The largest source of migrants was {tmp.ta_vb_3_text_4}, with {tmp.ta_vb_3_text_5} arrivals.")
    
    #VB4
    
    mig_all_panel$ta_vb_4_value <- tmp.vb_data %>% 
      filter(METRIC == "in_3_country_NZ") %>% 
      .[["VALUE"]] %>% 
      as.integer() %>% 
#      round(-(nchar(.) - 3)) %>% 
      comma()
    
    tmp.ta_vb_4_text_1 <- tmp.vb_data %>% 
      filter(METRIC == "out_3_country_NZ") %>% 
      .[["VALUE"]] %>% 
      as.integer() %>% 
#      round(-(nchar(.) - 3)) %>% 
      comma()
    
    tmp.ta_vb_4_text_2 <- tmp.vb_data %>% 
      filter(METRIC == "net_3_country_NZ") %>%
      mutate(VALUE_PERC = paste0(formatC(100 * abs(VALUE)/TOTAL_PREV,
                                         format = 'f',
                                         digits = 1),
                                 "%")) %>% 
      .[["VALUE_PERC"]]
    
    tmp.ta_vb_4_test_1 <- tmp.vb_data %>% 
      filter(METRIC == "net_3_country_NZ") %>%
      mutate(test = VALUE > 0) %>% 
      .[['test']]
    
    tmp.ta_vb_4_text_3 <- tmp.vb_data_nat %>% 
      filter(METRIC == "net_3_country_NZ") %>%
      mutate(VALUE_PERC = paste0(formatC(100 * abs(VALUE)/TOTAL_PREV,
                                         format = 'f',
                                         digits = 1),
                                 "%")) %>% 
      .[["VALUE_PERC"]]
 
    tmp.ta_vb_4_text_5 <- tmp.vb_data %>% 
      filter(METRIC == "net_3_country_NZ") %>%
      mutate(VALUE = abs(VALUE)) %>% 
      .[["VALUE"]]
 
    tmp.ta_vb_4_test_2 <- tmp.vb_data_nat %>% 
      filter(METRIC == "net_3_country_NZ") %>%
      mutate(test = VALUE > 0) %>% 
      .[['test']]
    
    tmp.ta_vb_4_text <- glue("New Zealanders arrived in {vt.param_ta} by {vt.param_end_year} after living overseas in {vt.param_start_year}, while {tmp.ta_vb_4_text_1} departed NZ by {vt.param_end_year} after living in {vt.param_ta} in {vt.param_start_year}. This represents {ifelse(tmp.ta_vb_4_test_1, 'an increase', 'a decrease')} of {comma(tmp.ta_vb_4_text_5)} or {tmp.ta_vb_4_text_2} of the {vt.param_ta} population, compared to {ifelse(tmp.ta_vb_4_test_2, 'an increase', 'a decrease')} of {tmp.ta_vb_4_text_3} across NZ.")
    
    #VB5
    
    tmp.ta_vb_5_value <- tmp.vb_data %>% 
      filter(METRIC == 'in_4') %>% 
      .[["VALUE"]] 
    
    mig_all_panel$ta_vb_5_value <- tmp.ta_vb_5_value %>% 
      as.integer() %>% 
#      round(-(nchar(.) - 3)) %>% 
      comma()
    
    tmp.ta_vb_5_text_1_val <- tmp.vb_data %>% 
      filter(METRIC == 'out_4') %>% 
      .[["VALUE"]] 
    
    tmp.ta_vb_5_text_1 <- tmp.ta_vb_5_text_1_val%>% 
      as.integer() %>% 
#      round(-(nchar(.) - 3)) %>% 
      comma()
    
    tmp.ta_vb_5_test_1 <- tmp.ta_vb_5_value > tmp.ta_vb_5_text_1_val
    
    tmp.ta_vb_5_text_2 <- paste0(formatC(100 * (tmp.ta_vb_5_value - tmp.ta_vb_5_text_1_val) / tmp.vb_data %>% 
                                           filter(METRIC == 'in_4') %>% 
                                           .[["TOTAL_PREV"]],
                                         format = 'f',
                                         digits = 1),
                                 "%")
    
    tmp.ta_vb_5_text_5 <- abs(tmp.ta_vb_5_value - tmp.ta_vb_5_text_1_val)
	
    tmp.ta_vb_5_text_3 <- tmp.vb_data_nat %>% 
      filter(METRIC == 'net_4') %>% 
      mutate(VALUE_PERC = paste0(formatC(100 * VALUE / TOTAL_PREV,
                                         format = 'f',
                                         digits = 1),
                                 "%")) %>% 
      .[["VALUE_PERC"]]
    
    tmp.ta_vb_5_text_3_test <- tmp.vb_data_nat %>% 
      filter(METRIC == 'net_4') %>% 
      .[['VALUE']] > 0
    
    tmp.ta_vb_5_text <- glue("children were born in {vt.param_ta} between June {vt.param_start_year} and June {vt.param_end_year} while {tmp.ta_vb_5_text_1} people died. This represents a natural {ifelse(tmp.ta_vb_5_test_1, 'increase', 'decrease')} of {comma(tmp.ta_vb_5_text_5)} or {tmp.ta_vb_5_text_2}, compared to a natural {ifelse(tmp.ta_vb_5_text_3_test, 'increase', 'decrease')} of {tmp.ta_vb_5_text_3} across New Zealand.")
    
    mig_all_panel$ta_vb_1_text <- tmp.ta_vb_1_text
    mig_all_panel$ta_vb_3_text <- tmp.ta_vb_3_text
    mig_all_panel$ta_vb_4_text <- tmp.ta_vb_4_text
    mig_all_panel$ta_vb_5_text <- tmp.ta_vb_5_text
    
  }
  
})

#Add map download
output$mig_all_map_download <- downloadHandler(
  filename = function(){
    
    vt.param_agg_level <- input$mig_all_agg_level
    
    if(vt.param_agg_level == "Total NZ"){
      return(paste0("NZ_migration_map_data_", format(Sys.time(), "%x_%H:%M"), ".csv"))
    } else{
      vt.param_ta <- input$mig_all_param_ta
      
      return(paste0(vt.param_ta, "_migration_map_data_", format(Sys.time(), "%x_%H:%M"), ".csv"))
    }
    
  },
  content = function(file) {
    
    # browser()
    
    isolate({
      vt.param_agg = input$mig_all_agg_level
      vt.param_perc = input$mig_all_param_map_perc
      vt.param_map_flow = input$mig_all_param_flow
    })
    
    df = mig_all_panel$map@data
    
    if(vt.param_agg == vt.init_mig_all_agg_level_select[1]){
      
      if(!vt.param_perc){
        
        df = national_map_csv_reformat(df, df.mig_all_map_national_rename_absolute)
        
      } else{
        
        df = df %>% 
        mutate(VALUE = paste0(formatC(100 * VALUE,
          format = 'f',
          digits = 2),
          "%"))
        
        df = national_map_csv_reformat(df, df.mig_all_map_national_rename_percentage)
        
      }
      
    } else{
      
      vt.flow_dir_value = df.mig_natural_change_flow_dir_mapping %>% 
        filter(FLOW_DIR %in% vt.param_map_flow) %>% 
        .[["FLOW_DIR_VALUE"]]
      
      if(!vt.param_perc){
        
        if(vt.flow_dir_value %in% vt.mig_all_param_flow_dir_in){
          
          df = ta_map_csv_reformat(df, df.mig_all_map_ta_rename_in_absolute)
          
        } else{
          
          df = ta_map_csv_reformat(df, df.mig_all_map_ta_rename_out_absolute)
          
        }
        
      } else{
        
        if(vt.flow_dir_value %in% vt.mig_all_param_flow_dir_in){
          
          df = df %>% 
            mutate(VALUE = paste0(formatC(100 * VALUE,
                                          format = 'f',
                                          digits = 2),
                                  "%"))
          
          df = ta_map_csv_reformat(df, df.mig_all_map_ta_rename_in_percentage)
          
        } else{
          
          df = df %>% 
            mutate(VALUE = paste0(formatC(100 * VALUE,
                                          format = 'f',
                                          digits = 2),
                                  "%"))
          
          df = ta_map_csv_reformat(df, df.mig_all_map_ta_rename_out_percentage)
          
        }
      }
    }
    
    write.csv(df, file, row.names = F) 
  })
  
#Invalidate the map the first time it's displayed.
observeEvent({input$grey_tabBox},
             { 
               if(mig_all_panel$map_init_refresh == 0 &
                  input$grey_tabBox == vt.mig_all_map_tab_title){
                 mig_all_panel$map_init_refresh <- mig_all_panel$map_init_refresh + 1
               }
             })  