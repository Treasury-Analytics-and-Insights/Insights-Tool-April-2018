output$mig_all_valuebox_1 <- renderValueBox({

  vt.param_agg_level <- input$mig_all_agg_level
  mig_all_panel$vb_1_value
  mig_all_panel$vb_1_text
  
  isolate({
    vt.param_start_year <- input$mig_all_param_year[1]
    vt.param_end_year <- input$mig_all_param_year[1]
  })
  
  if(vt.param_agg_level == "Total NZ"){
    
    return(
      local_value_box(value = mig_all_panel$vb_1_value,
                      subtitle = mig_all_panel$vb_1_text,
                      color = 'purple',
                      width = NULL,
                      icon = local_icon(name = list(src = "img/icons/tsy-icons_male-female.png",
                                                    width = df.mig_all_img_dim %>% 
                                                      filter(icon == 'male-female') %>% 
                                                      .[['height_perc']])))
    )
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    return(
      local_value_box(mig_all_panel$ta_vb_1_value,
               mig_all_panel$ta_vb_1_text,
               color = 'purple',
               width = NULL,
               icon = local_icon(name = list(src = "img/icons/tsy-icons_male-female.png",
                                             width = df.mig_all_img_dim %>% 
                                               filter(icon == 'male-female') %>% 
                                               .[['height_perc']])))
    )
  }
})

output$mig_all_valuebox_2 <- renderValueBox({
  vt.param_agg_level <- input$mig_all_agg_level
  mig_all_panel$vb_5_value
  mig_all_panel$vb_5_text
  
  isolate({
    vt.param_start_year <- input$mig_all_param_year[1]
    vt.param_end_year <- input$mig_all_param_year[2]
  })
  
  if(vt.param_agg_level == "Total NZ"){
    
    
    return(
      local_value_box(mig_all_panel$vb_5_value,
               mig_all_panel$vb_5_text,
               color = 'purple',
               width = NULL,
               icon = local_icon(name = list(src = "img/icons/tsy-icons_pattern.png",
                                             width = df.mig_all_img_dim %>% 
                                               filter(icon == 'pattern') %>% 
                                               .[['height_perc']])))
    )
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    return(
      local_value_box(mig_all_panel$ta_vb_5_value,
               mig_all_panel$ta_vb_5_text,
               color = 'purple',
               width = NULL,
               icon = local_icon(name = list(src = "img/icons/tsy-icons_pattern.png",
                                             width = df.mig_all_img_dim %>% 
                                               filter(icon == 'pattern') %>% 
                                               .[['height_perc']])))
    )
  }
})

output$mig_all_valuebox_3 <- renderValueBox({
  vt.param_agg_level <- input$mig_all_agg_level
  mig_all_panel$vb_3_value
  mig_all_panel$vb_3_text  
  
  isolate({
    vt.param_start_year <- input$mig_all_param_year[1]
    vt.param_end_year <- input$mig_all_param_year[2]
  })

  if(vt.param_agg_level == "Total NZ"){
    
    return(
      local_value_box(mig_all_panel$vb_3_value,
               mig_all_panel$vb_3_text,
               color = 'purple',
               width = NULL,
               icon = local_icon(name = list(src = "img/icons/tsy-icons_globe.png",
                                             width = df.mig_all_img_dim %>% 
                                               filter(icon == 'globe') %>% 
                                               .[['height_perc']])))
    )
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    return(
      local_value_box(mig_all_panel$ta_vb_3_value,
               mig_all_panel$ta_vb_3_text,
               color = 'purple',
               width = NULL,
               icon = local_icon(name = list(src = "img/icons/tsy-icons_globe.png",
                                             width = df.mig_all_img_dim %>% 
                                               filter(icon == 'globe') %>% 
                                               .[['height_perc']])))
    )
  }
})

output$mig_all_valuebox_4 <- renderValueBox({
  vt.param_agg_level <- input$mig_all_agg_level
  mig_all_panel$vb_4_value
  mig_all_panel$vb_4_text
  
  isolate({
    vt.param_start_year <- input$mig_all_param_year[1]
    vt.param_end_year <- input$mig_all_param_year[2]
  })
  
  if(vt.param_agg_level == "Total NZ"){
    
    return(
      local_value_box(mig_all_panel$vb_4_value,
               mig_all_panel$vb_4_text,
               color = 'purple',
               width = NULL,
               icon = local_icon(name = list(src = "img/icons/tsy-icons_bird.png",
                                             width = df.mig_all_img_dim %>% 
                                               filter(icon == 'bird') %>% 
                                               .[['height_perc']])))
    )
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    return(
      local_value_box(mig_all_panel$ta_vb_4_value,
               mig_all_panel$ta_vb_4_text,
               color = 'purple',
               width = NULL,
               icon = local_icon(name = list(src = "img/icons/tsy-icons_bird.png",
                                             width = df.mig_all_img_dim %>% 
                                               filter(icon == 'bird') %>% 
                                               .[['height_perc']])))
    )
  }
})

output$mig_all_chord_title <- renderText({
  
  mig_all_panel$base_data
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
    
    if(!vt.param_perc){
      
      title_str <- glue("Migration within NZ from {vt.param_start_year} to {vt.param_end_year}")
      
    }
    
    if(vt.param_perc){
      
      title_str <- glue("Migration within NZ as a percentage of the {vt.param_start_year} populations of the source areas, {vt.param_start_year} - {vt.param_end_year}")
      
    }
    
  }
  
  if(vt.param_agg_level == "Territorial authority"){
    
    if(!vt.param_perc){
      
      title_str <- glue("Migration to and from {vt.param_ta} from {vt.param_start_year} to {vt.param_end_year}")
      
    }
    
    if(vt.param_perc){
      
      title_str <- glue("Migration to and from {vt.param_ta} from {vt.param_start_year} to {vt.param_end_year} as a percentage of the {vt.param_start_year} populations of the source areas")
      
      
    }
    
  }
  
  return(title_str)
  
})

#Leaflet titles
output$mig_all_map_title <- renderUI({
  
  mig_all_panel$map
  
  isolate({
  vt.param_start_year <- input$mig_all_param_year[1]
  vt.param_end_year <- input$mig_all_param_year[2]
  vt.param_flow <- input$mig_all_param_flow
  vt.param_flow_type <- input$mig_all_param_flow_type
  vt.param_perc <- input$mig_all_param_map_perc
  vt.param_agg_level <- input$mig_all_agg_level
  vt.param_ta <- input$mig_all_param_ta
  })
  
  vt.title <- glue("{vt.param_flow_type} - {vt.param_flow}")
  
  if(vt.param_agg_level == "Total NZ"){
    vt.title <- glue("{vt.title} {ifelse(vt.param_flow %in% vt.mig_all_map_label_test, 'to', 'from')} each territorial authority")
    
    if(vt.param_perc){
      vt.title <- glue("{vt.title} as a percentage of the area's population")
      }
    } else{
    vt.title <- glue("{vt.title} {ifelse(vt.param_flow %in% vt.mig_all_map_label_test, 'to', 'from')} {vt.param_ta} {ifelse(vt.param_flow %in% vt.mig_all_map_label_test, 'from', 'to')} other Territorial Authorities")
  
    if(vt.param_perc){
      vt.title <- glue("{vt.title} as a percentage of the {vt.param_ta} population")
      }
    }
  
  return(h4(vt.title))
  
})