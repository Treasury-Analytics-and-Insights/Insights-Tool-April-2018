# ui script for a combined national/ta migration panel

#Panel ui fn
panel_mig_all <- function(){
  tabPanel("Population explorer",
           value = "Population explorer",
           fluidPage(
             fluidRow(
               column(id = "combined_column",
                      width = 2,
                      mig_all_control()),
               column(width = 10,
                      tags$p(
						strong("Population explorer "),
                        "shows the key drivers of population change in New Zealand, both at a total New Zealand level and for specific territorial authorities.",
						p(),
						"(Hover your mouse over the data visualisations to reveal more information)."
						),
                      mig_all_headlines(),
                      tabBox(
                        width = 12,
                        id="grey_tabBox",
                        tabPanel(
                          "Migration within NZ",
                          mig_all_internal()
                        ),
                        tabPanel(
                          "International migration",
                          mig_all_external()
                        ),
                        tabPanel(
                          "Age profile",
                          mig_all_natural()
                        ),
                        tabPanel(
                          vt.mig_all_map_tab_title,
                          mig_all_map()
                        ),
                        tabPanel(
                          "Background",
						  mig_all_background()
                        )
                      )
               )
             )
           )
  )
}


#Master control
mig_all_control <- function() {
  box(
    title = "User inputs:",
    id = 'grey_box',
    status = 'primary',
    width = 12,
    sliderInput(
      inputId = "mig_all_param_year",
      label = "Year range selection",
      min = vt.init_mig_all_year_min,
      max = vt.init_mig_all_year_max,
      value = vt.init_mig_all_year_range,
      step = 1,
      dragRange = F,
      sep = '',
      ticks = T
    ),
    checkboxInput(
      inputId = "mig_all_param_map_perc",
      label = "Display as a percentage?",
      value = F
    ),
    radioButtons(
      inputId = "mig_all_agg_level",
      label = "Level of analysis",
      choices = vt.init_mig_all_agg_level_select,
      selected = vt.init_mig_all_agg_level
    ),
    selectInput(
      inputId = "mig_all_param_ta",
      label = "Territorial authority",
      choices = vt.init_mig_all_param_ta_select,
      selected = vt.init_mig_all_param_ta
    ),
    actionButton("mig_all_page_modal_help",
                 "About this tool",
                 icon = icon('question-circle',
                             class = 'fa fa-question-circle'))
  )
}

#display the internal migration tab
mig_all_internal <- function(){
  tabPanel(
    title = NULL,
    fluidRow(
      column(
        width = 6,
        align = 'center',
        textOutput("mig_all_chord_title"),
        chorddiagOutput("mig_all_chord"),
        actionButton("mig_all_chord_modal_help",
                     "About this chord diagram",
                     icon = icon('question-circle',
                                 class = 'fa fa-question-circle'))
      ),
      column(
        width = 6,
        highchartOutput("mig_all_domestic_mover_comparison")
      )
    )
  )
}

#display the external migration tab
mig_all_external <- function(){
  tabPanel(
    title = NULL,
    fluidRow(
      column(
        width = 6,
        highchartOutput("mig_all_ext_nz",
                        height = "200px"),
        highchartOutput("mig_all_ext_other",
                        height = "600px")
      ),
      column(
        width = 6,
        highchartOutput("mig_all_international_migrant_demo",
                        height = "800px")
      )
    )
  )
}



#display the natural increase tab
mig_all_natural <- function(){
  tabPanel(
    title = NULL,
    fluidRow(
      column(
        width = 12,
        highchartOutput("mig_all_popn_changes"),
        fluidRow(
          column(
            width = 12,
            align = 'center',
            actionButton("mig_all_pyramid_modal_help",
                         "About this population pyramid",
                         icon = icon('question-circle',
                                     class = 'fa fa-question-circle'))
          )
        )
      )
    )
  )
}

# #Display the national migration map
mig_all_map <- function(){
  # browser()
  tabPanel(
    title = NULL,
    fluidRow(
      column(
        width = 12,
        align = 'center',
        uiOutput(outputId = "mig_all_map_title")
      )
    ),
    fluidRow(
      column(
        width = 12,
        leafletOutput("mig_all_map", height = "800px"),
        absolutePanel(
          top = 10,
          left = 50,
          column(
            width = 8,
            radioButtons(
              inputId = "mig_all_param_flow",
              label = "Direction of population change",
              choices = vt.init_mig_all_param_flow_select,
              selected = vt.init_mig_all_param_flow
            ),
            radioButtons(
              inputId = "mig_all_param_flow_type",
              label = "Type of population change",
              choices = vt.init_mig_all_parm_flow_type_select,
              selected = vt.init_mig_all_parm_flow_type
            ),
            downloadButton(
              outputId = "mig_all_map_download",
              label = "Download data"
            )
          )
        )
      )
    )
  )
}

## Display the background section
mig_all_background <- function() {
  tabPanel(
    "Background",
    value = "comp_mig_all_background",
    div(
      column(
        width = 1
      ),
      column(
        width = 10,
        fluidRow(
          br(),
          tags$p(
			"The ",
            strong("Population explorer "),
			"and ",
            strong("Population directions "),
            "tools provide information about population change in New Zealand. They show how the population of New Zealand and its territorial authority areas have changed since 2008, and allow users to better understand the characteristics and drivers of that change.",
            p(),
            "Changes in the population of an area can be decomposed into the following components:",
            tags$ul(
              tags$li("Migration within NZ - Also called internal migration, this counts the number of people moving between areas of New Zealand. Because internal migration does not change the population at a national level, migration within NZ is only described by Territorial Authorities and not for Total NZ."),
              tags$li("International migration - Also called external migration, this counts the number of people arriving in and departing from New Zealand."),
              tags$li("Natural increase - This represents the difference between the number of people born in New Zealand and the number of people who died.")
	      ),
		    p(),
            "The characteristics of people moving to and from the population of an area can be described by the following characteristics for each component:",
            tags$ul(
              tags$li("Sex"),
              tags$li("Ethnic group - A person can be recorded as having multiple ethnic groups and these are reported individually. As a result percentages add to more than 100. Categories are European, Maori, Pacific Peoples, Asian, MELAA (Middle Eastern, Latin American, and African) and Other."),
              tags$li("Age group"),
              tags$li("Country of origin - Country of origin is taken from the passport used when a person first arrives in New Zealand, but they are considered to have a New Zealand country of origin if they were granted residence more than 10 years ago."),
              tags$li("Visa type - Broad visa types reported are Work, Student, Resident and Other. This is based on the most recent visa approved for a person in the last year over which changes are being examined. The Other category is largely made up of people travelling on an Australian passport or who are in New Zealand on a Visitor visa."),
              tags$li("Territorial authority (TA) - There are 67 territorial authority areas in New Zealand, representing the areas served by the 13 city councils, the 53 district councils and the Chatham Islands Council. The Chatham Islands area is excluded from our analysis due to data quality issues.")
	      ),
		  p(),
  			"The ",
            strong("Population explorer "),
			"tool illustrates the key drivers of population change in New Zealand over a specified time period, both at a total New Zealand level and for specific territorial authorities. It shows high level key facts boxes, and includes maps and graphs that help visualise and describe the components and characteristics of change.",
            p(),
  			"The ",
            strong("Population directions "),
			"tool shows how the population of New Zealand has changed over time since 2008 and what has driven this change. It provides a flexible way for users to look at trends in population change.",
            p(),
            "Data comes from Stats NZ's Integrated Data Infrastructure (IDI). Population change has been identified and described using integrated government datasets, with people's location being estimated from a range of administrative data sources. The quality of the data has been assessed against official statistical sources. Although it has been shown to be reasonably robust at describing population change, results are not official statistics and should be treated with caution.", 
            p(),
            "More information about the data behind these tools can be found in ", 
            a(href = "https://treasury.govt.nz/publications/ap/ap-18-02", "Where we come from, where we go - Describing population change in New Zealand"),
			"."
          )
        )
      )
    )
  )
}


#Display the valueboxes at the top of the panel

mig_all_headlines <- function(){
  fluidRow(
    column(
      width = 12,
      valueBoxOutput("mig_all_valuebox_1",
                     width = 3),
      
      valueBoxOutput("mig_all_valuebox_2",
                     width = 3),
      
      valueBoxOutput("mig_all_valuebox_3",
                     width = 3),
      
      valueBoxOutput("mig_all_valuebox_4",
                     width = 3)
    )
  )
}