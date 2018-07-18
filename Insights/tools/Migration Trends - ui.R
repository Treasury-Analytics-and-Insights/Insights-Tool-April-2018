panel_mig_trend <- function(){
  tabPanel("Population directions",
           value = "Population directions",
           fluidPage(
             fluidRow(
               column(
                 width = 2,
                 mig_trend_control()
               ),
               column(
                 width = 10,
                 tags$p(
				   strong("Population directions "),
                   "shows how the population of New Zealand has changed over time and what has driven this change.",
					p(),
					"(Hover your mouse over the data visualisations to reveal more information. For further information about the tool see the Background section of the Population explorer page.)"
                 ),
                 tabBox(
                   width = 12,
                   id = 'salmon_tabBox',
                   tabPanel(
                     title = vt.mig_trend_panel_1,
                     mig_trend_population()
                   ),
                   tabPanel(
                     title = vt.mig_trend_panel_2,
                     mig_trend_location()
                   ),
                   tabPanel(
                     title = vt.mig_trend_panel_3,
                     mig_trend_source_target()
                   )
                 )
               )
             )
           )
  )
}

mig_trend_control <- function(){
  box(
    title = "User inputs:",
    width = 12,
    id = 'salmon_box',
    status = 'danger',
    conditionalPanel(
      condition = glue("input.salmon_tabBox == '{vt.mig_trend_panel_2}' || input.salmon_tabBox == '{vt.mig_trend_panel_3}'"),
      radioButtons(
        inputId = "mig_trend_param_flow",
        label = "Direction of population change",
        choices = vt.init_mig_trend_param_flow_select,
        selected = vt.init_mig_trend_param_flow
      )
    ),
    conditionalPanel(
      condition = glue("input.salmon_tabBox == '{vt.mig_trend_panel_2}'"),
      checkboxGroupInput(
        inputId = "mig_trend_param_demo_flow_type",
        label = "Type of population change",
        choices = vt.init_mig_trend_param_flow_type_select,
        selected = vt.init_mig_trend_param_flow_type
      )
    ),
    conditionalPanel(
      condition = glue("input.salmon_tabBox == '{vt.mig_trend_panel_3}'"),
      radioButtons(
        inputId = "mig_trend_param_st_flow_type",
        label = "Type of population change",
        choices = vt.init_mig_trend_param_flow_type_st_select,
        selected = vt.init_mig_trend_param_flow_type_st
      )
    ),
    radioButtons(
      inputId = "mig_trend_param_chart_type",
      label = "Chart type",
      choices = vt.init_mig_trend_param_chart_type_select,
      selected = vt.init_mig_trend_param_chart_type
    ),
    conditionalPanel(
      condition = glue("input.salmon_tabBox == '{vt.mig_trend_panel_2}' || input.salmon_tabBox == '{vt.mig_trend_panel_3}'"),
      checkboxInput(
        inputId = "mig_trend_param_perc",
        label = "Display as a percentage of the previous year's population?",
        value = F
      )
    ),
    conditionalPanel(
      condition = glue("input.salmon_tabBox == '{vt.mig_trend_panel_1}'"),
      checkboxInput(
        inputId = "mig_trend_param_popn_perc",
        label = "Display as a percentage of the New Zealand population?",
        value = F
      )
    ),
    downloadButton("mig_trend_data_download",
                   label = p("Download current data", width = "80%"),
                   width = "100%"),
    br(),
    br(),
    actionButton("mig_trends_page_modal_help",
                 "About this tool",
                 icon = icon('question-circle',
                             class = 'fa fa-question-circle'))
  )
}

mig_trend_location <- function(){
  tabPanel(
    title = NULL,
    fluidRow(
      column(
        width = 12,
        fluidRow(
          column(width = 4,
                 selectizeInput(
                   inputId = "mig_trend_param_location",
                   label = "Location(s) of interest",
                   choices = vt.init_mig_trend_param_source_select,
                   selected = "Total NZ",
                   multiple = T,
                   options = NULL
                 )
          ),
          column(
            width = 4
          ),
          column(width = 4,
                 selectizeInput(
                   inputId = "mig_trend_param_demo",
                   label = vt.mig_trend_param_demo_label,
                   choices = vt.init_mig_trend_param_demo_select,
                   selected = vt.init_mig_trend_param_demo,
                   multiple = T,
                   options = NULL
                 )
          )
        ),
        highchartOutput("mig_trend_location",
                        height = '500px')
      )
    )
  )
}

mig_trend_source_target <- function(){
  tabPanel(
    title = NULL,
    fluidRow(
      column(
        width = 12,
        fluidRow(
          column(width = 4,
                 selectizeInput(
                   inputId = "mig_trend_param_source",
                   label = vt.mig_trend_source_loc_name,
                   choices = vt.init_mig_trend_param_source_select,
                   selected = vt.init_mig_trend_param_source,
                   multiple = T,
                   options = NULL
                 )
          ),
          column(
            width = 4,
            align = 'center',
            style = "margin-top: 25px;",
            conditionalPanel(condition = "input.mig_trend_param_st_flow_type == 'Migration within NZ'",
                             actionButton(inputId = 'mig_trend_action_reversal',
                                          label = 'Swap locations',
                                          icon = icon('fa-arrows-h', class = "fa fa-arrows-h")))
          ),
          column(width = 4,
                 selectizeInput(
                   inputId = "mig_trend_param_target",
                   label = vt.mig_trend_target_loc_name,
                   choices = vt.init_mig_trend_param_source_select,
                   selected = vt.init_mig_trend_param_target,
                   multiple = T,
                   options = NULL
                 )
          )
        ),
        highchartOutput("mig_trend_source_target",
                        height = '500px')
      )
    )
  )
}

mig_trend_population <- function(){
  tabPanel(
    title = NULL,
    fluidRow(
      column(
        width = 12,
        fluidRow(
          column(width = 4,
                 selectizeInput(
                   inputId = "mig_trend_param_popn_location",
                   label = "Location(s) of interest",
                   choices = vt.init_mig_trend_param_source_select,
                   selected = "Total NZ",
                   multiple = T,
                   options = NULL
                 )
          ),
          column(
            width = 4
          ),
          column(width = 4,
                 selectizeInput(
                   inputId = "mig_trend_param_popn_demo",
                   label = vt.mig_trend_param_demo_label,
                   choices = vt.init_mig_trend_param_popn_demo_select,
                   selected = vt.init_mig_trend_param_popn_demo,
                   multiple = T,
                   options = NULL
                 )
          )
        ),
        highchartOutput("mig_trend_population",
                        height = '500px')
      )
    )
  )
}