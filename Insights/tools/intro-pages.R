panel_intro <- function() {
  tabPanel(
    "Home",
    icon = icon("home"),
    fluidPage(
      column(
        width = 6,
        h1("Welcome to Insights"),
        column(
          width = 10,
          h2("Informing policies and services through better information.")
        ),
        column(
          width = 11,
          h3(strong("Insights")," provides information drawn from a range of public sector agencies and presents it in an easy to use interactive format which includes data visualisation and mapping tools.")
        ),
        column(
          width = 12,
          tags$p(tags$span("This work is part of the Treasury's commitment to higher living standards
                           and a more prosperous, inclusive New Zealand. Insights enables the
                           analysis and understanding needed to improve social and economic
                           outcomes for all New Zealanders. All information is anonymous - no
                           individuals are identified through this analysis. While care has been taken
						   to ensure the statistics are reliable, they are not official statistics, and
						   should be treated with caution."))
        ),
        column(
          width = 12,
          tags$p(tags$span("For more detailed information about Insights and the data behind it see ",
						   tags$a(
                             href = "https://treasury.govt.nz/publications/ap/ap-18-02",
                             "Where we come from, where we go - Describing population change in New Zealand"),
							 " and ",
							 tags$a(
                             href = "https://treasury.govt.nz/publications/ap/ap-17-02",
                             "Insights - informing policies and services for at-risk children and youth"),
                           "."))
        )
      ),
      column(
        width = 6,
        id = "aoa_links",
        h1("New areas of analysis"),
        h2("Migration and population change"),
        tags$a(
          href = "#",
          h3(
            "Population explorer",
            style = "background-color: #A49E9D; padding: 20px; width: 65%;"
          )
        ),
        tags$a(
          href = "#",
          h3(
            "Population directions",
            style = "background-color: #EF966C; padding: 20px; width: 65%;"
          )
        )
      )
    )
  )
}