
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(
  
  bootstrapPage(
    title = "Insights",
    thead = tagList(
      tags$head(
        includeCSS("www/css/treasury-fronts.css"),
        includeCSS("www/css/cost-header.css"),
        includeCSS("www/css/AdminLTE.css"),
        includeCSS("www/css/shinydashboard.css"),
        includeCSS("www/css/custom.css"),
        #Customise value Boxes: background colour only has an allowed range in shinydashboard
        tags$style(HTML(glue(".small-box {'{'}height: 250px;
                             background-color: {df.treasury_color_refresh %>%
                             filter(colour == 'Lt Grey') %>%
                             .[['code']]} !important;{'}'}"))),
        tags$html(lang = 'en'),
        tags$title("Insights - New Zealand Treasury")
      ),
      div(class = "container-fluid", treasury_header())
    ),
    div(
      class = "container-fluid",
      navbarPage(
        title = NULL,
        useShinyjs(),
        selected = "Home",
        panel_intro(),
        panel_mig_all(),
        panel_mig_trend(),
        panel_cyar(),
        panel_tyo(),
        panel_sfy(),
        treasury_about(),
        treasury_contact(),
        treasury_privacy()
      )
    ),
    tags$html(HTML("<!-- Google Analytics -->
                <script>
                  window.ga=window.ga||function(){(ga.q=ga.q||[]).push(arguments)};ga.l=+new Date;
                  ga('create', 'UA-48728248-6', 'auto');
                  ga('send', 'pageview');
                </script>
                <script async src='https://www.google-analytics.com/analytics.js'></script>
              <!-- End Google Analytics -->")),
    tags$script(src = "js/intropage_clickable.js"),
    treasury_footer()
  )
)