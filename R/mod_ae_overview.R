#' ae_overview UI Function
#'
#' @description A shiny Module for plotting Summary level information on the Adverse Events
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param dataset data frame (reactive) with variables necessary for adae overview
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinydashboard plotly dplyr ggplot2
mod_ae_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        tabBox(
          title = "AE Overview",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "aeOverviewTab", 
          # height = "250px",
          width = 12,
          tabPanel("AE by SOC",
                   "AE distribution by SOC",
                   plotlyOutput(ns("plot2"))),
          tabPanel("AE by PT",
                   "AE distribution by PT",
                   plotlyOutput(ns("plot3")))
        )
      )
    )
  )
}
    
#' ae_overview Server Function
#'
#' @noRd 
mod_ae_overview_server <- function(input, output, session, dataset){
  ns <- session$ns
  
  # for SOC
  
  output$plot2 <- renderPlotly({
    ggplot(dataset(), aes( x = reorder(AEBODSYS,AEBODSYS,length), fill = TRTA )) + geom_bar() + coord_flip()
  })
  
  output$plot3 <- renderPlotly({
    # shinipsum::random_ggplot(type = "line")
    ggplot(dataset(), aes( x = reorder(AEDECOD,AEDECOD,length), fill = TRTA )) + geom_bar() + coord_flip()
  })
 
}
    
## To be copied in the UI
# mod_ae_overview_ui("ae_overview_ui_1")
    
## To be copied in the server
# callModule(mod_ae_overview_server, "ae_overview_ui_1")
 
