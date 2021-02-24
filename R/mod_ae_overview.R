#' ae_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinydashboard
mod_ae_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        shinydashboard::box(
          width = NULL,
          title = "Total Weight Per Workout",
          plotOutput(ns("plot2"))
        )
      ),
      column(
        width = 6,
        shinydashboard::box(
          width = NULL,
          title = "Max Weight Per Workout",
          plotOutput(ns("plot3"))
        )
      )
    )
  )
}
    
#' ae_overview Server Function
#'
#' @noRd 
mod_ae_overview_server <- function(input, output, session){
  ns <- session$ns
  output$plot2 <- renderPlot({
    shinipsum::random_ggplot(type = "line")
  })
  
  output$plot3 <- renderPlot({
    shinipsum::random_ggplot(type = "bar")
  })
 
}
    
## To be copied in the UI
# mod_ae_overview_ui("ae_overview_ui_1")
    
## To be copied in the server
# callModule(mod_ae_overview_server, "ae_overview_ui_1")
 
