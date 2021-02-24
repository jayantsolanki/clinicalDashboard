#' adverse_events UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinydashboard 
mod_adverse_events_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      tabBox(
        title = "First tabBox",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        tabPanel("Tab1", "First tab content"),
        tabPanel("Tab2", "Tab content 2")
      ),
      tabBox(
        side = "right", height = "250px",
        selected = "Tab1",
        tabPanel("Tab1", "Tab content 1"),
        tabPanel("Tab2", "Tab content 2"),
        tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
      )
    )
  )
}
    
#' adverse_events Server Function
#'
#' @noRd 
mod_adverse_events_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_adverse_events_ui("adverse_events_ui_1")
    
## To be copied in the server
# callModule(mod_adverse_events_server, "adverse_events_ui_1")
 
