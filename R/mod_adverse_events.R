#' adverse_events UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_adverse_events_ui <- function(id){
  ns <- NS(id)
  tagList(
 
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
 
