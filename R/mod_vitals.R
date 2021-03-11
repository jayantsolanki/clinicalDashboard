#' vitals UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_vitals_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' vitals Server Function
#'
#' @noRd 
mod_vitals_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_vitals_ui("vitals_ui_1")
    
## To be copied in the server
# callModule(mod_vitals_server, "vitals_ui_1")
 
