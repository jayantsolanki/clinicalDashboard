#' vitals_labs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_vitals_labs_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' vitals_labs Server Function
#'
#' @noRd 
mod_vitals_labs_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_vitals_labs_ui("vitals_labs_ui_1")
    
## To be copied in the server
# callModule(mod_vitals_labs_server, "vitals_labs_ui_1")
 
