#' ae_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ae_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
      title = "AE Filters",
      width = NULL,
      sliderInput("slider", "Slider input:", 1, 100, 50),
      textInput("text", "Text input here:")
  )
}
    
#' ae_filters Server Function
#'
#' @noRd 
mod_ae_filters_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_ae_filters_ui("ae_filters_ui_1")
    
## To be copied in the server
# callModule(mod_ae_filters_server, "ae_filters_ui_1")
 
