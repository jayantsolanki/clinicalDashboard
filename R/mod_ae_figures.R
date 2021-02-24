#' ae_figures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ae_figures_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ae_figures Server Function
#'
#' @noRd 
mod_ae_figures_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_ae_figures_ui("ae_figures_ui_1")
    
## To be copied in the server
# callModule(mod_ae_figures_server, "ae_figures_ui_1")
 
