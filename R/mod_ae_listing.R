#' ae_listing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ae_listing_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
      "Box content"
    )
  )
}
    
#' ae_listing Server Function
#'
#' @noRd 
mod_ae_listing_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_ae_listing_ui("ae_listing_ui_1")
    
## To be copied in the server
# callModule(mod_ae_listing_server, "ae_listing_ui_1")
 
