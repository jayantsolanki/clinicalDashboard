#' ae_listing UI Function
#'
#' @description A shiny Module for plotting Adverse Events listing
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param dataset data frame (reactive) with variables necessary for adae listing
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ae_listing_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        shinydashboard::box(
          width = NULL,
          title = "Adverse Event Listing",
          DT::dataTableOutput(
            ns("aeListing"),
            width=400)
        )
      )
    )
  )
}
    
#' ae_listing Server Function
#'
#' @noRd 
mod_ae_listing_server <- function(input, output, session, dataset){
  ns <- session$ns
  output$aeListing = DT::renderDataTable({
    dataset(),
    options = list(scrollX = TRUE)
  })
}
    
## To be copied in the UI
# mod_ae_listing_ui("ae_listing_ui_1")
    
## To be copied in the server
# callModule(mod_ae_listing_server, "ae_listing_ui_1")
 
