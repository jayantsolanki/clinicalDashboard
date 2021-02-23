#' enrollments UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrollments_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 10,
          DT::dataTableOutput(ns("dt"))
        )
      )
    )
  )
}
    
#' enrollments Server Function
#'
#' @noRd 
mod_enrollments_server <- function(input, output, session){
  ns <- session$ns
  print(nrow(session$userData$adae))
  output$dt <- DT::renderDataTable({
    DT::datatable(session$userData$adae, filter = "top")
  })
}
    
## To be copied in the UI
# mod_enrollments_ui("enrollments_ui_1")
    
## To be copied in the server
# callModule(mod_enrollments_server, "enrollments_ui_1")
 
