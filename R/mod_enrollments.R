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
    fluidRow(
      shinydashboard::box(title = "Box title", "Box content"),
      shinydashboard::box(status = "warning", "Box content")
    ),
    
    fluidRow(
      shinydashboard::box(
        title = "Title 1", width = 4, solidHeader = TRUE, status = "primary",
        "Box content"
      ),
      shinydashboard::box(
        title = "Title 2", width = 4, solidHeader = TRUE,
        "Box content"
      ),
      shinydashboard::box(
        title = "Title 1", width = 4, solidHeader = TRUE, status = "warning",
        "Box content"
      )
    ),
    
    fluidRow(
      shinydashboard::box(
        width = 4, background = "black",
        "A box with a solid black background"
      ),
      shinydashboard::box(
        title = "Title 5", width = 4, background = "light-blue",
        "A box with a solid light-blue background"
      ),
      shinydashboard::box(
        title = "Title 6",width = 4, background = "maroon",
        "A box with a solid maroon background"
      )
    )
  )
}
    
#' enrollments Server Function
#'
#' @noRd 
mod_enrollments_server <- function(input, output, session){
  ns <- session$ns

}
    
## To be copied in the UI
# mod_enrollments_ui("enrollments_ui_1")
    
## To be copied in the server
# callModule(mod_enrollments_server, "enrollments_ui_1")
 
