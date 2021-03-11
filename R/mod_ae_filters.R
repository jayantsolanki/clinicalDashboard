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
      selectInput(ns("groupby"), "Group By", choices = c("Month", "Country", "gender", "Site ID", "Treatment Group"), multiple = TRUE),
      uiOutput(ns("coun")),
      textInput("text", "Text input here:")
  )
}
    
#' ae_filters Server Function
#'
#' @noRd 
mod_ae_filters_server <- function(input, output, session){
  ns <- session$ns
  observe({
    print(input$groupby)
    print(length(input$groupby))
    if(length(input$groupby) > 1){
      output$coun <- renderUI({
        selectInput(ns("country"), "Country", selected = c("2"), choices = c(1,2))
      })
    }
    else{
      output$coun <- renderUI({})
    }
    
  })
  observe({
    print(input$country)
  })
  
 
}
    
## To be copied in the UI
# mod_ae_filters_ui("ae_filters_ui_1")
    
## To be copied in the server
# callModule(mod_ae_filters_server, "ae_filters_ui_1")
 
