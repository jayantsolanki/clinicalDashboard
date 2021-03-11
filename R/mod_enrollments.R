#' enrollments UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import DT lubridate
mod_enrollments_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      div(plotlyOutput(ns("lchart")), style = "margin-right:10px;margin-left:10px;margin-top:5px")
    )
    
    # ,fluidRow(
    #   shinydashboard::box(
    #     title = "Title 1", width = 4, solidHeader = TRUE, status = "primary",
    #     "Box content"
    #   ),
    #   shinydashboard::box(
    #     title = "Title 2", width = 4, solidHeader = TRUE,
    #     "Box content"
    #   ),
    #   shinydashboard::box(
    #     title = "Title 1", width = 4, solidHeader = TRUE, status = "warning",
    #     "Box content"
    #   )
    # ),
    
    # fluidRow(
    #   shinydashboard::box(
    #     width = 4, background = "black",
    #     "A box with a solid black background"
    #   ),
    #   shinydashboard::box(
    #     title = "Title 5", width = 4, background = "light-blue",
    #     "A box with a solid light-blue background"
    #   ),
    #   shinydashboard::box(
    #     title = "Title 6",width = 4, background = "maroon",
    #     "A box with a solid maroon background"
    #   )
    # )
  )
}
    
#' enrollments Server Function
#'
#' @noRd 
mod_enrollments_server <- function(input, output, session, dataset){
  ns <- session$ns
  
  ay <- list(
    showline = TRUE,
    mirror = "ticks",
    linecolor = "grey",
    linewidth = 0.5
  )
  
  ax <- list(
    showline = TRUE,
    mirror = "ticks",
    linecolor = "grey",
    linewidth = 0.5
  )
  
   output$lchart <- renderPlotly({
     req(dataset())
     # print(dataset() %>% group_by(Month) %>% summarise(Screened_sum=sum(total_screened,na.rm = TRUE)) %>% arrange(match(Month, month.name)))
     final_df <- as.data.frame(dataset()) %>% group_by(Month) %>% summarise(Screened_sum=sum(total_screened,na.rm = TRUE),Enrolled_sum=sum(total_enrolled,na.rm = TRUE)) 
     # print(final_df)
     
     plot_ly(final_df) %>% 
       layout(yaxis = list(title = "Total")) %>%
       add_trace(x = ~Month, y = ~Screened_sum, type='scatter', mode = "lines", yaxis = "y1", line = list(color = 'yellow'),name = 'Screened') %>%
       add_trace(x = ~Month, y = ~Enrolled_sum, type='scatter', mode = "lines", yaxis = "y1", line = list(color = 'red'), name = 'Enrolled') %>% 
       layout(plot_bgcolor='grey', paper_bgcolor='#AAF0D1', yaxis = ay, xaxis = ax,
              title = list(text = "Screened Vs Enrolled Records Across Months", y = 0.98)) %>% 
       layout(legend = list(x = 0.1, y = 0.3))
     
   })
}
    
## To be copied in the UI
# mod_enrollments_ui("enrollments_ui_1")
    
## To be copied in the server
# callModule(mod_enrollments_server, "enrollments_ui_1")
 
