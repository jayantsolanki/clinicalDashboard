#' ae_overview UI Function
#'
#' @description A shiny Module for plotting Summary level information on the Adverse Events
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param dataset data frame (reactive) with variables necessary for adae overview
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinydashboard plotly dplyr ggplot2
mod_ae_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        tabBox(
          title = "AE Overview",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "aeOverviewTab", 
          # height = "250px",
          width = 12,
          tabPanel("AE Toxicity Distribution",
                   "AE distribution by Toxicity",
                   dataTableOutput(ns("aeToxDis"))),
          tabPanel("AE Overall distribution",
                   "AE distribution by groups",
                   plotlyOutput(ns("plot3")))
        )
      )
    )
  )
}
    
#' ae_overview Server Function
#'
#' @noRd 
mod_ae_overview_server <- function(input, output, session, dataset){
  ns <- session$ns
  
  # for AE Toxicity Distribution
  incidenceRateTable <- reactive({
    req(dataset())
    
    Final_Table <- dataset() %>% 
        select(USUBJID, AEBODSYS, AETOXGR, AESER, TRTA)  %>% 
        distinct(USUBJID, AEBODSYS, AETOXGR, AESER, TRTA)  
    incidenceRateTable <- as.data.frame(matrix(NA, nrow=0, ncol=1 + 3*length(unique(Final_Table$TRTA)))) %>% 
      mutate_all(as.integer)
    colnames(incidenceRateTable)[1] <- "AEBODSYS"
    incidenceRateTable$AEBODSYS <- as.character(incidenceRateTable$AEBODSYS)
    
    for (arm in unique(Final_Table$TRTA)){
      anyGrade <- Final_Table %>% 
        filter(TRTA == arm) %>% 
        group_by(AEBODSYS)  %>% 
        summarize(patients=length(unique(USUBJID)))  
      grad3plus <- Final_Table %>% 
        filter(AETOXGR %in% c(3,4,5), TRTA == arm) %>% 
        group_by(AEBODSYS)  %>% 
        summarize(patients=length(unique(USUBJID)))  
      seriousAE <- Final_Table %>% 
        filter(AESER == 1, TRTA == arm) %>% 
        group_by(AEBODSYS)  %>% 
        summarize(patients=length(unique(USUBJID))) 
      tempTable <- left_join(anyGrade, grad3plus, all =T, by = "AEBODSYS") %>% 
        left_join(., seriousAE, all =T, by = "AEBODSYS") %>% 
        replace(is.na(.), 0)
      print(tempTable)
      if (nrow(incidenceRateTable) == 0){
        incidenceRateTable <- rbind(incidenceRateTable, tempTable)
        
      }
      else{
        incidenceRateTable <- full_join(incidenceRateTable, tempTable, by = "AEBODSYS")  %>% 
          replace(is.na(.), 0)
      }
      
    }
  })

  
  output$aeToxDis = DT::renderDataTable({
    # req(dataset())
    req(incidenceRateTable())
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'System Organ Class'),
          lapply(unique(dataset()$TRTA), th, colspan = 3)
        ),
        tr(
          lapply(rep(c('Any Grades', 'Grade 3+', 'SAE'), length(unique(dataset()$TRTA))), th)
        )
      )
    ))
    # datatable(incidenceRateTable, container = sketch, rownames = FALSE)
    
    DT::datatable(
      incidenceRateTable(), 
      selection = 'single',
      options = list(scrollX = TRUE),
      container = sketch, 
      rownames = FALSE)
  })
  # 
  # output$plot2 <- renderPlotly({
  #   req(dataset())
  #   ggplot(dataset(), aes( x = reorder(AEBODSYS,AEBODSYS,length), fill = TRTA )) + geom_bar() + coord_flip()
  # })
  
  output$plot3 <- renderPlotly({
    req(dataset())
    # shinipsum::random_ggplot(type = "line")
    ggplot(dataset(), aes( x = reorder(AEDECOD,AEDECOD,length), fill = TRTA )) + geom_bar() + coord_flip()
  })
 
}
    
## To be copied in the UI
# mod_ae_overview_ui("ae_overview_ui_1")
    
## To be copied in the server
# callModule(mod_ae_overview_server, "ae_overview_ui_1")
 
