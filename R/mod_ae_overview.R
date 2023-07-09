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
      tabBox(
        title = "AE Overview",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "aeOverviewTab", 
        # height = "250px",
        width = 12,
        tabPanel("AE Toxicity Distribution",
          fluidRow(
            box(title = "AE distribution by Toxicity", 
              solidHeader = TRUE, 
              status = "success", 
              width = 12,
              fluidRow(
                column(
                  width = 12,
                  uiOutput(ns('selection')) 
                )
              ),
            br(),
              fluidRow(
                column(width = 12,
                       shinycssloaders::withSpinner(DT::dataTableOutput(ns("aeToxDis")), type = 4, color = "#0dc5c1")
                )
              )
            ),
            br(),
            box(title = "AE Listing for selected row", 
              solidHeader = TRUE, 
              status = "success", 
              width = 12,
              fluidRow(
                column(width = 12,
                       shinycssloaders::withSpinner(DT::dataTableOutput(ns("aeToxDisListing")), type = 4, color = "#0dc5c1")
                )
              )
            )
          )
        ),
        tabPanel( "AE Overall Distribution", 
          fluidRow(
            box(title = "AE Overall Distribution by groups", 
              solidHeader = TRUE, 
              status = "success", 
              width = 12,
              fluidRow(
                column(
                  width = 4,
                  uiOutput(ns('soc_pt_selection')) 
                ),
                column(
                  width = 2,
                  uiOutput(ns('groupSelection')) 
                )
              ),
              br(),
              fluidRow(
                column(width = 12,
                       shinycssloaders::withSpinner(DT::dataTableOutput(ns("aeOverallDis")), type = 4, color = "#0dc5c1")
                )
              )
            ),
            br(),
            box(title = "AE Listing for selected row", 
                solidHeader = TRUE, 
                status = "success", 
                width = 12,
                fluidRow(
                  column(width = 12,
                         shinycssloaders::withSpinner(DT::dataTableOutput(ns("aeOverallDisListing")), type = 4, color = "#0dc5c1")
                  )
                )
            )
          )
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
  

  Final_Table <- reactive({
    req(dataset())
    dataset() %>% 
    select(USUBJID, AEDECOD, AEBODSYS, AETOXGR, AESER, TRTA, SEX, SITEID, AEREL, RELGR1, AGEGR1)  %>% 
    distinct(USUBJID, AEDECOD, AEBODSYS, AETOXGR, AESER, TRTA, SEX, SITEID, AEREL, RELGR1, AGEGR1)  
  })
  # for AE Toxicity Distribution
  
  incidenceRateTable <- reactive({
    req(Final_Table())
    req(input$selectType)
    incidenceRateTable <- as.data.frame(matrix(NA, nrow=0, ncol=1 + 3*length(unique(Final_Table()$TRTA)))) %>% 
      mutate_all(as.integer)
    groupVar <- input$selectType
    # colnames(incidenceRateTable)[1] <- "AEBODSYS"
    colnames(incidenceRateTable)[1] <- groupVar
    # incidenceRateTable$AEBODSYS <- as.character(incidenceRateTable$AEBODSYS)
    incidenceRateTable[[groupVar]] <- as.character(incidenceRateTable[[groupVar]])
    categories <- sort(unique(Final_Table()$TRTA))
    for (arm in categories){
      # had to use  group_by_at(groupVar) instead of groupVar
      anyGrade <- Final_Table() %>% 
        filter(TRTA == arm) %>% 
        group_by_at(groupVar)  %>% 
        summarize(patients=length(unique(USUBJID)))  
      grad3plus <- Final_Table() %>% 
        filter(AETOXGR %in% c(3,4,5), TRTA == arm) %>% 
        group_by_at(groupVar)  %>% 
        summarize(patients=length(unique(USUBJID)))  
      seriousAE <- Final_Table() %>% 
        filter(AESER == 1, TRTA == arm) %>% 
        group_by_at(groupVar)  %>% 
        summarize(patients=length(unique(USUBJID))) 
      tempTable <- left_join(anyGrade, grad3plus, multiple = "all", by = groupVar) %>% 
        left_join(., seriousAE, multiple = "all", by = groupVar) %>% 
        replace(is.na(.), 0)
      if (nrow(incidenceRateTable) == 0){
        incidenceRateTable <- rbind(incidenceRateTable, tempTable)
        
      }
      else{
        incidenceRateTable <- full_join(incidenceRateTable, tempTable, by = groupVar)  %>% 
          replace(is.na(.), 0)
      }
      
    }
    incidenceRateTable
  })

  incidenceRateTable_selectedRow <- eventReactive(input$aeToxDis_rows_selected, {
    # print(input$aeToxDis_rows_selected)
    # print(incidenceRateTable()[c(input$aeToxDis_rows_selected), c(1)])
    # incidenceRateTable()[, c("AEBODSYS")][c(input$aeToxDis_rows_selected)]
    return (incidenceRateTable()[c(input$aeToxDis_rows_selected), c(1)])
  })
  
  incidenceRateOverallTable_selectedRow <- eventReactive(input$aeOverallDis_rows_selected, {
    # print(input$aeToxDis_rows_selected)
    # print(incidenceRateTable()[c(input$aeToxDis_rows_selected), c(1)])
    # incidenceRateTable()[, c("AEBODSYS")][c(input$aeToxDis_rows_selected)]
    return (incidenceRateOverallTable()[c(input$aeOverallDis_rows_selected), c(1)])
  })
  # for AE Overall Distribution
  incidenceRateOverallTable <- reactive({
    req(Final_Table())
    req(input$selectSOCPT)
    req(input$groupSelect)
    
    groupSOCPT <- input$selectSOCPT
    groupOverall <- input$groupSelect
    
    
    incidenceRateTable <- as.data.frame(matrix(NA, nrow=0, ncol=1 + length(unique(Final_Table()[[groupOverall]])))) %>% 
      mutate_all(as.integer)
    

    colnames(incidenceRateTable)[1] <- groupSOCPT
    incidenceRateTable[[groupSOCPT]] <- as.character(incidenceRateTable[[groupSOCPT]])
    categories <- sort(unique(Final_Table()[[groupOverall]]))
    for (item in categories){
      anyGrade <- Final_Table() %>% 
        filter(get(groupOverall) == item) %>% 
        group_by_at(groupSOCPT)  %>% 
        summarize(patients=length(unique(USUBJID)))  
      tempTable <- anyGrade
      if (nrow(incidenceRateTable) == 0){
        incidenceRateTable <- rbind(incidenceRateTable, tempTable)
        
      }
      else{
        incidenceRateTable <- full_join(incidenceRateTable, tempTable, by = groupSOCPT)  %>% 
          replace(is.na(.), 0)
      }
      
    }
    TotalAE <- Final_Table() %>% 
      group_by_at(groupSOCPT)  %>% 
      summarize(patients=length(unique(USUBJID)))
    incidenceRateTable <- full_join(incidenceRateTable, TotalAE, by = groupSOCPT)  %>% 
      replace(is.na(.), 0)
    incidenceRateTable
  })
  
  # SOC/PT selection
  output$selection <- renderUI({
    radioButtons(ns("selectType"), "Category:",
                 c("System Organ Class" = "AEBODSYS",
                   "Preferred Term" = "AEDECOD"))
  })
  output$soc_pt_selection <- renderUI({
    radioButtons(ns("selectSOCPT"), "Category:",
                 c("System Organ Class" = "AEBODSYS",
                   "Preferred Term" = "AEDECOD"))
  })
  
  output$groupSelection <- renderUI({

    choices <- c('TRTA', 'SEX' ,'AESER', 'SITEID', 'AGEGR1', 'AEREL', 'RELGR1' )
    selectInput(
      inputId = ns("groupSelect"),
      label = "Choose Distribution group",
      choices = choices,
      selected = choices[1],
      multiple = F,
      selectize = TRUE
    )

  })

  output$aeToxDis = DT::renderDataTable({
    req(Final_Table())
    req(incidenceRateTable())
    req(input$selectType)
    
    groupVar <- input$selectType
    categories <- sort(unique(Final_Table()$TRTA))
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, groupVar),
          lapply(categories, th, colspan = 3)
        ),
        tr(
          lapply(rep(c('Any Grades', 'Grade 3+', 'SAE'), length(categories)), th)
        )
      )
    ))
    # datatable(incidenceRateTable, container = sketch, rownames = FALSE)
    
    DT::datatable(
      incidenceRateTable(), 
      selection = list(mode = "single"),
      options = list(scrollX = TRUE),
      container = sketch, 
      rownames = FALSE)
  })
  
  output$aeToxDisListing <- DT::renderDataTable({
    req(incidenceRateTable_selectedRow())
    # selectedRow <- dataset() %>% filter(AEBODSYS %in% incidenceRateTable_selectedRow()$AEBODSYS)
    selectedRow <- dataset() %>% filter(get(input$selectType) %in% incidenceRateTable_selectedRow()[[input$selectType]])
    DT::datatable(
      selectedRow, 
      selection = list(mode = "single"),
      options = list(scrollX = TRUE),
      rownames = T)
  })
  
  output$aeOverallDis <- DT::renderDataTable({
    req(incidenceRateOverallTable())
    req(input$selectSOCPT)
    req(input$groupSelect)
    groupSOCPT <- input$selectSOCPT
    groupOverall <- input$groupSelect
    
    categories <- sort(unique(Final_Table()[[groupOverall]]))
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, groupSOCPT),
          lapply(categories, th, colspan = 1),
          th(colspan = 1, 'Total')
        ),
        tr(
          lapply(rep(c("N"), length(categories)+1), th)
        )
      )
    ))
    DT::datatable(
      incidenceRateOverallTable(), 
      selection = list(mode = "single"),
      options = list(
        scrollX = TRUE,
        list(className = 'dt-center', targets = "_all")
      ),
      container = sketch,
      rownames = FALSE)

  })
  
  output$aeOverallDisListing <- DT::renderDataTable({
    req(incidenceRateOverallTable_selectedRow())
    # selectedRow <- dataset() %>% filter(AEBODSYS %in% incidenceRateTable_selectedRow()$AEBODSYS)
    selectedRow <- dataset() %>% filter(get(input$selectSOCPT) %in% incidenceRateOverallTable_selectedRow()[[input$selectSOCPT]])
    DT::datatable(
      selectedRow, 
      selection = list(mode = "single"),
      options = list(scrollX = TRUE),
      rownames = T)
  })
 
}
    
## To be copied in the UI
# mod_ae_overview_ui("ae_overview_ui_1")
    
## To be copied in the server
# callModule(mod_ae_overview_server, "ae_overview_ui_1")
 
