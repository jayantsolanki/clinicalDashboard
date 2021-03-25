#' enrollments_filters_functions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrollments_filters_functions_ui <- function(id){
  ns <- NS(id)
  tagList(
    title = "Enrollments Filters",
    width = NULL,
    # selectInput(ns("groupby"), "Group By", choices = c("Month","Country","Gender", "Site ID", "Treatment Arm"),multiple = TRUE,selected = c("Month"))
    uiOutput(ns("month"))
    , uiOutput(ns("coun"))
    , uiOutput(ns("gen"))
    , uiOutput(ns("trea"))
    , uiOutput(ns("site"))
    , hr()
    , uiOutput(ns("enrol_apply"))
    , br()
    , uiOutput(ns("enrol_reset"))
    # , actionButton(ns("proceed"),"Proceed")
  )
}
    
#' enrollments_filters_functions Server Function
#'
#' @noRd 
mod_enrollments_filters_functions_server <- function(input, output, session, parent_session){
  ns <- session$ns
 
  raw_data <- session$userData$enrol_groupby
  
  values_to_display <- reactiveValues()
  
  values_to_display$enrol_data <- raw_data
  
  
  #Step 1 : Month Filter based on Raw data
  output$month <- renderUI({
    if(!is.null(values_to_display$enrol_data)){
      choices <- c("All", unique(values_to_display$enrol_data$Month))
    }
    else{
      choices <-  c()
    }
    
    selectInput(
      inputId = ns("month"),
      label = "Choose Month",
      choices = choices,
      selected = "All",
      multiple = TRUE,
      selectize = TRUE
    )
  })
  
  
  
  enrol_country <- reactive({filter_funtion(values_to_display$enrol_data, Month, input$month)})
  output$coun <- renderUI({
    if(!is.null(enrol_country())){
      choices <- c("All", unique(enrol_country()$Country))
    }
    else{
      choices <-  c()
    }
    selectInput(
      inputId = ns("coun"),
      label = "Choose Country",
      choices = choices,
      selected = "All",
      multiple = TRUE,
      selectize = TRUE
    )
  })
  
  enrol_gender <- reactive({filter_funtion(enrol_country(), Country, input$coun)})
  output$gen <- renderUI({
    if(!is.null(enrol_gender())){
      choices <- c("All", unique(enrol_gender()$Gender))
    }
    else{
      choices <-  c()
    }
    selectInput(
      inputId = ns("gen"),
      label = "Choose Gender",
      choices = choices,
      selected = "All",
      multiple = TRUE,
      selectize = TRUE
    )
  })
  
  enrol_treatment <- reactive({filter_funtion(enrol_gender(), Gender, input$gen)})
  output$trea <- renderUI({
    if(!is.null(enrol_treatment())){
      choices <- c("All", unique(enrol_treatment()$`Treatment Arm`))
    }
    else{
      choices <-  c()
    }
    selectInput(
      inputId = ns("trea"),
      label = "Choose Treatment",
      choices = choices,
      selected = "All",
      multiple = TRUE,
      selectize = TRUE
    )
  })
  
  
  enrol_site <- reactive({filter_funtion(enrol_treatment(), `Treatment Arm`, input$trea)})
  output$site <- renderUI({
    if(!is.null(enrol_site())){
      choices <- c("All", unique(enrol_site()$`Site ID`))
    }
    else{
      choices <-  c()
    }
    selectInput(
      inputId = ns("site"),
      label = "Choose Site",
      choices = choices,
      selected = "All",
      multiple = TRUE,
      selectize = TRUE
    )
  })
  
  
  enrol_final <- eventReactive(input$en_apply,{filter_funtion(enrol_site(), `Site ID`, input$site)})
  
  # Step 11 : Apply button
  output$enrol_apply <- renderUI({
    actionButton(
      inputId = ns("en_apply"),
      label = "Apply",
      icon = icon("sync"), 
      width = NULL)
  })
  
  
  #Step 12 : Reset Button
  output$enrol_reset <- renderUI({
    actionButton(
      inputId = ns("en_reset"),
      label = "Clear",
      icon = icon("window-close"), 
      width = NULL)
  })
  
  # reset filters
  observeEvent(input$en_reset,{
    print("Clined in enroll")
    values_to_display$enrol_data <- NULL
    values_to_display$enrol_data <- raw_data
  })
  
  
  return(enrol_final)
}
    
## To be copied in the UI
# mod_enrollments_filters_functions_ui("enrollments_filters_functions_ui_1")
    
## To be copied in the server
# callModule(mod_enrollments_filters_functions_server, "enrollments_filters_functions_ui_1")
 
