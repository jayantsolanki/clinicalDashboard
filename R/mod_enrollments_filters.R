#' enrollments_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrollments_filters_ui <- function(id){
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
    , uiOutput(ns("proceed"))
    # , actionButton(ns("proceed"),"Proceed")
  )
}
    
#' enrollments_filters Server Function
#'
#' @noRd 
mod_enrollments_filters_server <- function(input, output, session){
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
  
  #Step 2 : Based on Month filter, the data should be filtered. This data is taken as raw data to come up with country filter
  enrol_country <- reactive({
    if(!is.null(input$month)){
      if("All" %in% input$month){
        filteredData <- values_to_display$enrol_data
      }
      else{
        filteredData <- values_to_display$enrol_data %>% filter(Month %in% input$month)
      }
    }
    else{
      return (NULL)
    }
  })

  #Step 3 : Country filter based on above data in Step 2
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
  
  #Step 4 : Based on Country filter, the data should be filtered. This data is taken as raw data to come up with Gender filter
  enrol_gender <- reactive({
    if(!is.null(input$coun)){
      if("All" %in% input$coun){
        filteredData <- enrol_country()
      }
      else{
        filteredData <- enrol_country()%>% filter(Country %in% input$coun)
      }
    }
    else{
      return (NULL)
    }
  })
  
  #Step 5 : gender filter based on above data in Step 4
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
  
  #Step 6 : Based on Gender filter, the data should be filtered. This data is taken as raw data to come up with Treatment filter
  enrol_treatment <- reactive({
    if(!is.null(input$coun)){
      if("All" %in% input$gen){
        filteredData <- enrol_gender()
      }
      else{
        filteredData <- enrol_gender()%>% filter(Gender %in% input$gen)
      }
    }
    else{
      return (NULL)
    }
  })
  
  #Step 7 : Treatment filter based on above data in Step 6
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
  
  #Step 8 : Based on Treatment filter, the data should be filtered. This data is taken as raw data to come up with Site ID filter
  enrol_site <- reactive({
    if(!is.null(input$trea)){
      if("All" %in% input$trea){
        filteredData <- enrol_treatment()
      }
      else{
        filteredData <- enrol_treatment()%>% filter(`Treatment Arm` %in% input$trea)
      }
    }
    else{
      return (NULL)
    }
  })
  
  #Step 9 : Site ID filter based on above data in Step 6
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

  #Step 8 : Based on Treatment filter, the data should be filtered. This data is taken as raw data to come up with Site ID filter
  enrol_final <- reactive({
    if(!is.null(input$site)){
      if("All" %in% input$site){
        filteredData <- enrol_site()
      }
      else{
        filteredData <- enrol_site()%>% filter(`Site ID` %in% input$site)
      }
    }
    else{
      return (NULL)
    }
  })
  
  
  
  observe({
    # print(enrol_country())
    # print(enrol_gender())
    # print(enrol_treatment())
    print(enrol_final())
  })
  
  filter_values <- reactiveValues(selected_countries = NULL, selected_t_area = NULL,
                                  selected_site = NULL, selected_gen = NULL)
  
  
  choice_values <- reactiveValues(choice_countries = unique(raw_data$Country),
                                  choice_treatment = unique(raw_data$`Treatment Arm`),
                                  choice_site = unique(raw_data$`Site ID`),
                                  choice_gender = unique(raw_data$Gender))
  
}
    
## To be copied in the UI
# mod_enrollments_filters_ui("enrollments_filters_ui_1")
    
## To be copied in the server
# callModule(mod_enrollments_filters_server, "enrollments_filters_ui_1")
 
