#' ae_filters UI Function
#'
#' @description A shiny Module for implementing right sidebar filters for Adverse Events
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @return list with following components
#' \describe{
#'   \item{adae_final}{reactive dataset filter from adae after applying all the filters}
#' }
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr
mod_ae_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
      tags$head(
        tags$style(HTML(paste0("#", ns("apply"),'{display:inline-block}'))),
        tags$style(HTML(paste0("#", ns("reset"),'{display:inline-block}')))
      ),
      strong("AE Filters"),
      width = NULL,
      uiOutput(ns("siteId")),
      uiOutput(ns("trtarm")),
      uiOutput(ns("aebodsys")),
      uiOutput(ns("aedecod")),
      uiOutput(ns("gender")),
      uiOutput(ns("usubjid")),
      hr(),
      uiOutput(ns("apply")),
      uiOutput(ns("reset"))
  )
}
    
#' ae_filters Server Function
#'
#' @noRd 
mod_ae_filters_server <- function(input, output, session, parent_session){
  ns <- session$ns
  adae <- session$userData$adae
  
  # generating intermediate data for filters
  # ae_data <- reactiveValues(adae = adae)
  ae_data <- reactiveValues()
  ae_data$adae <- adae
  adae_trtarm <- reactive({
    if(!is.null(input$siteId)){
      if("All" %in% input$siteId){
        filteredData <- ae_data$adae
      }
      else{
        filteredData <- ae_data$adae %>% filter(SITEID %in% input$siteId)
      }
      
    }
    else{
      return (NULL)
    }
  })
  
  adae_aebodsys <- reactive({
    if(!is.null(input$trtarm)){
      if("All" %in% input$trtarm){
        filteredData <- adae_trtarm()
      }
      else
      {
        filteredData <- adae_trtarm() %>% filter(TRTA %in% input$trtarm)
      }
    }
    else{
      return (NULL)
    }
  })
  
  adae_aedecod <- reactive({
    if(!is.null(input$aebodsys)){
      if("All" %in% input$aebodsys){
        filteredData <- adae_aebodsys()
      }
      else
      {
        filteredData <- adae_aebodsys() %>% filter(AEBODSYS %in% input$aebodsys)
      }
      
    }
    else{
      return (NULL)
    }
  })
  
  adae_gender <- reactive({
    if(!is.null(input$aedecod)){
      if("All" %in% input$aedecod){
        filteredData <- adae_aedecod()
      }
      else
      {
        filteredData <- adae_aedecod() %>% filter(AEDECOD %in% input$aedecod)
      }
      
    }
    else{
      return (NULL)
    }
  })
  
  adae_usubjid <- reactive({
    if(!is.null(input$gender)){
      if("All" %in% input$gender){
        filteredData <- adae_gender()
      }
      else
      {
        filteredData <- adae_gender() %>% filter(SEX %in% input$gender)
      }
    }
    else{
      return (NULL)
    }
  })
  
  # apply filters
  adae_final <- eventReactive(
    c(
      input$apply,
      parent_session$input$tabs
    ), {
    if(!is.null(input$usubjid)){
      if("All" %in% input$usubjid){
        filteredData <- adae_usubjid()
      }
      else
      {
        filteredData <- adae_usubjid() %>% filter(USUBJID %in% input$usubjid)
      }
      return (filteredData)
    }
    else{
      return (NULL)
    }
  })


  observeEvent(parent_session$input$tabs,{
    # if(parent_session$input$tabs == "ae_overview" | parent_session$input.tabs == "ae_figures" | parent_session$input.tabs == "ae_listings"){
    #   print("Running code for Adverse Events")
    #   # click(ns("apply"))
    # }
    if(parent_session$input$tabs == "ae_overview" | parent_session$input$tabs == "ae_figures" | parent_session$input$tabs == "ae_listings"){
      print(parent_session$input$tabs)
      shinyjs::click("apply")
      # alert("Hola it worked")
      # toggle("hello")
      # click(session$ns("reset"))
      # shinyjs::toggle("ae_filters_ui_1-apply")
      # click("ae_filters_ui_1-apply")
    }
    
  })
  # reset filters
  observeEvent(input$reset,{
    print("reset clicked")
    ae_data$adae <- NULL
    ae_data$adae <- adae

  })

  # rendering filter values
  output$siteId <- renderUI({
    if(!is.null(ae_data$adae)){
      choices <- c("All", unique(ae_data$adae$SITEID))
    }
    else{
      choices <-  c()
    }
    selectInput(
      inputId = ns("siteId"),
      label = "Site ID",
      choices = choices,
      selected = "All",
      multiple = TRUE,
      selectize = TRUE
    )
  })
  output$trtarm <- renderUI({
    if(!is.null(adae_trtarm())){
      choices <- c("All", unique(adae_trtarm()$TRTA))
    }
    else{
      choices <-  c()
    }
    selectInput(
      inputId = ns("trtarm"),
      label = "Treatment Arm",
      choices = choices,
      selected = "All",
      multiple = TRUE,
      selectize = TRUE
    )
    
  })
  output$aebodsys <- renderUI({
    if(!is.null(adae_aebodsys())){
      choices <-  c("All", unique(adae_aebodsys()$AEBODSYS))
    }
    else{
      choices <-  c()
    }
    selectInput(
      inputId = ns("aebodsys"),
      label = "System Organ Class",
      choices = choices,
      selected = "All",
      multiple = TRUE,
      selectize = TRUE
    )
  })
  output$aedecod <- renderUI({
    if(!is.null(adae_aedecod())){
      choices <-  c("All", unique(adae_aedecod()$AEDECOD))
    }
    else{
      choices <-  c()
    }
    selectInput(
      inputId = ns("aedecod"),
      label = "Preferred Terms",
      choices = choices,
      selected = "All",
      multiple = TRUE,
      selectize = TRUE
    )
  })
  output$gender <- renderUI({
    if(!is.null(adae_gender())){
      choices <-  c("All", unique(adae_gender()$SEX))
    }
    else{
      choices <-  c()
    }
    selectInput(
      inputId = ns("gender"),
      label = "Gender",
      choices = choices,
      selected = "All",
      multiple = TRUE,
      selectize = TRUE
    )
  })
  output$usubjid <- renderUI({
    if(!is.null(adae_usubjid())){
      choices <-  c("All", unique(adae_usubjid()$USUBJID))
    }
    else{
      choices <-  c()
    }
    selectInput(
      inputId = ns("usubjid"),
      label = "Subject ID",
      choices = choices,
      selected = "All",
      multiple = TRUE,
      selectize = TRUE
    )
  })
  
  output$apply <- renderUI({
    actionButton(
      inputId = ns("apply"),
      label = "Apply",
      icon = icon("sync"),
      class = "btn-success",
      width = NULL)
  })
  
  output$reset <- renderUI({
    actionButton(
      inputId = ns("reset"),
      label = "Clear",
      icon = icon("window-close"),
      class = "btn-info",
      width = NULL)
  })
  
  #returning final datasets
  return(adae_final)
}
    
## To be copied in the UI
# mod_ae_filters_ui("ae_filters_ui_1")
    
## To be copied in the server
# callModule(mod_ae_filters_server, "ae_filters_ui_1")
 
