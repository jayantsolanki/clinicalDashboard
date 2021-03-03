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
    , uiOutput(ns("sit"))
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
 
