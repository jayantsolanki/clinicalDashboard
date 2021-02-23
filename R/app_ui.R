#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  mod_Home_ui("Home_ui_1")
  mod_Exercises_ui("Exercises_ui_1")
  mod_MuscleGroup_ui("MuscleGroup_ui_1")
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    shinydashboardPlus::dashboardPagePlus(
      skin = "green", #“blue”, “blue-light”, “black”, “black-light”, “purple”, “purple-light”, “green”, “green-light”, “red”, “red-light”, “yellow”, “yellow-light”, “midnight”
      header = shinydashboardPlus::dashboardHeaderPlus(
        title = "Clinical Data Monitoring Dashboard",
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "gears"
      ),
      # Create our navigation menu that links to each of the tabs we defined
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          # Setting id makes input$tabs give the tabName of currently-selected tab
          id = "tabs",
          shinydashboard::menuItem("Enrollment", icon = icon("chart-pie"), tabName = "enrollments"),
          shinydashboard::menuItem("Adverse Events", icon = icon("notes-medical"), tabName = "adverseEvents"),
          shinydashboard::menuItem("Vitals and Labs", icon = icon("heartbeat"), tabName = "vitalsLabs"),
          shinydashboard::menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          shinydashboard::menuItem("Muscle Group View", icon = icon("th"), tabName = "mg"),
          shinydashboard::menuItem("Exercise View", icon = icon("bar-chart-o"), tabName = "ev"
          ))
      ),
      # Show the appropriate tab's content in the main body of our dashboard when we select it
      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem("enrollments", mod_enrollments_ui("enrollments_ui_1")),
          shinydashboard::tabItem("adverseEvents", mod_adverse_events_ui("adverse_events_ui_1")),
          shinydashboard::tabItem("vitalsLabs", mod_vitals_labs_ui("vitals_labs_ui_1")),
          shinydashboard::tabItem("dashboard", mod_Home_ui("Home_ui_1")),
          shinydashboard::tabItem("mg", mod_MuscleGroup_ui("MuscleGroup_ui_1")),
          shinydashboard::tabItem("ev", mod_Exercises_ui("Exercises_ui_1")
          )
        ) 
      ),
      rightsidebar = shinydashboardPlus::rightSidebar(
        # shinydashboardPlus::rightSidebarTabContent(
        #   id = 1,
        #   title = "Tab 1",
        #   icon = "desktop",
        #   active = TRUE,
        #   sliderInput(
        #     "obs",
        #     "Number of observations:",
        #     min = 0, max = 1000, value = 500
        #   )
        # ),
        # shinydashboardPlus::rightSidebarTabContent(
        #   id = 2,
        #   title = "Tab 2",
        #   textInput("caption", "Caption", "Data Summary")
        # ),
        # shinydashboardPlus::rightSidebarTabContent(
        #   id = 3,
        #   icon = "paint-brush",
        #   title = "Tab 3",
        #   numericInput("obs", "Observations:", 10, min = 1, max = 100)
        # )
      ),
      title = "Clinical Data Monitoring Dashboard"
    )

  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'protoDash'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

