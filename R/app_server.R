#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # load all the data here  first before continuing
  result <- trial_data()
  # shiny::validate(result)
  if(result==1 || result == 2){
    if(result==2){
      showNotification(
        ui = "Warning encountered during Clinical Data reading process. Please check app log.", 
        type = "warning",
        duration = 10
      )
    }
    # List the first level callModules here
    callModule(mod_enrollments_server, "enrollments_ui_1")
    
    # callModule(mod_adverse_events_server, "adverse_events_ui_1")
    callModule(mod_ae_overview_server, "ae_overview_ui_1")
    callModule(mod_ae_figures_server, "ae_figures_ui_1")
    callModule(mod_ae_listing_server, "ae_listing_ui_1")
    callModule(mod_ae_filters_server, "ae_filters_ui_1")
    
    callModule(mod_vitals_labs_server, "vitals_labs_ui_1")
    callModule(mod_Home_server, "Home_ui_1")
    callModule(mod_MuscleGroup_server, "MuscleGroup_ui_1")
    callModule(mod_Exercises_server, "Exercises_ui_1")
    
  }
  else{ # stop going ahead with application
    showNotification(
      ui = "Error occured during Clinical Data reading. Please check app log.", 
      type = "error",
      duration = NULL,
      action = a(href = "javascript:location.reload();", "Reload page")
    )
  }

  


}
