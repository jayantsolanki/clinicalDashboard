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
    withProgress(message = 'Loading Filters', value = 0, {
      incProgress(0/9, detail = paste("Loading Enrollment filters", 1))
      callModule(mod_enrollments_server, "enrollments_ui_1")
      incProgress(1/9, detail = paste("Loading Adverse Event filters", 2))
      adae_datasets <- callModule(mod_ae_filters_server, "ae_filters_ui_1", parent_session = session)
      incProgress(2/9, detail = paste("Loading AE Overview filters", 3))
      callModule(mod_ae_overview_server, "ae_overview_ui_1", adae_datasets)
      incProgress(3/9, detail = paste("Loading AE Figures filters", 4))
      callModule(mod_ae_figures_server, "ae_figures_ui_1")
      incProgress(4/9, detail = paste("Loading AE Listing filters", 5))
      callModule(mod_ae_listing_server, "ae_listing_ui_1", adae_datasets)
      incProgress(5/9, detail = paste("Loading Vitals and Labs filters", 6))
      callModule(mod_vitals_labs_server, "vitals_labs_ui_1")
      incProgress(6/9, detail = paste("Loading Home filters", 7))
      callModule(mod_Home_server, "Home_ui_1")
      incProgress(7/9, detail = paste("Loading Muscle Groups filters", 8))
      callModule(mod_MuscleGroup_server, "MuscleGroup_ui_1")
      incProgress(8/9, detail = paste("Loading Exercises filters", 9))
      callModule(mod_Exercises_server, "Exercises_ui_1")
      incProgress(9/9, detail = paste("Loading complete"))
      Sys.sleep(0.25)
      # incProgress(11/12, detail = paste("Reading adae", 11))
      # session$userData$adae <- read.csv(system.file("extdata", "adae.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
      # incProgress(12/12, detail = paste("Reading adae", 12))
      # session$userData$adae <- read.csv(system.file("extdata", "adae.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
      
    })
    # List the first level callModules here
    # callModule(mod_enrollments_server, "enrollments_ui_1")
    
    # callModule(mod_adverse_events_server, "adverse_events_ui_1")
    # adae_datasets <- callModule(mod_ae_filters_server, "ae_filters_ui_1", parent_session = session)
    # callModule(mod_ae_overview_server, "ae_overview_ui_1", adae_datasets)
    # callModule(mod_ae_figures_server, "ae_figures_ui_1")
    # callModule(mod_ae_listing_server, "ae_listing_ui_1", adae_datasets)
    # 
    # 
    # callModule(mod_vitals_labs_server, "vitals_labs_ui_1")
    # callModule(mod_Home_server, "Home_ui_1")
    # callModule(mod_MuscleGroup_server, "MuscleGroup_ui_1")
    # callModule(mod_Exercises_server, "Exercises_ui_1")
    # 
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
