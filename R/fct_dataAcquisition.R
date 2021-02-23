trial_data <- function(
  session = getDefaultReactiveDomain()
){
  tryCatch(
    expr = {
      withProgress(message = 'Reading Clinical Datasets', value = 0, {
        incProgress(0/10, detail = paste("Reading adae", 1))
        session$userData$adae <- read.csv(system.file("extdata", "adae.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(1/10, detail = paste("Reading adef", 2))
        session$userData$adef <- read.csv(system.file("extdata", "adef.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(2/10, detail = paste("Reading adsl", 3))
        session$userData$adsl <- read.csv(system.file("extdata", "adsl.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(3/10, detail = paste("Reading adtte", 4))
        session$userData$adtte <- read.csv(system.file("extdata", "adtte.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(4/10, detail = paste("Reading adverse", 5))
        session$userData$adverse <- read.csv(system.file("extdata", "adverse.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(5/10, detail = paste("Reading demographic", 6))
        session$userData$demographic <- read.csv(system.file("extdata", "demographic.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(6/10, detail = paste("Reading dosing", 7))
        session$userData$dosing <- read.csv(system.file("extdata", "dosing.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(7/10, detail = paste("Reading formatdata", 8))
        session$userData$formatdata <- read.csv(system.file("extdata", "formatdata.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(8/10, detail = paste("Reading labs", 9))
        session$userData$labs <- read.csv(system.file("extdata", "labs.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(9/10, detail = paste("Reading pain", 10))
        session$userData$pain <- read.csv(system.file("extdata", "pain.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(10/10, detail = paste("Reading complete"))
        Sys.sleep(0.25)
        # incProgress(11/12, detail = paste("Reading adae", 11))
        # session$userData$adae <- read.csv(system.file("extdata", "adae.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        # incProgress(12/12, detail = paste("Reading adae", 12))
        # session$userData$adae <- read.csv(system.file("extdata", "adae.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        
      })
      return(1)
    },
    error = function(error_message){
      print("Error occured during Clinical Data reading.")
      print(error_message)
      return(0)
    },
    warning = function(warning_message){
      print("Warning encountered during Clinical Data reading.")
      print(warning_message)
      return(2)
    }
  )
  
}