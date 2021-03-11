#' @import readxl
trial_data <- function(
  session = getDefaultReactiveDomain()
){
  tryCatch(
    expr = {
      withProgress(message = 'Reading Clinical Datasets', value = 0, {
        incProgress(0/11, detail = paste("Reading adae", 1))
        session$userData$adae <- read.csv(system.file("extdata", "adae.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(1/11, detail = paste("Reading adef", 2))
        session$userData$adef <- read.csv(system.file("extdata", "adef.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(2/11, detail = paste("Reading adsl", 3))
        session$userData$adsl <- read.csv(system.file("extdata", "adsl.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(3/11, detail = paste("Reading adtte", 4))
        session$userData$adtte <- read.csv(system.file("extdata", "adtte.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(4/11, detail = paste("Reading adverse", 5))
        session$userData$adverse <- read.csv(system.file("extdata", "adverse.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(5/11, detail = paste("Reading demographic", 6))
        session$userData$demographic <- read.csv(system.file("extdata", "demographic.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(6/11, detail = paste("Reading dosing", 7))
        session$userData$dosing <- read.csv(system.file("extdata", "dosing.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(7/11, detail = paste("Reading formatdata", 8))
        session$userData$formatdata <- read.csv(system.file("extdata", "formatdata.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(8/11, detail = paste("Reading labs", 9))
        session$userData$labs <- read.csv(system.file("extdata", "labs.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(9/11, detail = paste("Reading pain", 10))
        
        session$userData$enrol <- read_excel(system.file("extdata", "enrollments.xlsx", package = "protoDash", mustWork = TRUE))
        # session$userData$enrol <- data_frame[order(session$userData$enrol["Enrollment date"]),]
        print(as.data.frame(session$userData$enrol) %>% filter(Country %in% "CAN")) 
        session$userData$enrol <- as.data.frame(session$userData$enrol)
        session$userData$enrol['Screened'] <- ifelse(session$userData$enrol['Screened'] == "Y",1,ifelse(session$userData$enrol['Screened'] == "N", 0 , NA))
        session$userData$enrol['Enrolled'] <- ifelse(session$userData$enrol['Enrolled'] == "Y",1,ifelse(session$userData$enrol['Enrolled'] == "N", 0 , NA))
        # session$userData$enrol['Month'] <- format(session$userData$enrol['Enrollment date'],"%B")
        session$userData$enrol_groupby <- session$userData$enrol %>% mutate(Month = format(floor_date(`Enrollment date`,"month"),"%D")) %>% group_by(Month, Country, Gender, `Treatment Arm`, `Site ID`) %>% 
        summarise(total_screened = sum(Screened),total_enrolled = sum(Enrolled)) 
        print(session$userData$enrol_groupby)
        
        
        # print(session$userData$enrol_groupby %>% arrange(match(Month, month.name)))
        incProgress(10/11, detail = paste("Reading pain", 11))
        
        
        session$userData$pain <- read.csv(system.file("extdata", "pain.csv", package = "protoDash", mustWork = TRUE), stringsAsFactors = F)
        incProgress(11/11, detail = paste("Reading complete"))
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