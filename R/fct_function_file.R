#' @import rlang


## A filter function for enrollement tab
# df is a dataframe 
# col is column in the dataframe df
# val is value to filter value

# Exammple : 
# df 
#  ColA   ColB
#  "AXS"   "SDF"
#  "SDA"   "SDS"

## filter_function(df, ColA, AXS)
#   ColA   ColB
#  "AXS"   "SDF"


filter_funtion <- function(df, col, val){
  
  if(!is.null(val)){
    if("All" %in% val){
      filteredData <- df
    }
    else{
      filteredData <- df %>% filter({{col}} %in% val)
    }
  }
  else{
    return (NULL)
  }
  
}


