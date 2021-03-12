#' @import rlang


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


