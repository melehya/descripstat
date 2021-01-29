#' datedetect
#' determines if column contains dates. These will be removed 

#' @param column vector 
#' @return boolean
#' @export
datedetect <- function(dataframe_column)
{
  ifelse((is.na(as.Date(as.character(dataframe_column),format="%m/%d/%Y"))==FALSE)|(is.na(as.POSIXct(as.character(dataframe_column),format="%m/%d/%Y %H:%M"))==FALSE)
         |(is.na(as.Date(as.character(dataframe_column),format="%B %d, %Y"))==FALSE),
         {
           return(TRUE)
         },
         {
           return(FALSE)
         })
}