#' is_identical
#' determines if all values in column are identical

#' @param dataframe_column A column vector
#' @return boolean
#' @export
is_identical <- function(dataframe_column)
{
  dataframe_column <- na.omit(dataframe_column)
  for(i in dataframe_column)
  {
    ifelse((dataframe_column[[1]]==i),
           {
             identical = TRUE
             next
           },
           {
             identical = FALSE
           })
  }
  return(identical)
}
