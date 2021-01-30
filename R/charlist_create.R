#' charlist_create
#'
#' Removes specified column from dataframe or list, empty if
#' neither dataframe or list
#'
#' @param A column vector
#' @return A character list
#' @export
charlist_create <- function(dataframe_column)
{
  charlist = list()
  for(j in dataframe_column)
  {
    if((is.null(dataframe_column)==TRUE)| j=='')
    {
      next
    }
    ifelse((is_list_element(charlist,j)==FALSE),
           {
             charlist <- append(charlist,j)
           },
           {
             next
           })
  }
  return(charlist)
}
