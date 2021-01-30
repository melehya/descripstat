#' is_list_element
#' Determines if a variable is present in a particular list
#'
#' @param list A list
#' @param var A variable being searched for
#' @return boolean
#' @export
is_list_element <- function(list, var)
{
  i = 1
  test=FALSE
  for (j in list)
  {
    ifelse((var==j),
           {
             test = TRUE
             break
           },
           {
             next
           })
  }
  ifelse((test==TRUE),
         {
           return(TRUE)
         },
         {
           return(FALSE)
         })
}
