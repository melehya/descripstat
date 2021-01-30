#' list_from_user
#' allows user to input names for the column names
#' must ensure that there is no text including commented text beneath the final function
#' call or will not work
#' input is the number of groups being analyzed

#' @param number of groups
#' @return A list of desired column names
#' @export
list_from_user <- function(number_of_groups)
{
  t=0
  temp_list <- list()
  while(t<number_of_groups)
  {
    name <- readline(prompt="enter column name: ")
    temp_list <- append(temp_list,name)
    t=t+1
  }
  return(temp_list)
}
