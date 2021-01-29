#' is_normal 
#'
#' is_normal takes a continuous variable from a dataframe 
#' and determines if the values are normally distributed 
#' outputs TRUE if they are, FALSE if not 

#' @param column vector 
#' @return boolean 
#' @export
is_normal <- function(dataframe_column)
{
  ifelse((length(na.omit(dataframe_column))>5)
         &&(is_identical(dataframe_column)==FALSE),
         {
           myp <- shapiro.test(dataframe_column)
           myp <- myp$p.value
           ifelse(myp<0.05,myp<-FALSE,myp<-TRUE)
           return(myp)       
         },
         {
           return(FALSE)
         })
}