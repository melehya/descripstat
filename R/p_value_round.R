#' p_value_round

#' inputs a pvalue from a statistical test,
#' rounds it to two decimal places if > 0.1,
#' three decmial places if < 0.1
#' prints < 0.001 if p is < 0.001
#' returns rounded p value

#' @param pval A p-value
#' @return A rounded p-value
#' @export

p_value_round <- function(pval)
{
  ifelse((pval<0.1),
         {
           ifelse((pval<0.001),
                  {
                    pval <- "< 0.001"
                  },
                  {
                    pval <- round(pval, digits = 3)
                  })
         },
         {
           pval <- round(pval, digits = 2)
         })
  return(pval)
}
