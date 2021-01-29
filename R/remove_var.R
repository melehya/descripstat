#' remove var 
#' remove_var: removes specified variable from list 

#' @param dataframe A column vector 
#' @param var A variable 
#' @return dataframe 
#' @export
remove_var <- function(dataframe, var)
{
  ifelse((class(dataframe)=="data.frame"),
         {
           new.frame=data.frame(matrix(nrow=1,ncol=1))
           newcolnames<- list()
           for (i in colnames(dataframe))
           {
             ifelse((var!=i),
                    {
                      new.frame <- data.frame(new.frame, dataframe[[i]])
                      newcolnames <- append(newcolnames, i) 
                      next
                    },
                    {
                      next
                    })
           }
           new.frame <- new.frame[-1]
           colnames(new.frame) <- newcolnames
         },
         {
           ifelse((class(dataframe)=="list"),
                  {
                    new.frame=list()
                    for (i in dataframe)
                    {
                      ifelse((var!=i),
                             {
                               new.frame = append(i, new.frame)
                               next
                             },
                             {
                               next
                             })
                    }
                  },
                  {
                    new.frame = list()
                  })
         })
  return(new.frame)
}