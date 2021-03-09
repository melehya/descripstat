#' ConvertChar
#' Converts character to numeric string, can handle ordinal varialbes
#' will convert them to independent binary response variables, which is
#' appropriate for descriptive analysis, but for other analyses variables
#' should be kept in their ordinal form. Also determines if any columns contain
#' dates and removes them. Otherwise would turn each individual date into a separate
#' categorical variables
#'
#' @param dataframe dataframe to be inputted
#' @param group_name variable specifying groups
#' @return a dataframe with all columns containing characters, converted to numeric
#' @export
ConvertChar <- function(dataframe, group_name)
{
  new_frame <- data.frame(matrix(nrow=1,ncol=1))
  colname_new <- list()
  frame_list <- list()
  for (i in colnames(dataframe))
  {
    e = 1
    ifelse((class(dataframe[[i]])=="character"),
           {
             if(length(dataframe[[i]]!=0))
                {
                  if(datedetect(dataframe[[i]])==TRUE)
                  {
                    next
                  }
                }
             #colnamelist = colnames(dataframe)
             newvarlist = list()
             charlist = list()
             numlist = list()
             start=1
             count=0
             for(j in dataframe[[i]])
             {
               if((is.null(j)==TRUE)| j=='')
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
             for(x in charlist)
             {
               numlist <- append(numlist,count)
               count=count+1
             }
             ifelse(((length(charlist)>2)&(is_list_element(charlist, group_name)==FALSE)),
                    {
                      #colnamelist <- colnames(dataframe)
                      d = 1
                      m = 1
                      while(m <= length(charlist))
                      {
                        newvarlist = list()
                        blanklist = list()
                        for (z in dataframe[[i]])
                        {
                          ifelse((z==charlist[[m]]),
                                 {
                                   newvarlist = append(newvarlist, 1)
                                 },
                                 {
                                   newvarlist = append(newvarlist, 0)
                                 })
                          if(e==length(charlist))
                          {
                            blanklist = append(blanklist,'')
                          }
                        }
                        #colnamelist <- append(paste(charlist[[m]]),colnamelist)
                        colname_new <-append(paste(charlist[[m]]),colname_new)
                        #dataframe <- data.frame(unlist(newvarlist),dataframe)
                        new_frame <- data.frame(unlist(newvarlist),new_frame)
                        colnames(new_frame) <- colname_new

                        m = m + 1
                        e = e + 1
                      }
                      dataframe <- remove_var(dataframe, i)
                      #colnamelist <- colnames(dataframe)
                      # dataframe <- data.frame(unlist(blanklist),dataframe)
                      new_frame <-data.frame(unlist(blanklist),new_frame)
                      #colnamelist <- append(paste(i), colnamelist)
                      colname_new <- append(paste(i),colname_new)
                      colnames(new_frame)<-colname_new
                    },
                    {
                      while(start <= length(dataframe[[i]]))
                      {
                        charlistnum=1
                        while(charlistnum<=length(charlist))
                        {
                          ifelse((dataframe[[i]][[start]]==charlist[[charlistnum]]),
                                 {
                                   dataframe[[i]][[start]] <- numlist[[charlistnum]]
                                   start = start+1
                                   break
                                 },
                                 {
                                   charlistnum = charlistnum + 1
                                 })
                        }
                        next
                      }
                      next
                    })
             next
           },
           {
             next
           })
  }
  dataframe <- as.data.frame(apply(dataframe, 2, as.numeric))
  new_frame <- remove_var(new_frame, "NA")
  new_frame <- as.data.frame(apply(new_frame, 2, as.numeric))
  frame_list <- list(dataframe,new_frame)
  return(frame_list)
}
