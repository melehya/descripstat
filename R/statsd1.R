#' statsd1

#' Computes table of descriptive statistics and table of p-values from pairwise comparisons
#'
#' Set fisher allows user to specify the threshold number of observations beneath which fisher's exact test
#' will be used to compare categorical variables. Above this threshold chi squared will be used.
#' fxn will determine if continuous variables are normally distributed or not and use t.test or
#' man-whitney for 2 groups, anova or kriskal wallis for 3 or more groups
#'
#' @param dataframe A dataframe
#' @param set_fisher Set the value beneath which fisher's exact will be used
#' @param group_name A column variable that specifies groups
#' @param number_of_groups The number of groups being compared
#' @return A list of tables, one summarizing descriptive statistics, the other a list of p-values from pairwise testing if number of groups > 2
#' @export

statsd1 <- function(dataframe, set_fisher=10, group_name, number_of_groups){
  ifelse((number_of_groups>1),
         {
           categorical_table <- data.frame(matrix(nrow=1,ncol=(number_of_groups+2)))
         },
         {
           categorical_table <- data.frame(matrix(nrow=1,ncol=(number_of_groups+1)))
         })

  # Takes group_name and converts it to numeric starting from 0 if
  # it is in character form
  temp_dat = subset(dataframe, select=c(group_name))
  temp_dat = ConvertChar(temp_dat, group_name)

  dataframe[[group_name]] <- temp_dat[[1]][[group_name]]
  # #call ConvertChar on inputted dataframe to convert all character strings to numbers,
  # #will order values 0,1,2,etc.

  dataframe_list <- ConvertChar(dataframe, group_name)
  dataframe1 <- dataframe_list[[1]]

  #intialzes list to hold table column names
  cattab_colnames=list()
  pairtab_colnames=list()

  #indexes for while loop that produces column names
  n = 0
  m = 1

  # intializes empty list that we will use to access each group by index
  groups <- list()

  #produces column names of ultimate output table
  #structured variable, group ____, group _____,..., p-val
  my_list_colnames <- list_from_user(number_of_groups)
  while (n<number_of_groups)
  {
    groups[[m]] <- dataframe1[dataframe1[[group_name]]==n,]
    total <- paste0("(","n= ",((length(na.omit(groups[[m]][[group_name]])))),")")
    cattab_colnames <- append(cattab_colnames,
                              list(paste(my_list_colnames[[m]],total)))
    pairtab_colnames <- append(pairtab_colnames,
                               list(paste(m)))
    m = m+1
    n = n + 1
  }
  categorical_table <- data.frame(matrix(nrow=1,ncol=(number_of_groups+2)))
  ifelse((number_of_groups>1),
         {
           colnames(categorical_table)<-c("variable", cattab_colnames, "P-value")
         },
         {
           colnames(categorical_table)<-c("variable",cattab_colnames)
         })
  pairtable = data.frame(matrix(nrow=1,ncol=(number_of_groups+3)))
  colnames(pairtable)<- c("variable",pairtab_colnames,number_of_groups+1,number_of_groups+2)

  # outer most loop that sequentially steps through each variable in the imported csv
  for (i in colnames(dataframe1)){
    if((length(na.omit(dataframe1[[i]]))<5))
    {
      next
    }
    if(i == group_name)
    {
      next
    }
    a=1
    b = 1
    c=1
    d=1
    fish=0
    cat_list_percent = list()
    cat_list_num = list()
    num_percentlist=list()
    cont_anal_tab=list()
    my_list=list()
    norm_list=list()
    varskip = 0

    ifelse((((is.integer(dataframe1[[i]])==TRUE|is.numeric(dataframe1[[i]])==TRUE)&
               (max(dataframe1[[i]],na.rm=TRUE)==1))),
           {
             while(c<(number_of_groups+1))
             {
               if((sum(na.omit(groups[[c]][[i]])))<set_fisher)
               {
                 fish=1
               }
               guy <- (length(na.omit(groups[[c]][[i]])))
               sum <- (sum(groups[[c]][[i]], na.rm=TRUE))
               cat_list_percent[[c]] = paste0((round(((sum / guy)*100),digits=0)),"%")
               sum <- paste0("(",sum,")")
               cat_list_percent[[c]]=paste((cat_list_percent[[c]]),sum)
               num_percentlist<-append(num_percentlist,
                                       (cat_list_percent[[c]]))
               c = c + 1
             }

             ifelse((number_of_groups>1),
                    {
                      ifelse((number_of_groups<3),
                             {
                               ifelse((fish==0),
                                      {
                                        mytab <- with(dataframe1,table(dataframe1[[group_name]],
                                                                       dataframe1[[i]]))
                                        my_chi <- chisq.test(mytab)
                                        categorical_table <-rbind(categorical_table,
                                                                  c(i,num_percentlist,
                                                                    p_value_round(my_chi$p.value)))
                                      },
                                      {
                                        mytab <- with(dataframe1,table(dataframe1[[group_name]],
                                                                       dataframe1[[i]]))
                                        my_fisher <- try(fisher.test(mytab, workspace = 2*10^8))
                                        ifelse(((class(my_fisher))=="try-error"),
                                               {
                                                 categorical_table <-rbind(categorical_table,
                                                                           c(i,num_percentlist,
                                                                             "error"))
                                               },
                                               {
                                                 categorical_table <-rbind(categorical_table,
                                                                           c(i,num_percentlist,
                                                                             p_value_round(my_fisher$p.value)))
                                               })
                                      })
                             },
                             {
                               ifelse((fish==0),
                                      {
                                        mytab <- with(dataframe1,table(dataframe1[[group_name]],
                                                                       dataframe1[[i]]))
                                        my_test <- chisq.test(mytab)
                                        categorical_table <-rbind(categorical_table,
                                                                  c(i,num_percentlist,
                                                                    p_value_round(my_test$p.value)))
                                      },
                                      {
                                        mytab <- with(dataframe1,table(dataframe1[[group_name]],
                                                                       dataframe1[[i]]))
                                        my_test <- try(fisher.test(mytab, workspace = 2*10^8))
                                        ifelse(((class(my_test))=="try-error"),
                                               {
                                                 categorical_table <-rbind(categorical_table,
                                                                           c(i,num_percentlist,
                                                                             "error"))
                                                 next
                                               },
                                               {
                                                 categorical_table <-rbind(categorical_table,
                                                                           c(i,num_percentlist,
                                                                             p_value_round(my_test$p.value)))
                                               })
                                      })
                               pairlist = list()

                               ifelse((my_test$p.value<0.05),
                                      {
                                        startvar = 1
                                        endvar=2
                                        z=1
                                        while(z <= number_of_groups+5)
                                        {
                                          total_frame = data.frame()
                                          if(number_of_groups==4)
                                          {
                                            guy<-as.data.frame(groups[startvar])
                                            me<-as.data.frame(groups[endvar])
                                            ifelse((is.null(cont_anal_tab[startvar])==FALSE
                                                    &is.null(cont_anal_tab[endvar])==FALSE
                                                    &sum(na.omit(guy[[i]]))>0
                                                    &sum(na.omit(me[[i]]))>0),
                                                   {
                                                     total_frame <- rbind((as.data.frame(groups[startvar])),(as.data.frame(groups[endvar])))
                                                     ifelse(((sum(total_frame[[i]], na.rm=TRUE)>0)),
                                                            {
                                                              ifelse((fish==0),
                                                                     {
                                                                       newtab <- with(total_frame,table(total_frame[[group_name]],
                                                                                                        total_frame[[i]]))
                                                                       my_test <- chisq.test(newtab)
                                                                       pairlist <- append(pairlist, p_value_round(my_test$p.value))
                                                                     },
                                                                     {
                                                                       newtab <- with(total_frame,table(total_frame[[group_name]],
                                                                                                        total_frame[[i]]))
                                                                       my_test <- try(fisher.test(newtab, workspace = 2*10^8),silent=TRUE)
                                                                       ifelse((class(my_test)=="try-error"),
                                                                              {
                                                                                pairlist <- append(pairlist, "no observed values in both groups")
                                                                              },
                                                                              {
                                                                                pairlist <- append(pairlist, p_value_round(my_test$p.value))
                                                                              })

                                                                     })
                                                            },
                                                            {
                                                              # endvar = endvar + 1
                                                              # z = z + 1
                                                              # next
                                                            })
                                                     if(endvar==number_of_groups)
                                                     {
                                                       startvar = startvar + 1
                                                       ifelse((endvar==number_of_groups&startvar>1),
                                                              {
                                                                startvar = startvar - 1
                                                                endvar = endvar - 1
                                                              },
                                                              {
                                                                endvar = endvar-2
                                                              })
                                                     }
                                                     endvar = endvar + 1
                                                     z = z + 1
                                                   },
                                                   {
                                                     if(endvar==number_of_groups)
                                                     {
                                                       startvar = startvar + 1
                                                       ifelse((z==number_of_groups+1),
                                                              {
                                                                startvar = startvar - 1
                                                                endvar = endvar - 1
                                                              },
                                                              {
                                                                endvar = endvar-2
                                                              })
                                                     }
                                                     endvar = endvar + 1
                                                     pairlist <- append(pairlist, '')
                                                     z = z + 1
                                                   })
                                          }
                                          if(number_of_groups==3)
                                          {
                                            if(z>3)
                                            {
                                              pairlist <- append(pairlist,'')
                                              pairlist <- append(pairlist,'')
                                              pairlist <- append(pairlist,'')
                                              break
                                            }
                                            guy<-as.data.frame(groups[startvar])
                                            me<-as.data.frame(groups[endvar])
                                            ifelse((is.null(cont_anal_tab[startvar])==FALSE
                                                    &is.null(cont_anal_tab[endvar])==FALSE
                                                    &sum(na.omit(guy[[i]]))>0
                                                    &sum(na.omit(me[[i]]))>0),
                                                   {
                                                     total_frame <- rbind((as.data.frame(groups[startvar])),(as.data.frame(groups[endvar])))
                                                     total_frame <- rbind((as.data.frame(groups[startvar])),(as.data.frame(groups[endvar])))
                                                     ifelse(((sum(total_frame[[i]], na.rm=TRUE)>0)),
                                                            {
                                                              ifelse((fish==0),
                                                                     {
                                                                       newtab <- with(total_frame,table(total_frame[[group_name]],
                                                                                                        total_frame[[i]]))
                                                                       my_test <- chisq.test(newtab)
                                                                       pairlist <- append(pairlist, p_value_round(my_test$p.value))
                                                                     },
                                                                     {
                                                                       newtab <- with(total_frame,table(total_frame[[group_name]],
                                                                                                        total_frame[[i]]))
                                                                       my_test <- fisher.test(newtab)
                                                                       pairlist <- append(pairlist, p_value_round(my_test$p.value))

                                                                       #### fix
                                                                     })

                                                            },
                                                            {
                                                              # endvar = endvar + 1
                                                              # z = z + 1
                                                              # next
                                                            })
                                                     if(endvar==number_of_groups)
                                                     {
                                                       startvar = startvar + 1
                                                       startvar=startvar-1
                                                       endvar = endvar - 1
                                                     }
                                                     endvar = endvar + 1
                                                     z = z + 1
                                                   },
                                                   {
                                                     if(endvar==number_of_groups&startvar<2)
                                                     {
                                                       startvar = startvar + 1
                                                       startvar=startvar-1
                                                       endvar = endvar-1
                                                     }
                                                     endvar = endvar + 1
                                                     pairlist <- append(pairlist, '')
                                                     z = z + 1
                                                   })
                                          }
                                          if(number_of_groups==5)
                                          {
                                            guy<-as.data.frame(groups[startvar])
                                            me<-as.data.frame(groups[endvar])
                                            ifelse((is.null(cont_anal_tab[startvar])==FALSE
                                                    &is.null(cont_anal_tab[endvar])==FALSE
                                                    &sum(na.omit(guy[[i]]))>0
                                                    &sum(na.omit(me[[i]]))>0),
                                                   {
                                                     total_frame <- rbind((as.data.frame(groups[startvar])),(as.data.frame(groups[endvar])))
                                                     ifelse(((sum(total_frame[[i]], na.rm=TRUE)>0)),
                                                            {
                                                              ifelse((fish==0),
                                                                     {
                                                                       newtab <- with(total_frame,table(total_frame[[group_name]],
                                                                                                        total_frame[[i]]))
                                                                       my_test <- chisq.test(newtab)
                                                                       pairlist <- append(pairlist, p_value_round(my_test$p.value))
                                                                     },
                                                                     {
                                                                       newtab <- with(total_frame,table(total_frame[[group_name]],
                                                                                                        total_frame[[i]]))
                                                                       my_test <- fisher.test(newtab, workspace = 2*10^8)
                                                                       #### fix
                                                                       pairlist <- append(pairlist, p_value_round(my_test$p.value))
                                                                     })
                                                            },
                                                            {
                                                              # endvar = endvar + 1
                                                              # z = z + 1
                                                              # next
                                                            })
                                                     if(endvar==number_of_groups)
                                                     {
                                                       startvar = startvar + 1
                                                       ifelse((endvar==number_of_groups&startvar>1),
                                                              {
                                                                startvar = startvar - 1
                                                                endvar = endvar - 1
                                                              },
                                                              {
                                                                endvar = endvar-3
                                                              })
                                                     }
                                                     endvar = endvar + 1
                                                     z = z + 1
                                                   },
                                                   {
                                                     if(endvar==number_of_groups)
                                                     {
                                                       startvar = startvar + 1
                                                       ifelse((z==number_of_groups+1),
                                                              {
                                                                startvar = startvar - 1
                                                                endvar = endvar - 1
                                                              },
                                                              {
                                                                endvar = endvar-2
                                                              })
                                                     }
                                                     endvar = endvar + 1
                                                     pairlist <- append(pairlist, '')
                                                     z = z + 1
                                                   })

                                          }
                                        }
                                        pairtable <- rbind(pairtable,
                                                           c(i,unlist(pairlist)))

                                        #next
                                      },
                                      {
                                        next
                                      })
                             })
                    },
                    {
                      categorical_table <-rbind(categorical_table,
                                                c(i,num_percentlist,
                                                  ''))
                    })

           },
           {
             ifelse(((is.numeric(dataframe1[[i]]))==TRUE),
                    {
                      normal = 0
                      while(c<(number_of_groups+1))
                      {
                        if(is_normal(groups[[c]][[i]])==TRUE)
                        {
                          normal = normal + 1
                        }

                        ifelse(((length(na.omit(groups[[c]][[i]])))>3),
                               {
                                 guy <- median(groups[[c]][[i]],na.rm=TRUE)
                                 quart1 <- quantile(groups[[c]][[i]],c(0.25),na.rm=TRUE)
                                 quart3 <- quantile(groups[[c]][[i]],c(0.75),na.rm=TRUE)
                                 IQR <- paste0("(",round(quart1,digits=2),"-",
                                               round(quart3,digits=2),")")
                                 med_quart <- paste(round(guy,digits=2),IQR)
                                 my_list<-append(my_list, med_quart)
                                 cont_anal_tab<-append(cont_anal_tab,
                                                       list(jitter(groups[[c]][[i]])))
                                 c=c+1
                               },
                               {
                                 c=c+1

                               })
                      }

                      ifelse((normal==number_of_groups),
                             {
                               while(a<(number_of_groups+1))
                               {
                                 guy <- mean(groups[[a]][[i]],na.rm=TRUE)
                                 std <- sd(groups[[a]][[i]],na.rm=TRUE)
                                 stdpaste <- paste("±", round(std,digits=2))
                                 men_std <- paste(round(guy,digits=2),stdpaste)
                                 norm_list<-append(norm_list, men_std)
                                 list(jitter(groups[[a]][[i]]))
                                 a=a+1

                               }
                               ifelse((number_of_groups>1),
                                      {
                                        ifelse((number_of_groups<3),
                                               {
                                                 my_cox <- t.test((unlist(cont_anal_tab[1])),
                                                                  (unlist(cont_anal_tab[2])),
                                                                  alternative = "two.sided")
                                                 categorical_table <-rbind(categorical_table,
                                                                           c(i,unlist(norm_list),
                                                                             (p_value_round(my_cox$p.value))))
                                               },
                                               {
                                                 my_anova <- aov(dataframe1[[i]]~dataframe1[[group_name]], data=dataframe1)
                                                 categorical_table <-rbind(categorical_table,
                                                                           c(i,unlist(norm_list),
                                                                             (p_value_round(summary(my_anova)[[1]][["Pr(>F)"]]))))

                                                 ifelse((summary(my_anova)[[1]][["Pr(>F)"]]<0.05),
                                                        {
                                                          startvar = 1
                                                          z=1
                                                          endvar=2
                                                          pairlist = list()
                                                          while(z <= number_of_groups+2)
                                                          {
                                                            if(number_of_groups==4)
                                                            {
                                                              ifelse((is.null(cont_anal_tab[startvar])==FALSE
                                                                      &is.null(cont_anal_tab[endvar])==FALSE),
                                                                     {
                                                                       my_cox <- t.test((unlist(cont_anal_tab[startvar])),
                                                                                        (unlist(cont_anal_tab[endvar])),
                                                                                        alternative = "two.sided")
                                                                       pairlist <- append(pairlist, p_value_round(my_cox$p.value))
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar-2
                                                                       }
                                                                       endvar = endvar + 1
                                                                       z = z + 1
                                                                     },
                                                                     {
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar-2
                                                                       }
                                                                       endvar=endvar+1
                                                                       pairlist <- append(pairlist, '')
                                                                       z = z + 1
                                                                     })
                                                            }
                                                            if(number_of_groups==3)
                                                            {
                                                              if(z>3)
                                                              {
                                                                pairlist <- append(pairlist,'')
                                                                pairlist <- append(pairlist,'')
                                                                pairlist <- append(pairlist,'')
                                                                break
                                                              }
                                                              ifelse((is.null(cont_anal_tab[startvar])==FALSE
                                                                      &is.null(cont_anal_tab[endvar])==FALSE),
                                                                     {
                                                                       my_cox <- t.test((unlist(cont_anal_tab[startvar])),
                                                                                        (unlist(cont_anal_tab[endvar])),
                                                                                        alternative = "two.sided")
                                                                       pairlist <- append(pairlist, p_value_round(my_cox$p.value))
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar - 1
                                                                       }
                                                                       endvar = endvar + 1
                                                                       z = z + 1
                                                                     },
                                                                     {
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar - 1
                                                                       }
                                                                       endvar=endvar+1
                                                                       pairlist <- append(pairlist, '')
                                                                       z = z + 1
                                                                     })
                                                            }
                                                            # if(number_of_groups==5)
                                                            # {
                                                            #
                                                            # }
                                                          }
                                                          pairtable <- rbind(pairtable,
                                                                             c(i,unlist(pairlist)))
                                                          next
                                                        },
                                                        {
                                                          next
                                                        })
                                               })
                                      },
                                      {
                                        categorical_table <-rbind(categorical_table,
                                                                  c(i,unlist(norm_list),
                                                                    ""))
                                      })
                             },
                             {
                               ifelse((number_of_groups>1),
                                      {
                                        ifelse((number_of_groups<3),
                                               {
                                                 my_cox <- wilcox.test((unlist(cont_anal_tab[1])),
                                                                       (unlist(cont_anal_tab[2])),
                                                                       alternative = "two.sided")
                                                 categorical_table <-rbind(categorical_table,
                                                                           c(i,(unlist(my_list)),
                                                                             (p_value_round(my_cox$p.value))))
                                               },
                                               {

                                                 my_kw <- kruskal.test(dataframe1[[i]]~dataframe1[[group_name]], data=dataframe1)
                                                 categorical_table <-rbind(categorical_table,
                                                                           c(i,unlist(my_list),
                                                                             (p_value_round(my_kw$p.value))))
                                                 ifelse((my_kw$p.value<0.05),
                                                        {
                                                          startvar = 1
                                                          z=1
                                                          endvar=2
                                                          pairlist = list()
                                                          while(z <= number_of_groups+2)
                                                          {
                                                            if(number_of_groups==4)
                                                            {
                                                              ifelse((is.null(cont_anal_tab[startvar])==FALSE
                                                                      &is.null(cont_anal_tab[endvar])==FALSE),
                                                                     {
                                                                       my_cox <- t.test((unlist(cont_anal_tab[startvar])),
                                                                                        (unlist(cont_anal_tab[endvar])),
                                                                                        alternative = "two.sided")
                                                                       pairlist <- append(pairlist, p_value_round(my_cox$p.value))
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar-2
                                                                       }
                                                                       endvar = endvar + 1
                                                                       z = z + 1
                                                                     },
                                                                     {
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar-2
                                                                       }
                                                                       endvar=endvar+1
                                                                       pairlist <- append(pairlist, '')
                                                                       z = z + 1
                                                                     })
                                                            }
                                                            if(number_of_groups==3)
                                                            {

                                                              if(z>3)
                                                              {
                                                                pairlist <- append(pairlist,'')
                                                                pairlist <- append(pairlist,'')
                                                                pairlist <- append(pairlist,'')
                                                                break
                                                              }
                                                              ifelse((is.null(cont_anal_tab[startvar])==FALSE
                                                                      &is.null(cont_anal_tab[endvar])==FALSE),
                                                                     {
                                                                       my_cox <- t.test((unlist(cont_anal_tab[startvar])),
                                                                                        (unlist(cont_anal_tab[endvar])),
                                                                                        alternative = "two.sided")
                                                                       pairlist <- append(pairlist, p_value_round(my_cox$p.value))
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar - 1
                                                                       }
                                                                       endvar = endvar + 1
                                                                       z = z + 1
                                                                     },
                                                                     {
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar - 1
                                                                       }
                                                                       endvar=endvar+1
                                                                       pairlist <- append(pairlist, '')
                                                                       z = z + 1
                                                                     })
                                                            }
                                                            # if(number_of_groups==5)
                                                            # {
                                                            #
                                                            # }
                                                          }
                                                          pairtable <- rbind(pairtable,
                                                                             c(i,unlist(pairlist)))
                                                          next
                                                        },
                                                        {
                                                          next
                                                        })
                                               })
                                      },
                                      {
                                        categorical_table <-rbind(categorical_table,
                                                                  c(i,unlist(my_list),
                                                                    ''))
                                      })
                             })

                    }, next)
           })
  }
  ifelse((number_of_groups>2),
         {
           pairtable <- pairtable[-1,]
         },
         {
           pairtable <- data.frame()
         })

  x = 1
  column_list <- list()
  while(x < (length(colnames(categorical_table))-1))
  {
    column_list <- append(column_list, '')
    x=x+1
  }
  collist<-list()

  q = 0
  for (b in colnames(dataframe_list[[2]]))
  {
    q = q + 1
  }

  ifelse((q==1),
         {
           ord_frame<-data.frame(dataframe1[[group_name]])
           collist<-colnames(ord_frame)
           collist[[1]]=group_name
           colnames(ord_frame)<-collist
         },
         {
           ord_frame<-data.frame(dataframe1[[group_name]], dataframe_list[[2]])
           collist<-colnames(ord_frame)
           collist[[1]]=group_name
           colnames(ord_frame)<-collist
         })

  n = 0
  m = 1
  group<-list()
  while (n<number_of_groups)
  {
    group[[m]] <- ord_frame[ord_frame[[1]]==n,]
    m = m+1
    n = n + 1
  }

  for (j in colnames(ord_frame))
  {
    if(j == group_name)
    {
      next
    }

    h = 1
    while(h <= length(colnames(ord_frame)))
    {
      ifelse(((paste(ord_frame[[j]][[1]]))=='NA'),
             {
               h = h + 1
             },
             {
               var1 <- 1
               while(var1 < number_of_groups)
               {
                 ifelse((sum(na.omit(group[[var1]][[colnames(ord_frame)[h]]]))<set_fisher),
                        {
                          fish = 1
                        },
                        {
                          fish = 0
                        })
                 var1 = var1 + 1
               }
               h = h + 1
             })
    }
    ifelse(((paste(ord_frame[[j]][[1]]))=='NA'),
           {

             ifelse((fish==0),
                    {
                      dataframe[[j]] = factor(dataframe[[j]])
                      mytab <- with(dataframe,table(dataframe[[group_name]],
                                                    dataframe[[j]]))
                      my_test <- chisq.test(mytab)
                      categorical_table <- rbind(categorical_table,
                                                 c(j,unlist(column_list),p_value_round(my_test$p.value)))
                      next
                    },
                    {
                      dataframe[[j]] = factor(dataframe[[j]])
                      mytab <- with(dataframe,table(dataframe[[group_name]],
                                                    dataframe[[j]]))
                      my_test <- try(fisher.test(mytab, workspace = 2*10^8),silent=TRUE)
                      ifelse((class(my_test)=="try-error"),
                             {
                               categorical_table <- rbind(categorical_table,
                                                          c(j,unlist(column_list),"no observed values in both groups"))
                             },
                             {
                               categorical_table <- rbind(categorical_table,
                                                          c(j,unlist(column_list),p_value_round(my_test$p.value)))
                               next
                             })
                    })
           },
           {
             z=1
             cat_list_percent = list()
             num_percentlist = list()
             while(z <= number_of_groups)
             {
               if((length(na.omit(groups[[z]][[j]])))<set_fisher)
               {

                 fish=1

               }
               guy <- (length(na.omit(group[[z]][[j]])))
               sum <- (sum(group[[z]][[j]], na.rm=TRUE))
               cat_list_percent[[z]] = paste0((round(((sum / guy)*100),digits=0)),"%")
               sum <- paste0("(",sum,")")
               cat_list_percent[[z]]=paste((cat_list_percent[[z]]),sum)
               num_percentlist<-append(num_percentlist,
                                       (cat_list_percent[[z]]))
               z = z + 1
             }
             categorical_table <-rbind(categorical_table,
                                       c(j,num_percentlist,''))

             next
           })
  }
  categorical_table <- categorical_table[-1,]
  my_table_list = list()
  my_table_list = list(categorical_table, pairtable)
}
