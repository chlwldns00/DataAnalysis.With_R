install.packages("stringr")
library(stringr)
v1<-c("aa","bb","cc","abc","ifls","abb","ice","baby","coal")
m1<-matrix(v1,nrow=3,byrow=T)
m1
str_which(m1,"a")
myfunc<-function(Obj,Margin)
{
  col<-vector()
  row<-vector()
  if(Margin==1)
  {
    for(i in 1:nrow(Obj))
    {
      for(j in 1:ncol(Obj))
      {
        if(str_detect(Obj[i,j],"a"))
        {
          col<-c(col,i)
          row<-c(row,j)
        }
        else
        {}
      }
    }
    
    df<-data.frame(col,row)
    return(df)
  }
  else
  {
    for(i in 1:nrow(Obj))
    {
      for(j in 1:ncol(Obj))
      {
        if(str_detect(Obj[j,i],"a"))
        {
          col<-c(col,j)
          row<-c(row,i)
        }
        else
        {}
      }
    }
    
    df<-data.frame(col,row)
    return(df)
  }
}
myfunc(m1,1)
