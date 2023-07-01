library(stringr)
v1<-c("aa","bb","cc","abc","ifls","abb","ice","baby","coal")
m1<-matrix(v1,nrow=3,byrow=T)
m1
st<-function(x)
{
  return(x)
}
myfunc<-function(Obj,Margin)
{
  col<-vector()
  row<-vector()
  result<-vector()
  if(Margin==1)
  {
    col<-c(apply(Obj,1,st))
    result<-str_which(col,"a")
    return(result)
  }
  
    
   
  
  else
  {
    row<-c(apply(Obj,2,st))
    result<-str_which(row,"a")
    return(result)
  }
    
    
  
}
myfunc(m1,2)

