temp<-c(1,-1,-2,-1,1,1,1,1,1)
m1<-matrix(temp,nrow=3,byrow=T)
m1
myfunc<-function(Obj,Margin) 
{
  result<-vector()
  plus<-vector()
  minus<-vector()
  if(Margin==1)
  {
    for(i in 1:nrow(Obj))
    {
      for(j in 1:ncol(Obj))
      {
        if(Obj[i,j]>0)
        {
          plus<-c(plus,Obj[i,j])
        }
        else
        {
          minus<-c(minus,Obj[i,j])
        }
      }
      result<-c(result,sum(plus),sum(minus))
      plus<-vector()
      minus<-vector()
    }
    
    rm<-matrix(result,nrow=3,byrow=T)
    return(rm)
  }
  else
  {
    for(k in 1:ncol(Obj))
    {
      for(p in 1:nrow(Obj))
      {
        if(Obj[p,k]>0)
        {
          plus<-c(plus,Obj[p,k])
        }
        else
        {
          minus<-c(minus,Obj[p,k])
        }
      }
      result<-c(result,sum(plus),sum(minus))
      plus<-vector()
      minus<-vector()
    }
    
    rm<-matrix(result,nrow=3,byrow=T)
    return(rm)
  }
}
myfunc(m1,1)

