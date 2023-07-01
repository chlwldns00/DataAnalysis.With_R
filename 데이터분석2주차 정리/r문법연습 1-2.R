temp<-c(1,-1,-2,-1,1,1,1,1,1)
m4<-matrix(temp,nrow=3,byrow=T)
m4

onlyp<-function(Obj)
{
  for(i in 1:nrow(Obj))
  {
    for(j in 1:ncol(Obj))
    {
      if(Obj[i,j]>0)
      {}
      else
      {
        Obj[i,j]=0
      }
    }
  }
  return(Obj)
}
onlym<-function(Obj)
{
  for(i in 1:nrow(Obj))
  {
    for(j in 1:ncol(Obj))
    {
      if(Obj[i,j]<0)
      {}
      else
      {
        Obj[i,j]=0
      }
    }
  }
  return(Obj)
}
myfunc<-function(Obj,Margin)
{
  plus<-vector()
  minus<-vector()
  result<-vector()
  m1<-matrix()
  m2<-matrix()
  if(Margin==1)
  {
    m1<-onlyp(Obj)

    m2<-onlym(Obj)
    
    plus<-c(apply(m1,1,sum))
    
    minus<-c(apply(m2,1,sum))
    result<-c(plus,minus)
    rm<-matrix(result,byrow=F,ncol=2)
    return(rm)
  }
  else
  {
    m1<-onlyp(Obj)
    
    m2<-onlym(Obj)
    
    plus<-c(apply(m1,2,sum))
    
    minus<-c(apply(m2,2,sum))
    result<-c(plus,minus)
    rm<-matrix(result,byrow=F,ncol=2)
    return(rm)
  }
}
myfunc(m4,2)

