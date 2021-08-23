



Indicadora<-function(x){
  vec=c()
  for (i in x){
    if (i>=0){
      vec<-c(vec,i)
    }
    else{
      vec<-c(vec,0)
    }
  }
  return(vec)
}




f<-function(x,beta,w,b,alpha){
  aux<-w*x+b
  res<-sum(beta*Indicadora(aux))+alpha
  return(res)
}
xvec<-c(-100:100)
yvec<-c()
for (i in xvec){
  yvec=c(yvec,f(i,c(5,58,0.1),c(0.5,0.4,0.1),c(50,80,8),0))
}  

plot(xvec,yvec,type = 'l')
