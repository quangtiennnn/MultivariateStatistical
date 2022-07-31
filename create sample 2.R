#16.3

mvnormI=function(p){
  #co dinh mau de chay khong bi sai: set.seed(p)
  rnorm(p,0,1)
}
mvnormI(5)

sqrtmatrix=function(C){
  e = eigen(C)
  V = e$vectors
  return (V%*% diag(sqrt(e$values))%*%t(V))
}

A = matrix(c(1,2,2,5),2,2)
sqrtmatrix(A) 


E = matrix(c(1,2,1,2,5,2,1,2,2),3,3,byrow=T)
E
E^(-1)
u = c(1,0,1)
solve(E)


E = diag(3)
p=3
u = c(1,1,0)
n = 5
as.matrix(u)

mvnormI=function(p){
  #co dinh mau de chay khong bi sai: set.seed(p)
  rnorm(p,0,1)
}
mvnorm=function(u,E){
  Y = mvnormI(length(u))
  X = sqrtmatrix(E)%*%Y+u
  return(X)
}


sample.mvnorm=function(n,u,E){
  X = mvnorm(u,E)
  for (i in 2:n)
  {
      Y = mvnorm(u,E)
      X = c(X,Y)
  }
  A = matrix(X,nrow = n,ncol=length(u),byrow =  T)
  #dong la co mau/ so cot la tp vector
  return(A)
}
#YC1
D = sample.mvnorm(n,u,E)
D
t(D)
t(D)%*%D
rep(0,times = p)
#YC2
Wishart=function(p,E,n){
  D = sample.mvnorm(n,rep(0,times = p),E)#Wishart thi vector trung binh bang 0
  W = t(D)%*%D 
  return(W)
}
W = Wishart(p,E,n)
W^(-1)

E = diag(3)
p=3
u = c(1,1,0)
n = 5
as.matrix(u)
rep(0,times = 5)

#YC3
Hotelling = function(p,n){
  I = diag(p)
  W = Wishart(p,I,n)
  Y = mvnorm(rep(0,times = p),I)
  H = n* t(Y) %*% solve(W) %*% Y
  return(H)
}
Hotelling(p,n)


