
#-----------------------------------------------------
seq = c(18.45,70,1,18.41,65,0,18.39,71,0,18.70,72,0,18.34,94,1)
A = matrix(seq,nrow = 5,ncol = 3,byrow=T)
A

sample.mean=function(x){
  colMeans(x)
}

sample.cov <- function(A){
  Xngang = sample.mean(A)
  as.matrix(Xngang)
  S = (1/nrow(A))* t(A) %*% A - Xngang %*% t(Xngang)
  S
}
sample.cov(A)
#-----------------------------------------------------
sq = c(1,1,-1,1,-1,0)#Theo dong roi cot
E = matrix(sq,nrow = 2,ncol = 3,byrow = T)
E
u = c(0,0,1)
c = c(0,2)
B = c(1,-1)
as.matrix(c)
as.matrix(B)
as.matrix(u)
E%*%u+c
E = matrix(sq,nrow = 2,ncol = 3,byrow = T)
E
t(A)%*%E%*%B
#-----------------------------------------------------
r