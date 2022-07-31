#Debai
A = matrix(c(1,1),nrow = 1,ncol = 2)
A
a = 7/2
Xtb = c(1,1/2)
S = matrix(c(2,-1,-1,2),2,2)
S
alpha = 0.05
n = 6


#Code
Testlinear2 = function(A,a,D,alpha){
  q = nrow(A)
  p = ncol(A)
  #Tim S
  Xtb = colMeans(D)
  X = as.matrix(Xtb%*%t(Xtb))
  S = (1/n)*t(D)%*%D - X
  #Tinhtoan
  T5 = t(A%*%Xtb-a)%*%solve(A%*%S%*%t(A))%*%(A%*%Xtb-a)
  t0 = (q/(n-q))*qf(1-alpha,q,n-q)
  #Mien bac bo
  if(T5>t0){
    c = "Bac bo H0"
  } else {
    c = "Chap nhan H0"
  }
  message = paste(c,T4,t0)
  print(message)
}
Testlinear2(A,a,D,alpha)


q = nrow(A)
p = ncol(A)
#Tinhtoan
T5 = t(A%*%Xtb-a)%*%solve(A%*%S%*%t(A))%*%(A%*%Xtb-a)
T5
t0 = (q/(n-q))*qf(1-alpha,q,n-q)
t0
#Mien bac bo
if(T5>t0){
  c = "Bac bo H0"
} else {
  c = "Chap nhan H0"
}
message = paste(c,T5,t0)
print(message)