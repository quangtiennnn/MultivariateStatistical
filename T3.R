#De bai
Sigma0=matrix(c(20,100,100,1000),2,2)
Sigma0
X = c(69,74,68,70,72,67,66,70,76,68,72,79,74,67,66,71,74,75,75,76)
Y = c(153,175,155,135,172,150,115,137,200,130,140,265,185,112,140,150,165,185,210,220)
D = t(matrix(c(X,Y),2,20,byrow=T))
D
alpha=0.05
#CODE
Testcov = function(Sigma0,D,alpha){ 
  n = nrow(D)
  p = ncol(D)
  Xtb = colMeans(D)
  Xtb
  #Tinh toan
  X = as.matrix(Xtb%*%t(Xtb))
  S = (1/n)*t(D)%*%D - X
  #=====
  A = solve(Sigma0)%*%S
  T3 = n*sum(diag(solve(Sigma0)%*%S))-n*log(det(A))-n*p
  T3
  t0 = qchisq(1-alpha,p*(p+1)/2)
  t0
  #Mien bac bo
  if(T3>t0){
    c = "Bac bo H0"
  } else {
    c = "Chap nhan H0"
  }
  message = paste(c,T3,t0)
  print(message)
  }
Testcov(Sigma0,D,alpha)


n = nrow(D)
p = ncol(D)
n
p
#Tinh toan
Xtb = colMeans(D)
X = as.matrix(Xtb%*%t(Xtb))
S = (1/n)*t(D)%*%D - X
S
#=====
A = solve(Sigma0)%*%S
A
sum(diag(solve(Sigma0)%*%S))
T3 = n*sum(diag(solve(Sigma0)%*%S))-n*log(det(A))-n*p
T3
t0 = qchisq(1-alpha,p*(p+1)/2)
t0
