#=========
#Cac gia tri
Testmean2= function(muy0,D,alpha){
  n = nrow(D)
  p = ncol(D)
  Xtb = colMeans(D)
  Xtb
  t(Xtb)%*%Xtb
  #Tinh toan
  X = as.matrix(Xtb%*%t(Xtb))
  S = (1/n)*t(D)%*%D - X
  Stru1 = solve(S)
  T2 = t(Xtb - muy0)%*%Stru1%*%(Xtb - muy0)
  T2
  t0 = (p/(n-p))*qf(1-alpha,p,n-p)
  t0
  #Mien bac bo
  if(T2>t0){
    c = "Bac bo H0"
  } else {
    c = "Chap nhan H0"
  }
  message = paste(c,T2,t0)
  print(message)
}
#Debai
####TEST CHUONG TRINH
muy0 = c(1,1,0)
D = matrix(c(1,2,3,1,-1,-2,-3,2,0,0,0,1,1,1,1,4,1,2,1,1,1),nrow=7,ncol=3,byrow = T)
D
alpha = 0.1
Testmean2(muy0,D,alpha)
################

Sigma0=matrix(c(20,100,100,1000),2,2)
Sigma0
X = c(69,74,68,70,72,67,66,70,76,68,72,79,74,67,66,71,74,75,75,76)
Y = c(153,175,155,135,172,150,115,137,200,130,140,265,185,112,140,150,165,185,210,220)
D = t(matrix(c(X,Y),2,20,byrow=T))
D
alpha=0.05
