

n1 = 15
n2 = 10
p = 2
xtb1 = c(4084.0,2580.5)
xtb2 =c(4307.2,4925.2)
xtb1
S1 = (10^7)*matrix(c(1.6635,1.2410,1.2410,1.3747),2,2)
S2 = (10^7)*matrix(c(1.2248,1.1425,1.1425,1.5112),2,2)
alpha = 0.1

Testlinear6 = function(n1,n2,p,xtb1,xtb2,S1,S2,alpha){
  #q = nrow(A)
  #p = ncol(A)
  #Tim S
  #Xtb = colMeans(D)
  #Tinhtoan
  S = ((n1+n2)^-1)*(n1*S1+n2*S2)
  Stru1 = solve(S)
  T6 = ((n1*n2*(n1+n2-p-1))/(p*(n1+n2)^2))*t(xtb1-xtb2)%*%Stru1%*%(xtb1-xtb2)
  t0 = qf(1-alpha,p,n1+n2-p-1)
  #Mien bac bo
  if(T6>t0){
    c = "Bac bo H0"
  } else {
    c = "Chap nhan H0"
  }
  message = paste(c,T6,t0)
  print(message)
}
Testlinear6(n1,n2,p,xtb1,xtb2,S1,S2,alpha)

