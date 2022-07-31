#Debai

muy0 = c(70,170)
X = c(69,74,68,70,72,67,66,70,76,68,72,79,74,67,66,71,74,75,75,76)
Y = c(153,175,155,135,172,150,115,137,200,130,140,265,185,112,140,150,165,185,210,220)
D = t(matrix(c(X,Y),2,20,byrow=T))
D
alpha=0.05

#=========
Testmean1= function(muy0,sigma,D,alpha){
#Cac gia tri
n = nrow(D)
p = ncol(D)
sigmatru1 = solve(sigma)
sigmatru1
Xtb = colMeans(D)
Xtb
#Tinh toan
T1 = n%*%t(Xtb - muy0)%*%sigmatru1%*%(Xtb - muy0)
t0 = qchisq(1-alpha,p)
#Mien bac bo
  if(T1>t0){
    c = "Bac bo H0"
  } else {
    c = "Chap nhan H0"
  }
  message = paste(c,T1,t0)
  print(message)
}
Testmean1(muy0,sigma,D,alpha)
Xtb


n = nrow(D)
p = ncol(D)
sigmatru1 = solve(sigma)
sigmatru1
Xtb = colMeans(D)
Xtb
#Tinh toan
T1 = n%*%t(Xtb - muy0)%*%sigmatru1%*%(Xtb - muy0)
t0 = qchisq(1-alpha,p)
T1
t0

