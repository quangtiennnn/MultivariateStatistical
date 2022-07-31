#Debai
a=c(1,2)
A=matrix(c(1,2,3,4,5,6),2,3)
Sigma=matrix(c(2,0,0,0,2,0,0,0,2),3,3)
D=matrix(c(1,1,-3,0,1,4,1,2,-1,2,0,1,1,1,3,-2,0,1,1,2,1),7,3)
alpha=0.1

#CODE
Testlinear1 = function(Sigma,A,a,D,alpha){
q = nrow(A)
p = ncol(A)
n = nrow(D)
Xtb = colMeans(D)
T4 = n*t(A%*%Xtb-a)%*%solve(A%*%Sigma%*%t(A))%*%(A%*%Xtb-a)
t0 = qchisq(1-alpha,q)
#Mien bac bo
if(T4>t0){
  c = "Bac bo H0"
} else {
  c = "Chap nhan H0"
}
message = paste(c,T4,t0)
print(message)
}
Testlinear1(Sigma,A,a,D,alpha)