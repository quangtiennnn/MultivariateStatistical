p = 5
n = 10
N = 10000
alpha = 0.1
for (j in 1:N){ 
  Y = 1
    for (i in 1:p){
    Y = Y*rchisq(1,df = n+1-i)
    }
  sample[j]=Y
  }
sample


s = sort(sample)
k = floor(N*alpha)
k
lambda = s[k]
lambda

#--------
p = 5
m = 6
n = 7
N = 10000
alpha = 0.1

#Cau a)
for (j in 1:N){ 
  Y = 1
  for (i in 1:p){
    Y = Y*rbeta(1,(n+1-i)/2,m/2)
  }
  sample[j]=Y
}
sample

#Cau b)
cv = function(sample,alpha){
s = sort(sample)
k = floor(N*alpha)
lambda = s[k]
lambda
}
cv(sample,alpha)

