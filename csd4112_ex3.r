#askisi 1
matrix1 <- matrix(c(runif(100, min = 0, max = 10), rnorm(100, mean = 50, sd = 15), rbinom(100, 1000, 0.1), rpois(100, 10)), nrow=4, ncol = 100)

#1.1,1.2,1.3,1.4,1.5,1.6

print("ta prints gia tin prwti grammi")
print(mean(matrix1[1,]))
print(median(matrix1[1,]))
print(sum(matrix1[1,]>=mean((matrix1[1,])-10) & matrix1[1,]<=(mean(matrix1[1,])+10))/100)
print(sd(matrix1[1,]))
print(quantile(matrix1[1,]))

print("ta prints gia tin deuteri grammi")
print(mean(matrix1[2,]))
print(median(matrix1[2,]))
print(sum(matrix1[2,]>=mean((matrix1[2,])-10) & matrix1[2,]<=(mean(matrix1[2,])+10))/100)
print(sd(matrix1[2,]))
print(quantile(matrix1[2,]))

print("ta prints gia tin triti grammi")
print(mean(matrix1[3,]))
print(median(matrix1[3,]))
print(sum(matrix1[3,]>=mean((matrix1[3,])-10) & matrix1[3,]<=(mean(matrix1[3,])+10))/100)
print(sd(matrix1[3,]))
print(quantile(matrix1[3,]))

print("ta prints gia tin tetarti grammi")
print(mean(matrix1[4,]))
print(median(matrix1[4,]))
print(sum(matrix1[4,]>=mean((matrix1[4,])-10) & matrix1[4,]<=(mean(matrix1[4,])+10))/100)
print(sd(matrix1[4,]))
print(quantile(matrix1[4,]))

#1.6.diamesos
#h diamesos se ola einai sto 50%
#1.6 vector me ta athroismata
#1.6 parametroi katanomwn kai meses times
#omoiomorfi : sto wikipedia anaferei pws mean : 1/2 * (a+b)
#kanoniki : apantatai stin ekfwnisi
#diwnumiki : sto wikipedia anaferei pws mean : np
#poisson : sto wikipedia anaferei pws mean : lambda
sum1 = 0
sum2 = 0
sum3 = 0
sum4 = 0

for(i in 1:100){
  sum1 = sum1+matrix1[1,i]
  sum2 = sum1+matrix1[2,i]
  sum3 = sum1+matrix1[3,i]
  sum4 = sum1+matrix1[4,i]
}

print("to vector me ta athroismata twn rows")
vector = c(sum1,sum2,sum3,sum4)
print(vector)

#askisi 2
omoiomorfi = sample(100,runif(20,0,100),TRUE)
kanonikis = sample(100,rnorm(20, mean = 50, sd = 15),TRUE)
diwnumikis = sample(100,rbinom(20, 1000, 0.1),TRUE)
print("gia omoiomorfi (mean,sd)")
print(mean(omoiomorfi))
print(sd(omoiomorfi))
print("gia kanoniki (mean,sd)")
print(mean(kanonikis))
print(sd(kanonikis))
print("gia diwnumiki (mean,sd)")
print(mean(diwnumikis))
print(sd(diwnumikis))
#parathroume oti ta apotelesmata mean kai sd einai paromoia me mikri apoklisi

#askisi 3

#setwd('C:/Users/George Vamvakousis/Desktop/csd/R/assignment3')
a = read.table("pwm.txt")
unlist(a)
m1 = matrix("", nrow=nrow(a), ncol=11)
m1
for(i in 1:nrow(a)){
  v = unlist(strsplit(a[i,], split=""))
  m1[i,] = v
}
m1
countmat = matrix(0, nrow=4, ncol=11)
rownames(countmat) = c("A", "C", "G", "T")
for(i in 1:nrow(m1)){
  for(j in 1:ncol(m1)){
    mychar = m1[i,j]
    ##print(mychar)
    countmat[mychar, j] = countmat[mychar, j] + 1
  }
}

cs = colSums(countmat)
freqmat = t(t(countmat)/cs)
tmp.mat = freqmat/0.25 + 10^(-6)
pwm = log2(tmp.mat)

library(seqLogo)
seqLogo(freqmat)

#suntirimenes 7 kai 4 (ligoteres katastaseis)
#mi suntirimeni i 3 (perissoteres katastaseis)
