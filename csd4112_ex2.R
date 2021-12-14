#1
print("#1")
x <- sample(1:10, 1000, replace=TRUE)
print(x)

#1 a
print("#1a")
print(mean(x))

#1 b
print("#1b")
print(median(x))

#1 c
# Mean: For a data set, the arithmetic mean, also known as average or arithmetic average, 
#is a central value of a finite set of numbers: specifically, the sum of the values 
#divided by the number of values.
# Median: Median is the value separating the higher half from the lower half of a data sample, 
#a population, or a probability distribution.

#1 d
# fainetai logiko giati einai paromoia katanamhmena se plhthos apo 1 ews 10
# kai auto dikaiologeitai apo ton tropo pou katanemei ta stoixeia h sample wste o mesos oros na einai
# konta ston median arithmo tou sunolou
print("#1d")
print(table(x))

#1e
# uparxoun perissoteres times sto diasthma 50-60 sugkritika me to 0-10
# gia na kratithei o mesos oros tou sunolou twn timwn pio konta ston median arithmo tou sunolou
print("#1e")
a = rep(0, 1000)
n=10
for(i in 1:n){
  x <- sample(1:10, 1000, replace=TRUE)
  a = a + x  
}
print(a)
print(table(a))
array=rep(0,10)
array[1]<-sum(a>=0&a<=10)
for(i in 2:10){
  array[i]<-sum(a>=((i-1)*10+1)&a<=i*10)
}
print("values  from 0 to 10")
print(array[1])
for(i in 2:10){
  print(paste0("From ",((i-1)*10+1)," to ",(i*10)))
  print(array[i])
}

#2
print("#2")
y = rbinom(1,10000,0.001)
print(y)
#3
print("#3")
U = rbinom(100,10000,0.001)
print(U)
#3 i
print("#3i")
print(sum(U==0))
#3 ii
print("#3ii")
print(sum(U>=5))
#3 iii
print("#3iii")
print(mean(U))
#3 iv
#afisa to table pou dinetai ws hint se sxolio giati to xrhsimopoihsa mono gia epalitheusi
print("#3iv")
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
print(table(U))
print(getmode(U))
#3 v
print("#3v")
print(max(table))

#4
findMax = function (n,a,b){
  dummy = runif(n,a,b)
  maxDummy = max(dummy)
  return (maxDummy)
}

#4 a
print("#4a")
maxu = c()

for(i in 1:100)
  maxu[i] <- findMax(100,10,1000)

print(maxu)