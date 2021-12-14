v = sample.int(20,10)  #1
cat("Vector v = {", v,"}\n")

length_of_v = length(v)  #2
cat("Length of our vector", length_of_v,"\n")

numOfNumbers = 0   #3.a
for(iii in 5:15){
  if(iii %in% v){
    numOfNumbers = numOfNumbers + 1;
  }
}
cat(numOfNumbers,"numbers in [5,15]\n")

arithmitis = numOfNumbers  #3.b
pososto = (arithmitis/length_of_v) * 100
cat("Percentage", pososto,"%\n")

vector_powers= c()  #4.a
vector_powers[1] = 1
for(i in 2:11){
  vector_powers[i] <- 2^(i-1)
}
cat("Powers of 2: ", vector_powers,"\n")

dummy_vector= c()  #4.b
for(v in 1:5)
  dummy_vector[v] <- 1/v
cat("All 1/n numbers: ", dummy_vector,"\n")

log_vector= c()  #4.c
log_vector = log2(dummy_vector)
cat("All 1/n log2 numbers: ", log_vector,"\n")

u <- runif(100, 0, 10)  #5.a
result.mean <- mean(u)
cat("Mean: ", result.mean,"\n")

standard_deviation <- sd(u)  #5.b
cat("Standard deviation: ", standard_deviation,"\n")

variance <- var(u)  #5.c
cat("Variance: ", variance,"\n")

less_than_four= c()  #6
between_four_eight= c()
greater_than_eight= c()

for(i in 1:100){
    if(u[i]<4)less_than_four <- append(less_than_four,u[i])
    else if(4<=u[i] && u[i]<=8)between_four_eight <- append(between_four_eight,u[i])
    else if(u[i]>8)greater_than_eight <- append(greater_than_eight,u[i])
}

cat("Less than four: ", length(less_than_four),"\n")
cat("Between four and eight: ", length(between_four_eight),"\n")
cat("Greater than eight: ", length(greater_than_eight),"\n")

pososto_less_than_four = (length(less_than_four) / length(u)) * 100  #7
pososto_between_four_eight = (length(between_four_eight) / length(u)) * 100
pososto_greater_than_eight = (length(greater_than_eight) / length(u)) * 100
# Ta pososta einai logika giati panta exoun athroisma 100,
# kai to kathena ksexwrista einai, panta logiko sugkritika me to fasma arithmwn mou 
# antiproswpeuei

cat("Less than four - Percentage: ", pososto_less_than_four,"%\n")
cat("Between four and eight - Percentage: ", pososto_between_four_eight,"%\n")
cat("Greater than eight - Percentage: ", pososto_greater_than_eight,"%\n")

k <- rpois(1000,5) #8
zero_vector= c()
gr_eq_twelve= c()
for(ii in 1:1000){
  if(k[ii]==0) zero_vector <- append(zero_vector,k[ii]) #8.a
  else if(k[ii]>=12) gr_eq_twelve <- append(gr_eq_twelve,k[ii])
}
number_of_zero = length(zero_vector)
number_of_greq_twelve = length(gr_eq_twelve)
cat("Zeros: ", number_of_zero,"\n")
cat("Greater or equal to 12: ", number_of_greq_twelve,"\n")
pososto_gia_12aria = (number_of_greq_twelve/length(k)) * 100 #8.b


cat("Greater or equal to 12 - Percentage: ", pososto_gia_12aria,"%\n")

# 8.c
#  Epeita apo kapoia treksimata tou programmatos, parathroume oti oi pithanothtes tou 
#  na dexthke kapoios mathitis >=12 kliseis kumainetai se <1%. Etsi, parathroume
#  oti einai sigoura ektos tou 99% tou sunolou kai mporoume na ton therwrhsoume outlier. Ara, nai
#  uparxoun vasimes upopsies oti einai eksosxolikos. 
#  Se periptwsh pou theloume na isxuropoihsoume ta krithria mas gia na ton thewrhsoume outlier
#  elegxoume: if(klhseis > (mean(klhseis + 2*sd(klhseis))), to opoio epishs mas kaluptei.
#  An theloume na tis isxuropoihsoume ki allo elegxoume kai an if(klhseis > (mean(klhseis + 3*sd(klhseis)))
#  Me auto ton tropo eimaste vevaioi oti mporoume na ton thewrhsoume outlier giati kai oi 3 sunthikes mas
#  kaluptontai.



