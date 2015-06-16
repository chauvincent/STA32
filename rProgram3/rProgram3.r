##################################################
##    Author: Vincent Chau (SID:998947424)    ####
##                Program 3                   ####
##                                            ####
##################################################

# (Problem 1) A.
X <- rnorm(1000)
mean(X)
sd(X)

# (Problem 1) B && C. 

MyChebyShev = function(X,k){

  C <- sum(abs(X - mean(X)) < -k*sd(X) | abs(X - mean(X)) > k*sd(X))
  
  P <- (C/length(X))
    
  if(P < 1/(k*k))
      check = TRUE
  
  if (P > (1/(k*k))) 
      check = FALSE
  
  return(c(mean(X),sd(X),P,check))
}

set.seed(1001)

X1 = rnorm(1000)
MyChebyShev(X1,1)

X2 = rnorm(1000)
MyChebyShev(X2,2)

X3 = rnorm(1000)
MyChebyShev(X3,3)

###############################################################
#                      (Problem 2)                            #
###############################################################
#Part A:
totalV = 131257328
obamaV = 69456897

obamaP = obamaV / totalV
print(obamaP)

#Part B:

dbinom(7, 10, obamaP)

#Part C:

set.seed(9999)

meanSingleSample = function(N=1, n=10, p=obamaP){
  
  R = rbinom(N, n, p)
  print(R)
  
  pHat = R/n
  print(pHat)
  return (pHat)
  
}

meanSingleSample()

#Part D:

set.seed(9999)

variancePHat = function(N = 1000, n = 10, p=obamaP){
    
    # FOR THE TEN
    tenSucess = rbinom(N, n, p)
    tenPHat = tenSucess / n
    print(mean(tenPHat))
    tenPHatVar = sqrt((p*(1-p)/n))
    print(tenPHatVar)
    
    # FOR THE HUNDRED
    hundredSucess = rbinom(N, 100, p)
    hundredPHat = hundredSucess / 100
    print(mean(hundredPHat))
    hundredPHatVar = sqrt((p*(1-p)/100))
    print(hundredPHatVar)
    
    # FOR THE THOUSAND
    thousandSucess = rbinom(N, 1000, p)
    thousandPHat = thousandSucess / 1000
    print(mean(thousandPHat))
    thousandPHatVar = sqrt((p*(1-p)/1000))
    print(thousandPHatVar)
  
    #For TENTHOUSANDS
    tenKSucess = rbinom(N, 10000, p)
    tenKPHat = tenKSucess / 10000
    print(mean(tenKPHat))
    tenKPHatVar = sqrt((p*(1-p)/10000))
    print(tenKPHatVar)
    
}
  
variancePHat()

###############################################################
#                      (Problem 3)                            #
###############################################################

#(Problem 3) Part 1

Dataset1 <- read.table("~/Desktop/winterQuarter14/sta32/rPrograms/rProgram3/Dataset1.txt", quote="\"")
View(Dataset1)

Dataset2 <- read.table("~/Desktop/winterQuarter14/sta32/rPrograms/rProgram3/Dataset2.txt", quote="\"")
View(Dataset2)

Dataset3 <- read.table("~/Desktop/winterQuarter14/sta32/rPrograms/rProgram3/Dataset3.txt", quote="\"")
View(Dataset3)
set.seed(1001)

myBootStrap = function(X, B){
    BootStrapMean = sapply(1:B,function(i){
    NewSample = sample(X,length(X),replace = TRUE)
    MeanOfNew = mean(NewSample)  
    return(MeanOfNew)
    }) ##end of sapply and bootstrap mean

    print(mean(BootStrapMean))
    print(sd(BootStrapMean))
    print(median(BootStrapMean))
    print(mad(BootStrapMean))
}


myBootStrap(Dataset1$V1, 100)
myBootStrap(Dataset1$V1, 1000)
myBootStrap(Dataset1$V1, 10000)

myBootStrap(Dataset2$V1, 100)
myBootStrap(Dataset2$V1, 1000)
myBootStrap(Dataset2$V1, 10000)

myBootStrap(Dataset3$V1, 100)
myBootStrap(Dataset3$V1, 1000)
myBootStrap(Dataset3$V1, 10000)

#(Problem 3) Part B

par(mfrow = c(1,2))
#bernoulli since 1s, 0s, only
hist(Dataset1$V1, main= "Dataset1", xlab = "X")
Data1Unique <- unique(Dataset1$V1)
print(Data1Unique)


hist(Dataset2$V1, main= "Dataset2", xlab = "X")
Data2Unique <- unique(Dataset2$V1)
print(Data2Unique)


###############################################################
#                      (Problem 4)                            #
###############################################################

inCircle = function(n){
  
  X = runif(n, 0 , 1)
  #print(X)

  Y = runif(n, 0 , 1)
  #print(Y)

  XY <- data.frame(X,Y)
  ##print(XY)

  
  Check = sapply(1:n,function(i){
  
  Dsq = ((.5-X[i])^2) + ((.5-Y[i])^2)  
  D = sqrt(Dsq)
  insideC = D<.5
  return (insideC)    
  })
    
  ICircle = subset(Check ,Check == TRUE)
  Tpoints = length(ICircle) / n
  
  print(length(ICircle))
  print(4*Tpoints)
} #end of inCircle

set.seed(9999)

inCircle(1000)
inCircle(10000)
inCircle(100000)

