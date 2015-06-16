X3 <- read.table("~/Desktop/winterQuarter14/sta32/rPrograms/rProgram4/Dataset3.txt", quote="\"")
View(X3)

# (PROBLEM 1) PART A.

hist(X3[,1], main = "Annual Numbers of Lynx Trappings", xlab = "Years Since 1821 to 1934", ylab = "Annual Number of Lynx Tappings in Canada") 


# (PROBLEM 1) PART B.
PartB = function(X,n,N){
  
  sampleMean = sapply(1:N,function(i){
    newSample = X[sample(n,replace = TRUE),]
    meanOfNew = mean(newSample)  
    return(meanOfNew)
  }) ##end of sapply and bootstrap mean
  Sample = (sampleMean)
  return(Sample)
}
par(mfrow = c(1,3))
H1 <- PartB(X3, 30, 50)
H2 <- PartB(X3, 30, 100)
H3 <- PartB(X3, 30, 1000)
hist(H1, main = "Annual Numbers of Lynx Trappings", xlab = "Years Since 1821 to 1934", ylab = "Annual Number of Lynx Tappings in Canada") 
hist(H2, main = "Annual Numbers of Lynx Trappings", xlab = "Years Since 1821 to 1934", ylab = "Annual Number of Lynx Tappings in Canada") 
hist(H3, main = "Annual Numbers of Lynx Trappings", xlab = "Years Since 1821 to 1934", ylab = "Annual Number of Lynx Tappings in Canada")

# (PROBLEM 2) PART A.
Problem2 = function(Alpha, n, N, Mu, Sigma){
  
  Sample = rnorm(n,mean = Mu, sd = Sigma)
  {
    
  }
  
  
}



