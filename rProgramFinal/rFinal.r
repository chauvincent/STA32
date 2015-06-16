#  Vincent Chau   #
#  SID: 998947424 #
#  Final Program  #
# # # # # # # # # #

# (Problem 1)
Starcraft = function(n){
  Gender = sample(c("Male", "Female"),n, replace=TRUE, prob = c(0.65,0.35))
  
  Race = sapply(1:n,function(i){
  
    if(Gender[i] == "Male"){
      
     return(sample(c("Z", "P", "T"),1,prob = c(0.4,0.30,0.30)))
    
    }else if(Gender[i] == "Female"){
    
     return(sample(c("Z","P","T"),1,prob = c(0.20,0.35,0.45)))
    }
    }) #end race/sapply
    P.Z = sum(Race == "Z")/n
    P.T = sum(Race == "T")/n
    P.P = sum(Race == "P")/n
    P.MgZ = sum(Race == "Z" & Gender =="Male")/sum(Race == "Z")
    P.McGPc = sum(Race != "Z" & Gender !="Male")/sum(Race != "P")
    P.ZandMc = sum(Race == "Z" & Gender != "Male")/n
    P.TorM = sum(Race == "T" | Gender == "M")/n
    
    Results = matrix(c(P.Z,P.T,P.P,P.MgZ,P.McGPc,P.ZandMc,P.TorM),nrow=1)
    colnames(Results) = c("P(Z)","P(T)", "P(P)", "P(M|Z)","P(Mc|Pc)", "P(Z&Mc)", "P(TorM)")
    #print(Results)
    return(Results)

} # end starcraft
  
set.seed(1001)
Starcraft(100)
Starcraft(100000)

# (Problem 2)

library(MASS)
X = quine$Days

BS.It = function(X,B){
  n = length(X)
  BSMM = sapply(1:B,function(i){
    newSample = sample(X,n,replace=TRUE)
    meanOfNew = mean(newSample)
    medianOfNew = median(newSample)
    maxOfNew = which.max(newSample)
    quantityOfNew = sum(range(newSample))/sd(newSample)
    return(c(meanOfNew, medianOfNew, maxOfNew, quantityOfNew))  
})
  BSMM = t(BSMM)
  return(BSMM)
}

set.seed(1001)
BS.It(X,100)

# (Problem 3)
NormalTemp <- read.table("~/Desktop/winterQuarter14/sta32/rPrograms/rProgramFinal/NormalTemp.txt", quote="\"")

myHypoTest = function(X,Y, Alpha, Side){
  
  
  
}






# Part A
par(mfrow = c(1,2))
hist(NormalTemp$V1[which(NormalTemp$V2==1)], main = "Histogram for male body temperatures", xlab = "Body temperatre in degrees Fahrenheit")
hist(NormalTemp$V1[which(NormalTemp$V2==2)], main = "Histogram for female body temperatures", xlab = "Body temperatre in degrees Fahrenheit")

# 



