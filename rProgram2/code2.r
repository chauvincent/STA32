#Jan 16
#Code 2

X = c(67,63,65,71,72,68,60,59,67,66,75,74,69)
Y = c("F","F","F","M","M","M","F","F","M","F","M","M","F")
DataSet = data.frame(X,Y)

#Some other functions in R
#If you do head(DataSet), you will see the columns have names "X" and "Y". We can use the name() function to grab them.
names(DataSet)
#We can use this function to rename the headings
names(DataSet) = c("Height", "Gender")
head(DataSet)

#The which function which you have used in your homework already
#To combine two logical statements we use & (and), and | (or).
DataSet$Gender == "F" & DataSet$Height > 65   #Female and taller than 65
DataSet$Height < 62 | DataSet$Height > 70  #Height less than 62 or more than 70. 
#We can use these logical statements to subset data
#To get the indicies that these conditions holds, we use the which() function
which(DataSet$Gender == "F" & DataSet$Height > 65)
#To count how many data point satisfies this condition, we can use 
length(which(DataSet$Gender == "F" & DataSet$Height > 65))
#or
sum(DataSet$Gender == "F" & DataSet$Height > 65)
#R treats a TRUE as 1 and FALSE as 0 if you convert a logical vector to numeric

#You can order the data set by ascending or descending order
DataSet[order(DataSet$Height),] #ascending order
DataSet[order(DataSet$Height, decreasing=T),]  #descending order

#Setting up plots.
par(mfrow = c(1,1))  #Set up a plotting page with 1 plots
par(mfrow = c(2,1))  #Set up a plotting page with 2 rows and 1 column
hist(DataSet$Height, main = "Histogram of Height", xlab = "Height in Inches")
boxplot(DataSet$Height, main = "Boxplot of Height", ylab = "Height in Inches")
par(mfrow = c(1,2))  #Set up a plotting page with 1 row and 2 columns
hist(DataSet$Height, main = "Histogram of Height", xlab = "Height in Inches")
boxplot(DataSet$Height, main = "Boxplot of Height", ylab = "Height in Inches")
par(mfrow = c(1,1))  #Remember to reset the plotting window

#Sampling
sample(1:10, 5)    #Sample 5 numbers from 1 to 10 without replacement
sample(c("Cat", "Bird", "Dog"), 10, replace = T)  #Sample them with same probability
TESTING = sample(c("Cat", "Bird", "Dog"), 10, replace = T, prob = c(.45,.05,.50))  #Sample them with unequal probability
print(TESTING[1])
###################################################################################
#Making your own function
#Example
MyRange = function(X){ #This function finds the range of the input vector X
  n = length(X) #Setting the number of points total
  OrderedX = X[order(X)] #Reordering the X vector
  #Finding the min and max
  Min = OrderedX[1]  #This is ordered, so first element is smallest
  Max = OrderedX[n]
  Range = Max - Min  #Calculating the range
  return(Range) #Outputting the desired result 
}
MyRange(c(1,10))
MyRange(DataSet$Height)
#Notice unlike C, we do not need to prespecify the type of the variables
####################################################################################
#Loops
Do = "Add"
X = 2
Y = 4
if(Do == "Add"){
  Z = X+Y
}

#We can of course use the if statement in a function
Toyfun = function(X,Y,Do){
  if(Do == "Add"){
    Z = X+Y
    return(Z)
  }
  return(c(X,Y))  #else, return X and Y
}
Toyfun(2,4,"Add")
Toyfun(2,4,"NoAdd")

#Note: If we don't use return(Z), R will never save Z as a global variable
rm(Z)    #Remove Z from our workspace
Toyfun = function(X,Y,Do){
  if(Do == "Add"){
    Z = X+Y
    #return(Z)
  }
  #return(c(X,Y))
}
Toyfun(2,4,"Add")
Z           #You will get an error


#You can also add the else if case 
Toyfun = function(X,Y,Do){
  if(Do == "Add"){
    Z = X+Y
    return(Z)
  } else if(Do =="Subtract"){
    Z = X-Y
    return(Z)
  }else if(Do =="Multiply"){
    Z = X*Y
    return(Z)
  }else if(Do =="Penguin"){
    return(c("<('' )"))
  }
  return(c(X,Y))
}

Toyfun(2,4,"Add")
Toyfun(2,4,"Subtract")
Toyfun(2,4,"Penguin")

#####################################################################
#Sapply vs for loops
#To repeat some procedure many times, we can use the sapply() function
Results = sapply(1:100,function(i){
  Flip = sample(c("H","T"),1,replace = TRUE)
  if(Flip =="H"){return(TRUE)} else{return(FALSE)}
})
#This is equivalent to using a for loop.
Results1 = rep(NA,100) #Initializing the result vector
for(i in 1:length(Results1)){
  Flip = sample(c("H","T"),1,replace = TRUE)
  if(Flip =="H"){
    Results1[i] = TRUE
  } else{
    Results1[i] = FALSE
  }
} 

#We can also input a vector if we are to apply a function to each element of the vector
Y = c("F","F","F","M","M","M","F","F","M","F","M","M","F")
tmp = sapply(Y, function(i) {
  if(i == "F") return(1) else return(2)
})
#This is same as doing the following:
tmp1 = numeric(length(Y))   #Pre-set an empty numeric vector
for(i in 1:length(Y)){
  if(Y[i] == "F") tmp1[i] = 1 else tmp1[i] = 2
}

#We can of course put in a sapply function in our own defined function
HeadsOrTails = function(n){
  Results = sapply(1:n,function(i){
    Flip = sample(c("H","T"),1,replace = TRUE)
    if(Flip =="H"){return(TRUE)} else{return(FALSE)}
  })
  ProbOfHeads = sum(Results)/length(Results)
  return(ProbOfHeads)
}
HeadsOrTails(10)
HeadsOrTails(100)
HeadsOrTails(1000)
HeadsOrTails(10000)
HeadsOrTails(100000)

#################################################################
#Examples from handout
#Example 1
#Finds outliers, with an option to remove them from the dataset.
FindOutliers = function(Data,column = 1,Remove = FALSE){
  if(!is.numeric(Data[,column])) stop("Input is not numeric")
  X = Data[,column]
  Quant = fivenum(X)
  Q1 = Quant[2]
  Q3 = Quant[4]
  IQR = Q3-Q1
  LFence = Q1 - 1.5*IQR
  UFence = Q3 + 1.5*IQR
  Outlier = which(X < LFence | X > UFence)
  if(length(Outlier) != 0){
    print(paste("Observation", Outlier,"is an outlier."))
    if(Remove == TRUE){
      Data = Data[,-Outlier]
      print(paste("Observation",Outlier,"with value",X[Outlier],"is removed"))
      return(Data)
    }
    return(Outlier)
  }else{
    print("There are no outliers.")
  } 
}
FindOutliers(DataSet)
FindOutliers(DataSet, column = 2)

#Example 2
#Draws two cards from a 52 card deck, and checks to see if they are the same suit.
SuitCheck = function(n){
  Results = sapply(1:n,function(i){
    DeckValues = rep(c("H","S","C","D"),each = 13)
    Draw = sample(DeckValues,2,replace = FALSE)
    return(Draw[1] == Draw[2])
  })
  print(Results)
  Prob = sum(Results)/length(Results)
  return(Prob)
}

trials = c(10, 100, 1000, 10000, 100000)
sapply(trials, function(i) SuitCheck(i))
#Example 3
#Draws one card from a 52 card deck, calculates many probabilities.
#Let A be the event that there is an Ace draw.
#Let B be the event that a heart is draw.
#Calculates probabilities based on the above.
#SuitCheck = function(n){
#  Suit = rep(c("H","S","C","D"),each = 13)
#  Value = rep(c("A",2:10,"J","Q","K"),times = 4)
#  Deck = cbind(Suit,Value)
#  Results = sapply(1:n,function(i){
#    Draw = sample(1:52,1,replace = FALSE)
#    return(Deck[Draw,])
#  })
#  Results = t(Results)
#  P.A = sum(Results[,2]=="A")/nrow(Results)
#  P.B = sum(Results[,1] == "H")/nrow(Results)
#  P.AandB = sum(Results[,2] == "A" & Results[,1] == "H")/nrow(Results)
#  P.AorB = sum(Results[,2] == "A" | Results[,1] == "H")/nrow(Results)
#  AllResults = matrix(c(P.A,P.B,P.AandB,P.AorB),nrow = 1)
#  colnames(AllResults) = c("P(A)","P(B)","P(A&B)","P(AUB)")
#  return(AllResults)
#}
SuitCheck(50)
trials = c(10, 100, 150, 200, 1000)
sapply(trials, function(i) SuitCheck(i)) #Each time sapply will put your new result into a column
