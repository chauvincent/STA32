#############################################
### Author: Chau, Vincent (SID:998947424) ###
### STA 32 Program 1, DUE JAN 17          ###

# Read file
cats <- read.table("~/Desktop/rPrograms/cats.txt", header=T, quote="\"")

# Show data
cats

#############################################
##       PROBLEM (A) FOR ALL CATS          ##

mean(cats$Bwt) # mean for all cats bodyweight

median(cats$Bwt) # median for all cats bodyweight

var(cats$Bwt) # variance for all cats bodyweight

############################################## 
##      FOR FEMALE CATS (PROBLEM B)         ##

femaleCats <- subset(cats, cats$Sex == "F")

mean(femaleCats$Bwt) # mean for female cats

sd(femaleCats$Bwt)  # Standard dev for female cats

##############################################
##      FOR MALE CATS (PROBLEM B)           ##

maleCats <- subset(cats, cats$Sex == "M")

mean(maleCats$Bwt) # mean for male cats

sd(maleCats$Bwt) # standard deviation for male cats

##############################################
##  HISTOGRAM for FEMALE CATS (PROBLEM C)   ##

hist(femaleCats$Bwt, main = "Histogram for Female Cats' Body Weight", ylab = "Frequency", xlab = "Weight (in kg")


##############################################
##  HISTOGRAM for MALE CATS   (PROBLEM C)   ##

hist(maleCats$Bwt, main = "Historgram for Male Cats's Body Weight", ylab = "Frequency", xlab = "Weight (in kg)")

##########################################################
##  Proportion: male cats >= 2.5 weight(kg) (PROBLEM D) ##

maleCatsOver2.5 <- which(maleCats$Bwt >= 2.5)

length(maleCatsOver2.5)/length(maleCats$Bwt)


###########################################################
## Proportion: female cats >= 2.5 weight(kg) (PROBLEM D) ##

femaleCatsOver2.5 <- which(femaleCats$Bwt >= 2.5)

length(femaleCatsOver2.5)/length(femaleCats$Bwt) 

############################################################

