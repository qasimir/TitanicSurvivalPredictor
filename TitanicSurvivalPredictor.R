
# get the data from the working directory
trainingdata = read.csv("TitanicSurvivalTrainingData.csv")
testingdata = read.csv("TitanicSurvivalTestData.csv")

nrow(trainingdata) # number of rows
ncol(trainingdata) # number of columns
dim(trainingdata) # the general case

# a table gets the number of occurances of a particular value
table(trainingdata$Survived) #get the number of peopl who survived and those who didn't
prop.table(table(trainingdata$Survived)) # table of the proportion of people who survived

#1st approximation: more likely that a given person died. Therefore, we will make the guess that everyone in the testing set dies
testingdata$Survived = rep(0,ncol(testingdata))

# we can export our 1st approximation like so:
firstApprox = data.frame(PassengerID = testingdata$PassengerId, Survived = testingdata$Survived) # create a data frame like so 
write.csv(firstApprox, file = "firstApproximation.csv", row.names = FALSE)

# moving on to the second approximation. This gives a 2d table of the number of femals and males who survived
table(trainingdata$Sex, trainingdata$Survived)

#giving the proportion of each sex who were likely to survive. the "1" here indicates that we are examining the proportions of the rows:
prop.table(table(trainingdata$Sex, trainingdata$Survived), 1)

# we now have a better approximation. Females were more likely to survive. Can update the prediction now
#set the survival entries where Sex == female is true, to be true. The other entries are already for survival are already set to 0, so we don't need to worry about them  
testingdata$Survived[testingdata$Sex == 'female'] = 1

#and export second approximation:
scndapprox = data.frame(passengerID = testingdata$PassengerId, Survived = testingdata$Survived)
write.csv(scndapprox, file = "secondApproximation.csv", row.names = FALSE)


