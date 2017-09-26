
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

