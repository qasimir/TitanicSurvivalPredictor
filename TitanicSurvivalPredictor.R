
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

# 3rd approximation: factoring age into it as well 
summary(trainingdata$Age)

# make a variable which checks to see whether or not the passenger was a child
trainingdata$Child = 0
trainingdata$Child[trainingdata$Age < 18] = 1

# Now use an aggregate. Target variable on the LHS of the tilde, and the categories on the right.
# This subsets the frame for all of the different combinations of Sex and child. The result of this, is the sum of all the categories of people who survived.
aggregate(Survived ~ Child + Sex, data = trainingdata, FUN = sum)

# number of people in each subset, regardless of whether or not they survived:
aggregate(Survived ~ Child + Sex, data = trainingdata, FUN = length)

# now we want the proportion of people who survived, in each subset:
aggregate(Survived ~ Child + Sex, data = trainingdata, FUN = function(x) {sum(x)/length(x)})

# Children were more likely to survive if they were female, but that does not change our prediction, as the finer distinction does not add anything to the hypothesis.

# let us now try for fare types. First, seperate them into distinct groups:
trainingdata$FareType = ">30"
trainingdata$FareType[trainingdata$Fare < 30] = "20-30"
trainingdata$FareType[trainingdata$Fare < 20] = "10-20"
trainingdata$FareType[trainingdata$Fare < 10] = "<10"

# now see if there is anything interesting:
aggregate(Survived ~ FareType + Pclass + Sex, data = trainingdata, FUN = function(x) {sum(x)/length(x)})

# we can see from this that 3rd class females in the greater than 20 dollar fare range did worse off. Update for third prediction:
testingdata$Survived[testingdata$Sex == 'female' & testingdata$Fare >=20 & testingdata$Pclass == 3] = 0
thirdapprox = data.frame(passengerID = testingdata$PassengerId, Survived = testingdata$Survived)
write.csv(scndapprox, file = "ThirdApproximation.csv", row.names = FALSE)

