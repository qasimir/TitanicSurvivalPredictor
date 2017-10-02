
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

#now we use descision trees to automate the proccess of splitting the data into segments.
# import rpart (recursive partitioning and Regression Trees)
library(rpart)

# these will help with the visualisation
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("RColorBrewer")
#library(rattle)
#library(rpart.plot)
#library(RColorBrewer)

# once imported, we can use it in a similar manner to the aggregate function, but it does it with automation
fit = rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
            data = trainingdata,
            method = "class")

#we can plot the tree, and
plot(fit)
text(fit)

# update the prediction:
Prediction = predict(fit, testingdata, type = "class")
submit = data.frame(PassengerId = testingdata$PassengerId, Survived = Prediction)
write.csv(submit, file = "fourthOrderPrediction.csv", row.names = FALSE)

# lets see what feature engineering we can do
# first, lets combine the testing and training data, after tidying up a bit:
trainingdata$Child = NULL
trainingdata$FareType = NULL
testingdata$Survived = NA
combined = rbind(trainingdata, testingdata)

# change the names from factors to characters:
combined$Name = as.character(combined$Name)

# the titles are comprised of surnames, then titles, then first names, delineated by a comma
strsplit(combined$Name[1], split="[,.]")[[1]][2] # as an example of the first name

# for all names:
combined$Title = sapply(combined$Name, FUN = function(x) {strsplit(x, split="[,.]")[[1]][2]})

# strip off all of the leading whitespace:
combined$Title = sub(" ","", combined$Title)

# there are some redundancies in the titles, so we can reduce them:
# madame, and madmoiselle:
combined$Title[combined$Title %in% c("Mme","Mlle")] = "Mlle"

# male honourific
combined$Title[combined$Title %in% c("Capt","Don","Jonkheer","Major","Col")] = "Sir"

#female honourific
combined$Title[combined$Title %in% c("Dona","Lady","the Countess")] = "Lady"

#add a new feature, family size:
combined$FamilySize = combined$SibSp + combined$Parch + 1

#we can group people according to families, as needing to search for family members might have been a deciding factor as to whether an individual boarded a life raft
#get the surnames
combined$Surname = sapply(combined$Name, FUN=function(x){strsplit(x,split="[,.]")[[1]][1]})

#assign every person a family ID:
combined$FamilyID = paste(as.character(combined$FamilySize),combined$Surname, sep = "")

#we want to separate the singles and pairs from the list of families, so we give them their own designation
combined$FamilyID[combined$FamilySize <=2 ] = "Single/Pairs"

table(combined$FamilyID)

#looking at the table of families, there are some who have misreported their family sizes. Will need to clean this up
famIDs = data.frame(table(combined$FamilyID))
famIDs = famIDs[famIDs$Freq <= 2,]
combined$FamilyID[combined$FamilyID %in% famIDs$Var1] = "Single/Pairs"
combined$FamilyID = as.factor(combined$FamilyID)

#goodie, now that we have got a list of people who are part of a family, we can split it apart, and do some predictions on the new variables
trainingdata2 = combined[1:nrow(trainingdata),]
testingdata2 = combined[(nrow(trainingdata)+1):nrow(combined),]

# now grow a new descision tree with the updated info
fit = rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize,
            data = trainingdata2,
            method = "class")

# and make a fifth order prediction, based on the new engineered variables:
Prediction = predict(fit, testingdata2, type = "class")
submit = data.frame(PassengerId = testingdata2$PassengerId, Survived = Prediction)
write.csv(submit, file = "FifthOrderPrediction.csv", row.names = FALSE)


#part 6: random forests
#download and install random forest
install.packages('randomForest')
library(randomForest)


# first problem, is empty spaces in the data. Especially for age. rpart cannot handle this
# We can fill this in with an Agefit, with the method being anova, as the age data is continuous:
Agefit = rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title,
               data = combined[!is.na(combined$Age),],
               method = "anova")
               
combined$Age[is.na(combined$Age)] = predict(Agefit, combined[is.na(combined$Age),])

#Embarked and Fare also have some blanks

#Southampton is the most common embarkment point, so we will put these into Southampton
combined$Embarked[combined$Embarked == ""] = "S"
combined$Fare[is.na(combined$Fare)] = median(combined$Fare, na.rm = TRUE)

#our dataframe is now cleaned of blanks. we still have too many factors in Family ID to run this, however. Reducing the number of families: 

combined$FamilyID2 = as.character(combined$FamilyID)
combined$FamilyID2[combined$FamilySize <= 3] = 'SmallFamily'
combined$FamilyID2 = factor(combined$FamilyID2)

# Try converting titles to factors
combined$Title = as.factor(combined$Title)

#Now, we split the data again:
trainingdata2 = combined[1:nrow(trainingdata),]
testingdata2 = combined[(nrow(trainingdata)+1):nrow(combined),]

#set a seed for the random forest
set.seed(415)

fit = randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                   data=trainingdata2,
                   importance = TRUE, 
                   ntree=2000)

# look at some of the metrics of the fit
varImpPlot(fit)

# there are also "conditional inference trees" shown below. 
# They use statistical metrics to determine the nodes, and can handle more factors than random trees

install.packages("party")
library(party)

fit = cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
              data=trainingdata2,
              controls=cforest_unbiased(ntree=2000,mtry=3))

Prediction = predict(fit, testingdata2, OOB=TRUE, type = "response")
submit = data.frame(PassengerId = testingdata$PassengerId, Survived = Prediction)
write.csv(submit, file = "SixthOrderPrediction.csv", row.names = FALSE)


