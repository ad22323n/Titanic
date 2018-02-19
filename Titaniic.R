library(caret)
library(ggplot2)
library(corrplot)
library(rpart)
library(rpart.plot)
library(e1071)
library(mlbench)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

training1<-read.csv("file:///C:/Users/User/Desktop/Titanic/train.csv")
testing1<-read.csv("file:///C:/Users/User/Desktop/Titanic/test.csv")

View(training1)
summary(training1)
summary(testing1$Parch)
str(training1)
str(testing1)
sum(is.na(training1))
View(testing1)
#++++++++++++++++++++++++++++++++change transform attribute from the training set

training1$Name<-as.character(training1$Name)
training1$Survived<-as.factor(training1$Survived)
training1$SibSp<-as.factor(training1$SibSp)
training1$Parch<-as.factor(training1$Parch)
training1$Pclass<-as.factor(training1$Pclass)
#Dealing with NA 177

for (i in 1:ncol(training1)) {
  training1[is.na(training1[,i]),i]<-mean(training1[,i],na.rm = TRUE)
}


summary(training1)

ggplot(training1)+ geom_boxplot(aes(x=Embarked, y=Fare, fill= Pclass))

ggplot(training1, aes(x=Embarked, y=Fare))+geom_bar( stat = "identity")+geom_hline(yintercept = 80)
#Let's impute Embarkation missing values
training1$Embarked[c(62,830)]<-"C"

View(training1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++SELECT ATTRIBUTE FOR TRAINING AND CREATINT TITLE COL++++++++++++++++++++++
training1
str(training1)

training1$Title[grep(pattern = "Mr\\.", x=training1$Name,value = FALSE )] <- "Mr"  
training1$Title[grep(pattern = "Mrs\\.", x=training1$Name, value = FALSE)]<-"Mrs"
training1$Title[grep(pattern = "Master\\.",x=training1$Name, value = FALSE)]<-"Master"
training1$Title[grep(pattern = "Miss\\.", x=training1$Name, value = FALSE)]<-"Miss"
#myData$Title[grep(pattern = "NA//", x=training1$Title, value = FALSE)]<-"NONE"
sum(is.na(training1$Title))

training1$Title[which(is.na(training1$Title))]<-"other"

training1$Title<-as.factor(training1$Title)

#FAMILY SIZE
training1$myFamSize<-as.numeric(training1$SibSp)+as.numeric(training1$Parch)+1

#NUMBER PER FAMILY

training1$FamNumber[training1$myFamSize==1]<-"Single"
training1$FamNumber[training1$myFamSize>1 && training1$myFamSize<5]<-"medium"
training1$FamNumber[training1$myFamSize>=5]<-"Large"

View(training1)

#+++++++++++++++++++++++++++++++++++++++++++++VISUALIZATION_+_+_+_++++++++++++++++++++++++
ggplot(training1, aes(x=Survived, fill = Survived))+
  geom_histogram(stat = "count")+ geom_label(stat='count',aes(label=..count..))
prop.table(table(training1$Survived))


ggplot(training1)+ geom_bar(aes(x=Pclass, fill=Survived))
prop.table(table(training1$Pclass))

#ggplot(CompleteData)+ geom_bar(aes(x=Survived, fill=Pclass))#+ facet_wrap(~Sex)

ggplot(training1)+ geom_bar(aes(x=Pclass, fill=Survived))+ facet_wrap(~Sex)
prop.table(table(training1$Sex))

ggplot(training1)+ geom_bar(aes(x=Sex, fill=Survived))


ggplot(training1) + geom_bar(aes(x=Title, fill= Survived))+facet_wrap(~Pclass)
prop.table(table(training1$Title))
#visualizing people who survived depending on sex, pclass
ggplot(training1)+geom_bar(aes(x=Sex, fill=Survived)) +facet_wrap(~Pclass)

ggplot(training1)+geom_histogram(aes(x=Age, fill=Survived))+facet_wrap(~Sex+Pclass)

mean(training1$Age)
summary(training1$Age)
#taking a closer look at titles' variable
boys<-training1[which(training1$Title=="Master"),]
summary(boys$Age)

misses<-training1[which(training1$Title=="Miss"),]
summary(misses$Age)

ggplot(boys)+geom_histogram(aes(x=Age, fill=Survived),binwidth = 5)+facet_wrap(~Pclass)
ggplot(misses)+geom_histogram(aes(x=Age, fill=Survived),binwidth = 5)+facet_wrap(~Pclass)


ggplot(training1)+geom_bar(aes(x=Title, fill=Survived))+facet_wrap(~Title+Pclass)
prop.table(table(training1$Title))

ggplot(training1)+geom_bar(aes(x=SibSp, fill=Survived))+facet_wrap(~Pclass+FamNumber)
prop.table(table(training1$FamNumber))
View(training1)
#Embarked
summary(training1$Embarked)

#training1$Embarked[c(62,830)]<-"S"
ggplot(training1)+geom_bar(aes(x=Embarked, fill=Survived))
ggplot(training1)+geom_bar(aes(x=Embarked, fill=Survived))+facet_wrap(~Pclass)

prop.table(table(training1$Embarked, training1$Survived, training1$Pclass))

prop.table(table(training1$Embarked))
prop.table(table(training1$myFamSize))
prop.table(table(training1$FamNumber))
prop.table(table(training1$SibSp))
prop.table(table(training1$Parch))

#AGE
hist(training1$Age, col = c("red","steelblue"), freq = F, main = "Age Distribution", xlab="Age")
rug(jitter(training1$Age), col = "darkgrey")
lines(density(training1$Age, na.rm = T), col = "green", lwd = 3)
box()

#+++++++++++++++++++++++++++++++++++++++++++CREATING A NEW TESTING DATA CALLED myData2+++++++++++++++++++++++++++++
View(testing1)

myData2<-testing1
summary(myData2)


for (i in 1:ncol(myData2)) {
  myData2[is.na(myData2[,i]),i]<-mean(myData2[,i],na.rm = TRUE)
}

summary(myData2)
str(myData2)

myData2$SibSp<-as.factor(myData2$SibSp)
myData2$Parch<-as.factor(myData2$Parch)
myData2$Pclass<-as.factor(myData2$Pclass)
myData2$Name<-as.character(myData2$Name)

#++++++++++++++++++++++++++++++++++++++++++++++FEATURE ENGINEERING (creating Title from "name")+++++++++++++++++++++
#TITLE
myData2$Title[grep(pattern = "Master\\.",x=myData2$Name, value = FALSE)]<-"Master"
myData2$Title[grep(pattern = "Miss\\.", x=myData2$Name, value = FALSE)]<-"Miss"
myData2$Title[grep(pattern = "Mr\\.", x=myData2$Name,value = FALSE )] <- "Mr"  
myData2$Title[grep(pattern = "Mrs\\.", x=myData2$Name, value = FALSE)]<-"Mrs"

myData2$Title[which(is.na(myData2$Title))]<-"other"

sum(is.na(myData2$Title))
myData2$Title<-as.factor(myData2$Title)

#FAMILY SIZE
myData2$myFamSize<-as.numeric(myData2$SibSp) + as.numeric(myData2$Parch) + 1
#FAMILY SIZE NUMBER

myData2$FamNumber[myData2$myFamSize==1]<-"Single"
myData2$FamNumber[myData2$myFamSize >1 && myData2$myFamSize <5]<-"medium"
myData2$FamNumber[myData2$myFamSize >=5]<-"Large"

View(myData2)

#+++++++++++++++++++++++++++++++++VISUALIZING TRAINING DATA (myData)+++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++LET MERGE OUR 2 DATASET; TESTINGSET(myData2)+TRAININGSET(myData)
CompleteData<-bind_rows(training1, myData2)


str(CompleteData)
summary(CompleteData)
sum(is.na(CompleteData))
View(CompleteData)

CompleteData$myFamSize<-as.factor(CompleteData$myFamSize)
CompleteData$FamNumber<-as.factor(CompleteData$FamNumber)
CompleteData$Embarked<-as.factor(CompleteData$Embarked)
CompleteData$myFamSize<-as.factor(CompleteData$myFamSize)
CompleteData$FamNumber<-as.factor(CompleteData$FamNumber)
CompleteData$Parch<-as.factor(CompleteData$Parch)
#Let's look for outlier
plot(CompleteData$Survived)
plot(CompleteData$Pclass)
plot(CompleteData$Sex)
plot(CompleteData$Age)
plot(CompleteData$SibSp)
plot(CompleteData$Parch)
plot(CompleteData$Fare) #Outlier after 300
plot(CompleteData$Embarked)


CompleteData$Fare[CompleteData$Fare>300]<-mean(CompleteData$Fare)


#++++++++++++++++++++++++Modeling THE FULL DATASET++++++++++++++++++++
#setting seed and making our 10fold cross validation
set.seed(2018)
intrain<-createDataPartition(CompleteData$Survived, p=0.75, list = FALSE)
completeTraining<-CompleteData[1:891,]
completeTesting<-CompleteData[892:1309,]

View(completeTesting)
#______________LET'S CREATE A TRAINING-TRAINING & TRAINING-TESTING WITH "myTraintrain"+++++++++++++++++__________________

ctrl<-trainControl(method = "repeatedcv", number = 10, repeats = 3) 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
completeTraining3<-completeTraining[,c("Survived","Pclass","SibSp", "Parch","Sex", "Title","Embarked", "myFamSize","FamNumber")]
rfTraining3<-train(Survived~., data = completeTraining3, method="rf", trControl=ctrl, preProcess = c("center","scale"))
rfTraining3 

svmTraining3<-train(Survived~., data = completeTraining3, method="svmLinear", trControl=ctrl, preProcess = c("center","scale"))
svmTraining3 #PPPgood

gbmTraining3<-train(Survived~., data = completeTraining3, method="gbm", trControl=ctrl, preProcess = c("center","scale"))
gbmTraining3 

c5Training3<-train(Survived~., data=completeTraining3, method="C5.0", trControl=ctrl, preProcess=c("center","scale"))
c5Training3 #Good

myFitF3<-train(Survived~., data=completeTraining3, method="rpart", trControl=myCtrl, preProcess=c("center", "scale") )
myFitF3

FinalResult3<-resamples(list(gbm3=gbmTraining3, svm3=svmTraining3,C5.3=c5Training3,rf3=rfTraining3, rpart3=myFitF3))
dotplot(FinalResult3)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

completeTraining5<-completeTraining[,c("Survived","Pclass","SibSp", "Parch","Sex", "Title","Embarked", "myFamSize")]
rfTraining5<-train(Survived~., data = completeTraining5, method="rf", trControl=ctrl, preProcess = c("center","scale"))
rfTraining5

svmTraining5<-train(Survived~., data = completeTraining5, method="svmLinear", trControl=ctrl, preProcess = c("center","scale"))
svmTraining5 #Pgood

gbmTraining5<-train(Survived~., data = completeTraining5, method="gbm", trControl=ctrl, preProcess = c("center","scale"))
gbmTraining5 #PPgood

c5Training5<-train(Survived~., data=completeTraining5, method="C5.0", trControl=ctrl, preProcess=c("center","scale"))
c5Training5

myFitF5<-train(Survived~., data=completeTraining5, method="rpart", trControl=myCtrl, preProcess=c("center", "scale") )
myFitF5


FinalResult5<-resamples(list( svm5=svmTraining5,gbm5=gbmTraining5,C5.5=c5Training5,rf5=rfTraining5, rpart5=myFitF5))
dotplot(FinalResult5)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
completeTraining2<-completeTraining[,c("Survived","Pclass","SibSp", "Parch","Sex", "Title","Embarked","FamNumber")]
rfTraining2<-train(Survived~., data = completeTraining3, method="rf", trControl=ctrl, preProcess = c("center","scale"))
rfTraining2 

svmTraining2<-train(Survived~., data = completeTraining3, method="svmLinear", trControl=ctrl, preProcess = c("center","scale"))
svmTraining2 #PPgood

gbmTraining2<-train(Survived~., data = completeTraining3, method="gbm", trControl=ctrl, preProcess = c("center","scale"))
gbmTraining2 #pGood

c5Training2<-train(Survived~., data=completeTraining2, method="C5.0", trControl=ctrl, preProcess=c("center","scale"))
c5Training2 #good

myFitF2<-train(Survived~., data=completeTraining2, method="rpart", trControl=myCtrl, preProcess=c("center", "scale") )
myFitF2

FinalResult2<-resamples(list(gbm2=gbmTraining2, svm2=svmTraining2,C5.2=c5Training2,rf2=rfTraining2, rpart2=myFitF2))
dotplot(FinalResult2)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
completeTraining1<-completeTraining[,c("Survived","Pclass","SibSp", "Parch","Sex", "Title","Embarked")]
rfTraining1<-train(Survived~., data = completeTraining3, method="rf", trControl=ctrl, preProcess = c("center","scale"))
rfTraining1

svmTraining1<-train(Survived~., data = completeTraining3, method="svmLinear", trControl=ctrl, preProcess = c("center","scale"))
svmTraining1 #pgood

gbmTraining1<-train(Survived~., data = completeTraining3, method="gbm", trControl=ctrl, preProcess = c("center","scale"))
gbmTraining1 

c5Training1<-train(Survived~., data=completeTraining3, method="C5.0", trControl=ctrl, preProcess=c("center","scale"))
c5Training1 #Good


FinalResult1<-resamples(list(gbm1=gbmTraining1, svm1=svmTraining1,C5.1=c5Training1,rf1=rfTraining1))
dotplot(FinalResult1)
 #+++++++++++++++++++++++++++++++++++++FINAL RESULT+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Result<-resamples(list(svm3=svmTraining3, gbm=gbmTraining5, svm2=svmTraining2, svm1=svmTraining1))
dotplot(Result)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++
#_#_#_#_#__#_#_#_#_#_#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#
#Classification Tree with rpart

#+++++++++++++++++++++++++++++VALIDATING THE MODEL ON THE TESTING SET(myTrainTest) FROM completeTraining 
myFirstPred1<-predict(svmTraining1, completeTesting)
myFirstPred1

#myData2$Parch <- training1[sample(1:nrow(training1),418, replace = FALSE),]$Parch
mySolution <- data.frame(PassengerId = completeTesting$PassengerId, Survived = myFirstPred1)
View(mySolution)
#++++++++++++++++++++++++++++++LET'S TRY TO DO A NEW DATA PARTITION WITH THE WHOLE DATASET
completeTesting$Survived<-as.factor(myFirstPred1)
AllData<-rbind(completeTraining, completeTesting)



set.seed(123)
myintrain<-createDataPartition(AllData$Survived, p=.80, list = FALSE)
Alltrain<-AllData[1:891,]
Alltest<-AllData[892:1309,]

cotrl<-trainControl(method = "repeatedcv", repeats = 3, number = 10)

AlltrainData<-Alltrain[,c("Survived","Pclass","SibSp", "Parch","Sex", "Title","Embarked")]
rfAll<-train(Survived~., data = AlltrainData, method="rf", trControl=cotrl, preProcess = c("center","scale"))
rfAll 

svmAll<-train(Survived~., data = AlltrainData, method="svmLinear", trControl=cotrl, preProcess = c("center","scale"))
svmAll #PPPgood

gbmAll<-train(Survived~., data = AlltrainData, method="gbm", trControl=cotrl, preProcess = c("center","scale"))
gbmAll

c5All<-train(Survived~., data=AlltrainData, method="C5.0", trControl=cotrl, preProcess=c("center","scale"))
c5All #Good

myFitAll<-train(Survived~., data=AlltrainData, method="rpart", trControl=cotrl, preProcess=c("center", "scale") )
myFitAll

FinalRe<-resamples(list(gbm3=gbmAll, svm3=svmAll,C5.3=c5All,rf3=rfAll, rpart3=myFitAll))
dotplot(FinalRe)




