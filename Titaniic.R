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
library(dplyr)


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

metric<-"Accuracy"
mtry<-sqrt(ncol(completeTraining))
tunegrid <- expand.grid(.mtry=mtry)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
completeTraining3<-completeTraining[,c("Survived","Pclass","SibSp", "Parch","Sex", "Title","Embarked", "myFamSize","FamNumber")]

rfTraining3<-train(Survived~., data = completeTraining3, method="rf", trControl=ctrl, preProcess = c("center","scale"),metric=metric,
                   tuneGrid=tunegrid)
rfTraining3 



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

completeTraining5<-completeTraining[,c("Survived","Pclass","SibSp", "Parch","Sex", "Title","Embarked", "myFamSize","FamNumber")]

rfTraining5<-train(Survived~., data = completeTraining5, method="rf", trControl=ctrl, preProcess = c("center","scale"),
                   metric=metric,
                   tuneGrid=tunegrid)
rfTraining5


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
completeTraining2<-completeTraining[,c("Survived","Pclass","SibSp", "Parch","Sex", "Title","Embarked","FamNumber", "Age")]
rfTraining2<-train(Survived~., data = completeTraining3, method="rf", trControl=ctrl, preProcess = c("center","scale")
                   ,metric=metric,
                   tuneGrid=tunegrid)
rfTraining2 


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
completeTraining1<-completeTraining[,c("Survived","Pclass","myFamSize","Sex", "Title","Embarked", "Age", "SibSp")]
rfTraining1<-train(Survived~., data = completeTraining3, method="rf", trControl=ctrl, preProcess = c("center","scale")
                   ,metric=metric,
                   tuneGrid=tunegrid)
rfTraining1


 #+++++++++++++++++++++++++++++++++++++FINAL RESULT+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

completeTraining6<-completeTraining[,c("Survived","Pclass","Fare", "Parch","Sex", "Title","Embarked","Age")]

rfTraining6<-train(Survived~., data = completeTraining6, method="rf", trControl=ctrl, preProcess = c("center","scale")
                   ,metric=metric,
                   tuneGrid=tunegrid)
rfTraining6
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

scales <- list(x=list(relation="free"), y=list(relation="free")) 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Result<-resamples(list(rf1=rfTraining1, rf2=rfTraining2,rf5=rfTraining5,rf6=rfTraining6, rf3=rfTraining3))
dotplot(Result, scales=scales)
bwplot(Result, scales=scales)
dif2<-diff(Result)
summary(dif2)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++MAKING PREDICTIION  
myFirstPred1<-predict(rfTraining6, completeTesting)
myFirstPred1

mySolution <- data.frame(PassengerId = completeTesting$PassengerId, Survived = myFirstPred1)
View(mySolution)

write.csv(mySolution, file = "TitanicPred1.csv",row.names = FALSE)



