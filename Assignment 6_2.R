Assignment 6.2
1. Import the Titanic Dataset from the link Titanic Data Set.
   Perform the following:
 # Load packages
 library('ggplot2') # visualization
 library('ggthemes') # visualization
 library('scales') # visualization
 library('dplyr') # data manipulation
 library('mice') # imputation
 library('randomForest') # classification algorithm
 
 train <- read.csv("F:/AcadGild/Files/train.csv", stringsAsFactors = F, na.strings=c("","NA"))
 test  <- read.csv("F:/AcadGild/Files/test.csv", stringsAsFactors = F, na.strings=c("","NA"))
 
 # combine training & test data
 full  <- bind_rows(train, test) 
 
 # check data summary
 str(full)
 
 # Grab title from passenger names
 full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
 
 # Show title counts by sex
 table(full$Sex, full$Title)
 
 # Titles with very low cell counts to be combined to "rare" level
 rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
 
 # Also reassign mlle, ms, and mme accordingly
 full$Title[full$Title == 'Mlle']        <- 'Miss' 
 full$Title[full$Title == 'Ms']          <- 'Miss'
 full$Title[full$Title == 'Mme']         <- 'Mrs' 
 full$Title[full$Title %in% rare_title]  <- 'Rare Title'
 
 # Show title counts by sex again
 table(full$Sex, full$Title)
 
 # Finally, grab surname from passenger name
 full$Surname <- sapply(full$Name,  
                        function(x) strsplit(x, split = '[,.]')[[1]][1])
 
 # Create a family size variable including the passenger themselves
 full$Fsize <- full$SibSp + full$Parch + 1
 
 # Create a new variable that shows the family name and the family size
 full$Family <- paste(full$Surname, full$Fsize, sep = "_")
 
 # Missing values
 colSums(is.na(full))
 colSums(full=="")
 
 # We have a lot of missing data in the Age feature (263/1309)
 # Let's change the empty strings in Embarked to the first choice "C"
 full$Embarked[full$Embarked==""]="C"
 
 # Let's see how many features we can move to factors
 apply(full,2, function(x) length(unique(x))) 
 
 # Let's move the features Survived, Pclass, Sex, Embarked to be factors
 cols<-c("Survived","Pclass","Sex","Embarked")
 for (i in cols){
   full[,i] <- as.factor(full[,i])
 }
 
 # Now lets look on the structure of the full data set
 str(full)
 

 a. Is there any difference in fares by different class of tickets?
   Note - Show a boxplot displaying the distribution of fares by class
 
test_im<-full[1:1309]c("Pclass","Sex","Age","SibSp","Parch","Fare","Title")]
test_im

# Survival as a function of Pclass: (rows taken 15 only for visibility of graph)
ggplot(data = test_im[1:10,],aes(x=Pclass,fill=Fare))+geom_bar(position="fill")+ylab("Frequency")
   
ggplot(full, aes(Pclass,fill = Fare))+
  geom_bar(colour = "red")+
  xlab("Pclass(0/1)") + 
  ylab("Total Passengers") +
  coord_flip()+
  ggtitle("Passenger class and Fare")



b. Is there any association with Passenger class and gender?
   Note - Show a stacked bar chart

ggplot(full, aes(Pclass,fill = Sex))+
  geom_bar(colour = "blue")+
  xlab("Pclass(0/1)") + 
  ylab("Total Passengers") +
  coord_flip()+
  ggtitle("Passenger class and gender")




