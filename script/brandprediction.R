library(readr)
library(ggplot2)
library(caret)
library(C50)
# 0 = Acer
# 1 = Sony

# Load the data ####
complete <- read_csv('data/CompleteResponses.csv')
incomplete <- read_csv('data/SurveyIncomplete.csv')

# Remove duplicated observations (if there are any) ####
sum(duplicated(complete))
complete[!duplicated(complete),]
sum(duplicated(incomplete))
incomplete[!duplicated(incomplete),]

# Initial data exploration #####
summary(complete)
summary(incomplete)
str(complete)
str(incomplete)

# Data type conversion ####
incomplete$elevel <- as.factor(incomplete$elevel)
incomplete$zipcode <- as.factor(incomplete$zipcode)
incomplete$car <- as.factor(incomplete$car)
incomplete$brand<- as.factor(incomplete$brand)
complete$elevel <- as.factor(complete$elevel)
complete$zipcode <- as.factor(complete$zipcode)
complete$car <- as.factor(complete$car)
complete$brand<- as.factor(complete$brand)

# Feature selection investigation ####
chisq.test(complete$elevel,complete$brand)
chisq.test(complete$zipcode,complete$brand)
chisq.test(complete$car,complete$brand)
model <- glm(brand~ salary, data=complete, family=binomial(link="logit"))
anova(model, test="Chisq")
model <- glm(brand~ age, data=complete, family=binomial(link="logit"))
anova(model, test="Chisq")
model <- glm(brand~ credit, data=complete, family=binomial(link="logit"))
anova(model, test="Chisq")

# Boxplot comparison of brands ####
ggplot(complete, aes(y=salary,x=0, fill=brand)) +
  geom_boxplot() +
  xlab('') +
  ylab('Salary') +
  ggtitle('Boxplot of salary') +
  scale_fill_discrete(name = "Brand", labels = c("Acer", "Sony")) +
  theme(axis.text.x = element_blank())
ggplot(complete, aes(y=credit,x=0, fill=brand)) +
  geom_boxplot() +
  xlab('') +
  ylab('Credit') +
  ggtitle('Boxplot of credit') +
  scale_fill_discrete(name = "Brand", labels = c("Acer", "Sony")) +
  theme(axis.text.x = element_blank())
ggplot(complete, aes(y=age,x=0, fill=brand)) +
  geom_boxplot() +
  xlab('') +
  ylab('Age') +
  ggtitle('Boxplot of age') +
  scale_fill_discrete(name = "Brand", labels = c("Acer", "Sony")) +
  theme(axis.text.x = element_blank())

# Bar comparison of brands ####
ggplot(complete, aes(x=zipcode, fill = brand)) +
  geom_bar(position='dodge') +
  xlab('Zip Code') +
  ylab('Frequency') +
  ggtitle('Distribution of zip codes per brand') +
  scale_fill_discrete(name = "Brand", labels = c("Acer", "Sony"))
ggplot(complete, aes(x=elevel, fill = brand)) +
  geom_bar(position='dodge') +
  xlab('Education Level') +
  ylab('Frequency') +
  ggtitle('Distribution of education levels per brand') +
  scale_fill_discrete(name = "Brand", labels = c("Acer", "Sony"))
ggplot(complete, aes(x=car, fill = brand)) +
  geom_bar(position='dodge') +
  xlab('Car') +
  ylab('Frequency') +
  ggtitle('Distribution of cars per brand') +
  scale_fill_discrete(name = "Brand", labels = c("Acer", "Sony"))

# Comparison of Salary and age for each brand
ggplot(complete, aes(x=age,y=salary,col=brand)) + 
  scale_color_discrete(name = "Brand", labels = c("Acer", "Sony")) +
  geom_point() + 
  geom_smooth() +
  ggtitle('Salary vs Age per brand') +
  xlab('Age') +
  ylab('Salary')

# Check on class imbalance ####
brands <- table(complete$brand)
brand0 <- length(complete$brand[complete$brand == 0]) / length(complete$brand) * 100
brand1 <- length(complete$brand[complete$brand == 1]) / length(complete$brand) * 100
brand0
brand1
ggplot(complete,aes(brand, fill=brand)) +
  geom_bar(col='black') +
  scale_fill_discrete('Legend', labels=c('0'= 'Acer', '1'= 'Sony')) +
  labs(title='Brand distribution', colour= 'cadetblue4') +
  xlab('Brand') +
  ylab('Count') +
  scale_x_discrete(labels=c('0' = 'Acer', '1'= 'Sony')) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..count.., group=1 ), stat= 'count', vjust = -.5) + 
  ylim(0,7000)

# # Downsampling ####
CompleteResponsesTrue <-subset(complete,complete$brand==1)  #a new dataset of True
CompleteResponsesFalse <-subset(complete,complete$brand==0) #a new dataset of False
additionalTrueResponse <- nrow(CompleteResponsesTrue) - nrow(CompleteResponsesFalse ) #difference amongst raws
CompleteResponsesTrue <-CompleteResponsesTrue[-sample(1:nrow(CompleteResponsesTrue), additionalTrueResponse),]
CompleteResponses <- rbind(CompleteResponsesTrue,CompleteResponsesFalse)

# Separate training and testing sets ####
set.seed(107)
inTrain <- createDataPartition(y=CompleteResponses$brand,p=0.75,list=FALSE)
str(inTrain)
training <- CompleteResponses[inTrain,]
testing <- CompleteResponses[-inTrain,]
nrow(training)
nrow(testing)

# Change the brand names from 0,1 to Acer,Sony ####
levels(training$brand) <- c("Acer", "Sony")
levels(testing$brand) <- c("Acer", "Sony")

# Set the model training method to a 1 time repeated cross validation ####
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 1)

# Train the model ####
rfFit <- train(x = training[-7], y = training$brand, method = "rf", trControl = ctrl, tuneLength = 5)
Check variable importance ####
imp <- varImp(rfFit)
imp
plot(imp)

# Retrain the model with various methods using only Salary and Age ####
agesalarybrand <- training[,c(1,2,7)]
models <- c()
methodnames <- c('knn','rf','C5.0Tree','svmLinear','svmRadial')
for (i in methodnames){
model <- train(brand ~ ., data = agesalarybrand, method = i, trControl = ctrl,preProcess=c('center','scale'))
print(model)
predictions <- predict(model, newdata = testing)
acc <- postResample(predictions,testing$brand)
models <- cbind(models,acc)
}
colnames(models)<-methodnames
models
bestmethod <- which(models[1,] == max(models[1,]))
bestmodel <- train(brand ~ ., data = agesalarybrand, method = methodnames[bestmethod], trControl = ctrl,preProcess=c('center','scale'), tuneLength=20)
predictionsss <- predict(model, newdata = testing)
accur <- postResample(predictions,testing$brand)

# Check if the distributions of complete and incomplete are similar ####
a <- data.frame(group = "a", value = complete$salary)
b <- data.frame(group = "b", value = incomplete$salary)
c <- rbind(a,b)
ggplot(c, aes(x=group, y=value, fill=group)) + geom_boxplot()
a <- data.frame(group = "a", value = complete$credit)
b <- data.frame(group = "b", value = incomplete$credit)
c <- rbind(a,b)
ggplot(c, aes(x=group, y=value, fill=group)) + geom_boxplot()
a <- data.frame(group = "a", value = complete$age)
b <- data.frame(group = "b", value = incomplete$age)
c <- rbind(a,b)
ggplot(c, aes(x=group, y=value, fill=group)) + geom_boxplot()
a <- data.frame(group = "a", value = complete$elevel)
b <- data.frame(group = "b", value = incomplete$elevel)
c <- rbind(a,b)
ggplot(c, aes(x=value,fill=group)) +
  geom_bar(position='dodge')  +
  xlab('Education level') +
  ylab('Frequency') +
  ggtitle('Frequency of education level per dataset') +
  scale_fill_discrete(name = "Dataset", labels = c("Complete", "Incomplete"))
a <- data.frame(group = "a", value = complete$zipcode)
b <- data.frame(group = "b", value = incomplete$zipcode)
c <- rbind(a,b)
ggplot(c, aes(x=value,fill=group)) +
  geom_bar(position='dodge')  +
  xlab('Zip Code') +
  ylab('Frequency') +
  ggtitle('Frequency of zip codes  per dataset') +
  scale_fill_discrete(name = "Dataset", labels = c("Complete", "Incomplete"))
a <- data.frame(group = "a", value = complete$car)
b <- data.frame(group = "b", value = incomplete$car)
c <- rbind(a,b)
ggplot(c, aes(x=value,fill=group)) +
  geom_bar(position='dodge')  +
  xlab('Cars') +
  ylab('Frequency') +
  ggtitle('Frequency of cars  per dataset') +
  scale_fill_discrete(name = "Dataset", labels = c("Complete", "Incomplete"))

# Comparison of Salary and age for each brand in the incomplete dataset
ggplot(incomplete, aes(x=age,y=salary,col=brand)) + 
  scale_color_discrete(name = "Brand", labels = c("Acer", "Sony")) +
  geom_point() + 
  geom_smooth() +
  ggtitle('Salary vs Age per brand') +
  xlab('Age') +
  ylab('Salary')

# Do the predictions for the incomplete survey ####
brandpredictions <- predict(bestmodel, newdata = incomplete)
incomplete$brand <- brandpredictions
summary(brandpredictions)

# Check on the predictions ####
brandss <- table(incomplete$brand)
brand00 <- length(incomplete$brand[incomplete$brand == 'Acer']) / length(incomplete$brand) * 100
brand11 <- length(incomplete$brand[incomplete$brand == 'Sony']) / length(incomplete$brand) * 100
brand00
brand11
ggplot(incomplete,aes(brand, fill=brand)) +
  geom_bar(col='black') +
  scale_fill_discrete('Legend', labels=c('0'= 'Acer', '1'= 'Sony')) +
  labs(title='Brand distributiion', colour= 'cadetblue4') +
  scale_x_discrete(labels=c('0' = 'Acer', '1'= 'Sony')) +
  xlab('Brand') +
  ylab('Count') +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..count.., group=1 ), stat= 'count', vjust = -.5) + 
  ylim(0,3500)

#names(getModelInfo())
