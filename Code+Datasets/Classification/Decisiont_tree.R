library(rpart)
library(numDeriv)
library(mlbench)
library(dplyr)
library(caret)
library(e1071)
library(cli)
library(C50)
library(rpart.plot)
library(rattle)
# Load the data and remove NAs
mydata <-read.table(file="meandataset5.csv",header=T,sep=",",na.strings =" ")
attach(mydata)

mydata <- na.omit(mydata)
# Inspect the data
sample_n(mydata, 3)

mydata$level<-as.factor(mydata$level)
table(mydata$level)
mydata<-mydata[,3:7]
str(mydata)
set.seed(123)
g<-runif(nrow(mydata))
mydata<-mydata[order(g),]
# Split the data into training and test set
set.seed(123)
training.samples <- mydata$level %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- mydata[training.samples, ]
test.data <- mydata[-training.samples, ]
med<-level[-training.samples ]

set.seed(123)
model1 <- C5.0(level ~ humidty + pm2.5 + pm10 + temp, method = "class", data = train.data,control=
                 C5.0Control(CF = 0.20, minCases = 15) )

par(xpd = NA) # Avoid clipping the text in some device

plot(model1,cex=1.8)
text(model1, digits = 3)
summary(model1)


model1 <- rpart(level ~ humidty + pm2.5 + pm10 + temp, method = "class", data = train.data,
                control=rpart.control(minsplit = 3, minbucket = 2,maxdepth = 3,cp = 0.01) )

# Plot the trees
# model1 <- rpart(level ~ humidty + pm2.5 + pm10 + temp, method = "class", data = train.data)
#########################################
parse_tree(mydata,model1)
rpart.rules(model1)
rpart.rules(model1, roundint=FALSE, clip.facs=TRUE)
rattle::asRules(model1, TRUE)
###############################################
par(xpd = NA) # Avoid clipping the text in some device

plot(model1,cex=1.8)
text(model1, digits = 3)
summary(model1)

#rpart.plot(model1,type=1,cex=1, main="Number of cases per class",leaf.round=0,tweak=0.8)
#clip.right.labs = FALSE
#gap=0,
rpart.plot(under = TRUE, branch = .4,model1,cex=1.3, main="",type = 1, fallen.leaves = T, 
           extra = 0,tweak=0.7,legend.cex=1.3)
rpart.rules(model1)
fancyRpartPlot(under = TRUE,model1,type = 1, palettes=c("Greys", "Oranges"), tweak = 1.2)
prp(model1, box.palette = "Blues", tweak = 1.2)
rxAddInheritance
prp(model1)
fancyRpartPlot(model1,cex=0.7,gap=0)

varlen=3
predicted.classes <- model1 %>% 
  predict(test.data,type = "class")
head(predicted.classes)

confusionMatrix(predicted.classes,test.data$level,positive="1")
mean(predicted.classes == test.data$level)
#############################################################################
TClass <- factor(c("verylow","verylow","verylow","verylow","verylow","low","low","low","low","low","medium","medium","medium","medium","medium",3,3,3,3,3,4,4,4,4,4))
PClass <- factor(c(0,1,2,3,4,0,1,2,3,4,0,1,2,3,4,0,1,2,3,4,0,1,2,3,4))
Y   <- c( 0  , 0  ,     0    ,   0  ,     0,
          0  , 7 ,     0   ,     0    ,   3,
          0 ,  0   ,   0    ,    0   ,    0,
          0 ,  0 ,     0   ,     0   ,    0,
          0  ,30   ,   7     ,   1   ,  235)
df <- data.frame(PClass, TClass, Y)

#############################################################################

evaluation <- function(model, mydata, atype) {
  cat("\nConfusion matrix:\n")
  xtab = table(predicted.classes,test.data$level)
  print(xtab)
  accuracy = sum(predicted.classes == mydata$level)/length(mydata$level)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}
evaluation(predicted.classes, test.data, "class")
xtab = table(predicted.classes,test.data$level)

xtab[,1]<-xtab[,5]


accuracy = sum(predicted.classes == mydata$level)/length(mydata$level)
precision = xtab[5,5]/sum(xtab[,5])

recall = xtab[2,2]/sum(xtab[2,])

f = 2 * (precision * recall) / (precision + recall)
cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))


########################################################################################

TClass <- factor(c(0,0,0,0,0,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4))
PClass <- factor(c(0,1,2,3,4,0,1,2,3,4,0,1,2,3,4,0,1,2,3,4,0,1,2,3,4))
Y   <- c( 0  , 0  ,     0    ,   0  ,     0,
          0  , 7 ,     0   ,     0    ,   3,
          0 ,  0   ,   0    ,    0   ,    0,
          0 ,  0 ,     0   ,     0   ,    0,
          0  ,30   ,   7     ,   1   ,  235)
df <- data.frame(PClass, TClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")


rmse<-mean((predicted.classes - med)^2)
# Fit the model on the training set
set.seed(123)
model2 <- train(
  level ~., data = train.data, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Plot model accuracy vs different values of
# cp (complexity parameter)
plot(model2,lab.cex=2)
model2$bestTune
par(xpd = NA) # Avoid clipping the text in some device
plot(model2$finalModel)
text(model2$finalModel,  digits = 3)
confusionMatrix(predicted.classes,test.data$level)
##################################################
#improvement
#################################
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

