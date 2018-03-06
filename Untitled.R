
install.packages("pdfLaTeX")

#load
tab1 <- read.csv('RProjects/шышщай/table5k.csv')
tab <- read.csv('RProjects/шышщай/ttab5k.csv')
tab2 <- tab1
tab1$X <- NULL
tab<-t(tab1)

tab1 <- read.csv('RProjects/шышщай/testable/table3k.csv')
gender <- c('male', 'male', 'male', 'female', 'female', 'male', 'female', 'male', 'male', 'female', 'male', 'male', 'female', 'male', 'male', 'male', 'male', 'male', 'male', 'male', 'male', 'female', 'male', 'male', 'male', 'female', 'male', 'male', 'female', 'female', 'male', 'female', 'male', 'female', 'female', 'female', 'female', 'female', 'female', 'female', 'male', 'female', 'female', 'male', 'male', 'male', 'male', 'female', 'female', 'male', 'female', 'male', 'female', 'female', 'male', 'female', 'male', 'female', 'female', 'male', 'female', 'male', 'female', 'male', 'female', 'male', 'female', 'male', 'male', 'female', 'female', 'male', 'female', 'female', 'male', 'female', 'male', 'male', 'male', 'male', 'female', 'female', 'female', 'male', 'male', 'female', 'male', 'male', 'female', 'male', 'female', 'female', 'male', 'male', 'male', 'female')
length(gender)

#testload
testu <- read.csv('RProjects/шышщай/testable/table3k_test.csv')
testu2 <- testu
testu2$X <- NULL
testu<-t(testu2)





testing<-testu
options(digits=9)
#print(tab[13,1])
#print(as.numeric(tab[13,1]))

library(ggplot2)
testing <- data.frame(testing)
tt1 <- data.frame(t(testing))
head(tt1)
str(tt1)
ggplot(data=tt1, aes(x=expression.63) )+ geom_()


for (i in 1:ncol(testu))
{
  for(j in 1:nrow(testu))
  {
    if(as.double(testu[j,i])>20) {
      testing[j,i]<-log2(as.double(testu[j,i]))
    }
  }
  print(c(testing[j,i],as.double(testu[j,i])))
}


tt <- data.frame(t(testing))
head(tt)
str(tt)
ggplot(data=tt, aes(x=expression.63) )+ geom_histogram(binwidth = 0.3)

testing <- normalizeQuantiles(testing)
#testing <- normalizeMedianAbsValues(testing)


#log
tabnorm<-tab
options(digits=12)
#print(tab[13,1])
#print(as.numeric(tab[13,1]))
for (i in 2:ncol(tab))
{
  for(j in 1:nrow(tab))
  {
    if(as.double(tab[j,i]>25)) {
      tabnorm[j,i]<-log2(as.double(tab[j,i]))
      
    }
  }
  print(i)
}

#normailze
tabnorm2<-tabnorm
tabnorm2$G <- NULL

library('limma')

#rg <- new("RGList", as.list(assayData(tabnorm2)))
r_n <- normalizeQuantiles(tabnorm2)
r_n1 <- normalizeCyclicLoess(tabnorm2)
r_n2 <- normalizeMedianAbsValues(tabnorm2)
r_n3 <- normalizeMedianValues(tabnorm2)

r_n3[,3]
tabnorm2[,3]
tabnorm_0 <- r_n
tabnorm_1 <- r_n1
tabnorm_2 <- r_n2
tabnorm_3 <- r_n3

#tabnorm_0 <- t(tabnorm_0)

#tab_0 <- t(tab)
tab_0 <- tab
tab_1 <- tab
tab_2 <- tab
tab_3 <- tab
for (i in 2:ncol(tab_0))
{
  for(j in 1:nrow(tab_0))
  {
      tab_0[j,i]<-tabnorm_0[j,i-1]
  }
  print(i)
}
for (i in 2:ncol(tab_1))
{
  for(j in 1:nrow(tab_1))
  {
    tab_1[j,i]<-tabnorm_1[j,i-1]
  }
  print(i)
}
for (i in 2:ncol(tab_2))
{
  for(j in 1:nrow(tab_2))
  {
    tab_2[j,i]<-tabnorm_2[j,i-1]
  }
  print(i)
}
for (i in 2:ncol(tab_3))
{
  for(j in 1:nrow(tab_3))
  {
    tab_3[j,i]<-tabnorm_3[j,i-1]
  }
  print(i)
}

#tabnorm_0 <- t(tabnorm_0)
#tab_0 <- t(tab_0)



write.csv(tabnorm, file = "data/tabtranslog.csv")

#test-train
head(tabnorm_2,96)
trainframe <- head(tabnorm_2,96)
t20<-gender

svm_fit <- train(y = as.factor(t20), x = data.frame(trainframe), method = "svmLinear", trControl = ctrl)
svm_fit <- train(y = as.factor(t20), x = data.frame(trainframe), method = "LogitBoost", trControl = ctrl,tuneGrid = data.frame(nIter = 5))
confusionMatrix(predict(svm_fit, data.frame(trainframe)),t20)


#train
train_indx <- sample(c(1:nrow(tab_2)), size = 65, replace = FALSE)
print(train_indx)
t20<-tab_2$G[train_indx]
frame20 <- tabnorm_2[train_indx,]
head(frame20)



#test
set.seed(1112)
test_indx <- sample(c(1:nrow(tab_2)), size = 31, replace = FALSE)
print(test_indx)
t15<-tab_2$G[test_indx]
frame15 <- tabnorm_2[test_indx,]
head(frame15)

library(caret)
ctrl <- trainControl(method="none", number = 1, repeats = 1) 
# Parameters of cross validation 
ctrl <- trainControl(
  method = "cv",
  number = 4, # 4-fold CV, you can tune this number
  classProbs = TRUE
)
str(t20)
y = as.factor(t20, labels = "G")
svm_fit <- train(y = as.factor(t20), x = data.frame(frame20), method = "svmLinear", trControl = ctrl)
svm_fit <- train(y = as.factor(t20), x = data.frame(frame20), method = "cforest", trControl = ctrl)
svm_fit <- train(y = t20, x = data.frame(frame20), method = "LogitBoost", trControl = ctrl,tuneGrid = data.frame(nIter = 8))

confusionMatrix(predict(svm_fit, data.frame(frame15)),t15)
print(sum(predict(svm_fit, data.frame(frame15)) == t15)/length(t15))

print(testing[1,])

# Generate predictions for testing data
predictions <- predict(svm_fit, data.frame(testing),type="prob")
submission <- data.frame(y = predictions)
submission[3,1]
gender <- c()
length(submission$y.female)
for(i in 1:length(submission$y.female)){
  if(submission[i,1] >= 0.50) {
    gender <- c(gender,"female")
  }
  else {
    gender <- c(gender,"male")
  }
}
gender
submission <- data.frame(gender)
head(submission)

# Save into a file
write.csv(x = submission,
          file = 'testing_predictions.csv', row.names = TRUE)
