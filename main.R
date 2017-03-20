library(randomForest)
library(caret)


digit_orig <- read.csv("train.csv")
test_orig <- read.csv("test.csv")

digit = digit_orig[,-1]
labels = digit_orig$label
comb = rbind(digit,test)

#PCA

comb = comb[, apply(comb,2,var) != 0]
train.pca = comb[1:nrow(digit),]
test = comb[42001:nrow(comb),]


prin_comp = prcomp(train.pca, scale. = T)
prop_varex = ((prin_comp$sdev)^2)/sum((prin_comp$sdev)^2)
train.data = data.frame(label = labels,predict(prin_comp, train.pca))
test.data = data.frame(predict(prin_comp, test))

plot(prop_varex)
plot(cumsum(prop_varex))
biplot(prin_comp, scale = 0)
head(which(cumsum(prop_varex) > 0.98))
train <- train.data[,1:259]

test_pca <- test.data[,1:259]

train$label = labels

#SVM
svm_model <- svm(label~.,data = train)
pred = predict(rf1, newdata = test_pca)
head(pred)


#Benchmark
rows <- sample(1:nrow(digit), 10000)
labels <- as.factor(digit[rows,1])
train <- digit[rows,]

rf <- randomForest(train, labels, xtest = test,ntree = 25)
pred = rf$test$predicted


