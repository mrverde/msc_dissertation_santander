library(caret)

'%ni%' <- Negate('%in%') 

train <- read.csv("train.csv", sep=",", header = TRUE, dec=".")
test <- read.csv("test.csv", sep=",", header = TRUE, dec=".")

set.seed(1117)

vars <- c("imp_trans_var37_ult1", "num_var42", "saldo_var13", "saldo_var25", "ind_var30_0", 
          "saldo_medio_var13_corto_ult3", "imp_op_var40_efect_ult1", "saldo_var13_corto", 
          "var3", "ind_var13_largo")

train <- upSample(x=train[,colnames(train) %ni% "TARGET"], y=as.factor(train$TARGET), yname="TARGET")
TARGET <- train$TARGET
train <- train[,which(names(train) %in% vars)]
train <- data.frame(scale(train))

testID <- test$ID
test <- test[,which(names(test) %in% vars)]
test <- data.frame(scale(test))

fit <- knn3(as.matrix(train), as.factor(TARGET), k=5, prob = TRUE)  
predictions <- predict(fit, as.matrix(test), type="class")
table(predictions)

write.csv(data.frame(ID=testID, TARGET=predictions), 
          file = "KNN_upsampling_10vars_5k.csv",
          row.names=FALSE)