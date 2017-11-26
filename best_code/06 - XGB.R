rm(list=ls())

library(xgboost)
library(Matrix)

setwd("/home/usuario/Documentos/R/TFM/")

set.seed(1117)

train <- read.csv("train.csv", sep=",", header = TRUE, dec=".")
test <- read.csv("test.csv", sep=",", header = TRUE, dec=".")


TARGET <- train$TARGET
testID <- test$ID
train <- train[,-which(names(train) %in% c("ID", "TARGET"))]
test <- test[,-which(names(test) %in% c("ID"))]

train$val_cero <- as.integer(rowSums(train == 0))
test$val_cero <- as.integer(rowSums(test == 0))

for (var in names(train)){
  if (length(unique(train[[var]])) == 1) {
    train[[var]] <- NULL
    test[[var]] <- NULL
  }
}

var_par <- combn(names(train), 2, simplify = F)
to_delete <- c()
for(par in var_par) {
  v1 <- par[1]
  v2 <- par[2]
  
  if (!(v1 %in% to_delete) & !(v2 %in% to_delete)){
    if (all(train[[v1]] == train[[v2]])){
      to_delete <- c(to_delete, v2)
    }
  }
}

set_vars <- setdiff(names(train), to_delete)
train <- train[,-which(names(train) %in% to_delete)]
test <- test[,-which(names(test) %in% to_delete)]

train$TARGET <- TARGET

train <- sparse.model.matrix(TARGET ~ ., data = train)
train <- xgb.DMatrix(data = train, label = TARGET)

parametros <- list(objective="binary:logistic", booster="gbtree", 
                   eval_metric="auc", eta=0.008235294, subsample=0.75, 
                   colsample_bytree=0.7, min_child_weight=0.6, max_depth=7)

modelo <- xgb.train(params=parametros, data=train, nrounds=850, maximize=FALSE)

test$TARGET <- -1
test <- sparse.model.matrix(TARGET ~ ., data = test)

predictions <- predict(modelo, test)

write.csv(data.frame(ID=testID, TARGET=predictions), 
          file = "XGB_tree_1826.csv",
          row.names=FALSE)