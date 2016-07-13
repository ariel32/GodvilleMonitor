library("xgboost"); library("ggplot2"); library("caret"); library("ROCR")

d = read.csv("DungeonsDB.csv", sep = ";", stringsAsFactor = F)
godnames = unique(d$godname)

coeff <- sapply(godnames, FUN = function(x) {
  data = d[d$godname==x,]
  as.numeric(lm(wood_cnt ~ time, data = data)$coefficients[2])
} )

res.initial = data.frame(name = godnames, woods = round(as.numeric(coeff*60*60*24),2), stringsAsFactors = F)
res.initial$active = ifelse(res.initial$woods > median(res.initial$woods, na.rm = T), 1, 0)

library("plyr"); library("dplyr")

d %>% left_join(res.initial, c("godname" = "name")) %>% mutate(arena.rate = arena.wins/arena.loses, equip.rate = equip.level/level) -> res
label = res$active; res %<>% select(-c(godname, time, woods, active))

###################################################
# XGBOOST
xgb.data.matrix <- xgb.DMatrix(data = data.matrix(res), label = label, missing = NA)

param <- list("objective" = "binary:logistic",    # binary classification 
              "num_class" = 1,    # number of classes 
              "eval_metric" = "auc",    # evaluation metric 
              "nthread" = 16,   # number of threads to be used 
              "max_depth" = 16,    # maximum depth of tree 
              "eta" = 1,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12  # minimum sum of instance weight needed in a child 
)
xgb.model = xgboost(data = xgb.data.matrix, params = param, nrounds = 20)

xgb.cv.data <- xgb.cv(data = xgb.data.matrix, nfold = 8, label = label, nrounds = 100, objective = "binary:logistic", eval_metric = "auc", prediction=TRUE)
xgb.cv.data$pred
# confusion matrix
xgb.cv.data$pred <- ifelse(as.numeric(xgb.cv.data$pred) > 0.5, 1, 0)
confusionMatrix(data=xgb.cv.data$pred, label)

##### choosing parameters with caret
xgb.grid = expand.grid(nrounds = 1000, eta = c(0), max_depth = c(10), gamma = c(0,1), colsample_bytree=1, min_child_weight=c(12))
xgb.ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
xgb.model.caret <- train(x = res, y = as.factor(label), method="xgbTree", trControl = xgb.ctrl, tuneGrid = xgb.grid)
save(xgb.model.caret, file = "xgb.model.caret")
pred = predict(xgb.model.caret, newdata=res)
confusionMatrix(data=pred, label)

# manual checking :/
x = sample(dim(res)[1], 1); label[x]; predict(xgb.model.caret, newdata = res[x,], type = "prob")
#importance_matrix <- xgb.importance(names(res), model = xgb.model)
#xgb.plot.importance(importance_matrix)

newdata <- monitor("Nekonekoneko", res.return = TRUE)
newdata <- monitor("Capsula", res.return = TRUE)
newdata <- monitor("Крень", res.return = TRUE)

predict(xgb.model.caret, newdata = data.matrix(newdata), type = "prob")

################### ROC
predicted <- predict(xgb.model.caret, newdata = res)
prob <- prediction(as.numeric(as.character(predicted)), label)
tprfpr <- performance(prob, "tpr", "fpr")
tpr <- unlist(slot(tprfpr, "y.values"))
fpr <- unlist(slot(tprfpr, "x.values"))
roc <- data.frame(tpr, fpr)
cutoffs <- data.frame(cut=tprfpr@alpha.values[[1]], fpr=tprfpr@x.values[[1]], tpr=tprfpr@y.values[[1]])
acc.perf = performance(prob, measure = "acc"); plot(acc.perf)
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
sp = performance(prob, measure = "spec"); se = performance(prob, measure = "sens")
auc = performance(prob, measure = "auc"); f = performance(prob, measure = "f")
print(c(accuracy=acc, f=f@y.values[[1]][ind], sp = sp@y.values[[1]][ind], se = se@y.values[[1]][ind], auc = auc@y.values[[1]][1], cutoff = cutoff))

##### plot ROC-curve
ggplot(roc) + geom_line(aes(x = fpr, y = tpr), size = 1.8, color = "green") + 
  geom_abline(intercept = 0, slope = 1, colour = "gray") +
  ggtitle("ROC-curve for logistic model") +
  ylab("Sensitivity") + 
  xlab("1 - Specificity") +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)))






#xgb.plot.tree(feature_names = names(res[,4:33]),model = m.xgboost)
#importance_matrix <- xgb.importance(names(res[,4:33]), model = m.xgboost)
#xgb.plot.importance(importance_matrix)

