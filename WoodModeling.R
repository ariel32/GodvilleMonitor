# rm(list = ls())
library("xgboost"); library("ggplot2"); library("caret"); library("ROCR"); library("jsonlite"); library("XML"); library("httr")
library("doParallel"); library("plyr"); library("dplyr"); library("magrittr")
#registerDoParallel(detectCores())
options(mc.cores = detectCores())

d = read.csv("DungeonsDB.csv", sep = ";", stringsAsFactor = F)

d %>% select(godname, wood_cnt, time) %>% group_by(godname) %>%
  do(model = summary(lm(wood_cnt ~ time, data = .))) %>% ungroup %>%
  mutate(slope = sapply(model, "[[", c(4,2)), r.squared = sapply(model, "[[", "r.squared")) %>% select(-model) -> gods.stats

res.initial = data.frame(name = gods.stats$godname, woods = round(as.numeric(gods.stats$slope*60*60*24),2), stringsAsFactors = F)
#res.initial$active = ifelse(res.initial$woods > median(res.initial$woods, na.rm = T), 1, 0)
res.initial$active = ifelse(res.initial$woods > 2.5, 1, 0)

d %>% left_join(res.initial, c("godname" = "name")) %>% mutate(arena.rate = arena.wins/arena.loses, equip.rate = equip.level/level) -> res
res$active -> label; res %<>% select(-c(godname, time, woods, active)) %>% mutate_each(funs(as.numeric))







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

xgb.cv.data <- xgb.cv(data = xgb.data.matrix, nfold = 8, label = label, nrounds = 10, objective = "binary:logistic", eval_metric = "auc", prediction=TRUE)
xgb.cv.data$pred
# confusion matrix
xgb.cv.data$pred <- ifelse(as.numeric(xgb.cv.data$pred) > 0.5, 1, 0)
confusionMatrix(data=xgb.cv.data$pred, label)









################################
### SPLITTING DATA AND PARAMETER SEARCHING

inTrain <- sample(dim(res)[1], dim(res)[1]/4*3, replace = F)
#dtrain <- xgb.DMatrix(data = data.matrix(res[inTrain, ]), label = factor(label[inTrain], labels = c("no", "yes")), missing = NA)
#dtest  <- xgb.DMatrix(data = data.matrix(res[-inTrain, ]), label = factor(label[-inTrain], labels = c("no", "yes")), missing = NA)

##### choosing parameters with caret
xgb.grid <- expand.grid(nrounds = 1000,
                        max_depth = c(2,4,6,8,10),
                        eta = c(1, 0.5, 0.1, 0.05, 0.01, 0.001, 0.0001),
                        gamma = 1, colsample_bytree = 0.6,
                        min_child_weight = 1)

xgb.ctrl = trainControl(method = "repeatedcv", repeats = 1, number = 5, 
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel=T)

xgb.tune2 <- train(x = data.matrix(res[inTrain, ]),
                  y = factor(label[inTrain], labels = c("no", "yes")),
                  method="xgbTree",
                  trControl=xgb.ctrl,
                  tuneGrid=xgb.grid,
                  verbose=T,
                  metric="Kappa",
                  nthread = 3)
save(xgb.tune2, file = "xgb.tune2")
#importance_matrix <- xgb.importance(names(res), model = xgb.tune$finalModel)
#xgb.plot.importance(importance_matrix)
pred = predict(xgb.tune, newdata=res[-inTrain, ])
confusionMatrix(data=pred, factor(label[-inTrain], labels = c("no", "yes")))

newdata <- monitor("Nekonekoneko", res.return = TRUE) # YES - 100%
newdata <- monitor("Capsula", res.return = TRUE)      # YES - 100%
newdata <- monitor("Сангдир", res.return = TRUE)      # NO  - 100%
newdata <- monitor("Curandero", res.return = TRUE)    # NO  - 100%
newdata <- monitor("Uunium", res.return = TRUE)
predict(xgb.tune, newdata, type='prob')[, 'yes']


################### ROC
predicted <- predict(xgb.tune, newdata = res, type='prob')[, 'yes']
prob <- prediction(predicted, label)
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













# manual checking :/
x = sample(dim(res)[1], 1); label[x]; predict(xgb.model.caret, newdata = res[x,], type = "prob")
#importance_matrix <- xgb.importance(names(res), model = xgb.model)
#xgb.plot.importance(importance_matrix)

newdata <- monitor("Nekonekoneko", res.return = TRUE)
newdata <- monitor("Capsula", res.return = TRUE)
newdata <- monitor("Крень", res.return = TRUE)

predict(xgb.model.caret, newdata = data.matrix(newdata), type = "prob")
predict(xgb.model, newdata = data.matrix(newdata))
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

