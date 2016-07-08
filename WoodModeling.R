############################ DATA TRANSFORMING AND PREPEARING
#############################################################
d = read.csv("DungeonsDB.csv", sep = ";", stringsAsFactor = F)
godnames = unique(d$godname)
#save(godnames, file = "godnames")

coeff <- sapply(godnames, FUN = function(x) {
  data = d[d$godname==x,]
  as.numeric(lm(wood_cnt ~ time, data = data)$coefficients[2])
  } )

# переводим коэффициент наклона прямой в количество бревен
res = data.frame(name = godnames, woods = round(as.numeric(coeff*60*60*24),2), stringsAsFactors = F)
res <- res[order(-res$woods),]
head(res)

# summarization
res$active = ifelse(res$woods > median(res$woods, na.rm = T), 1, 0)
res$alignment <- sapply(res$name, function(x) FUN = median(d$alignment[d$godname==x]))
res$arena.wins <- sapply(res$name, function(x) FUN = median(d$arena.wins[d$godname==x]))
res$arena.loses <- sapply(res$name, function(x) FUN = median(d$arena.loses[d$godname==x]))
res$arena.rate <- sapply(res$name, function(x) FUN = median(d$arena.wins[d$godname==x])/median(d$arena.loses[d$godname==x]))
res$gold <- sapply(res$name, function(x) FUN = median(d$gold_approx[d$godname==x]))
res$level <- sapply(res$name, function(x) FUN = median(d$level[d$godname==x]))
res$equip.rate    <- sapply(res$name, function(x) FUN = median(d$equip.level[d$godname==x], na.rm = T)/median(d$level[d$godname==x], na.rm = T))
res$might         <- sapply(res$name, function(x) FUN = median(d$p.might[d$godname==x]))
res$templehood    <- sapply(res$name, function(x) FUN = median(d$p.templehood[d$godname==x]))
res$gladiatorship <- sapply(res$name, function(x) FUN = median(d$p.gladiatorship[d$godname==x]))
res$mastery       <- sapply(res$name, function(x) FUN = median(d$p.mastery[d$godname==x]))
res$taming        <- sapply(res$name, function(x) FUN = median(d$p.taming[d$godname==x]))
res$survival      <- sapply(res$name, function(x) FUN = median(d$p.survival[d$godname==x]))
res$savings       <- sapply(res$name, function(x) FUN = median(d$p.savings[d$godname==x]))
res$align.r       <- sapply(res$name, function(x) FUN = median(d$p.alignment[d$godname==x]))
res$a.lamb        <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.lamb[d$godname==x], na.rm = T)))
res$a.imp         <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.imp[d$godname==x], na.rm = T)))
res$a.martyr      <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.martyr[d$godname==x], na.rm = T)))
res$a.favorite    <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.favorite[d$godname==x], na.rm = T)))
res$a.scoffer     <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.scoffer[d$godname==x], na.rm = T)))
res$a.warrior     <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.warrior[d$godname==x], na.rm = T)))
res$a.maniac      <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.maniac[d$godname==x], na.rm = T)))
res$a.champion    <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.champion[d$godname==x], na.rm = T)))
res$a.tutor       <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.tutor[d$godname==x], na.rm = T)))
res$a.hunter      <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.hunter[d$godname==x], na.rm = T)))
res$a.plunderer   <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.plunderer[d$godname==x], na.rm = T)))
res$a.careerist   <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.careerist[d$godname==x], na.rm = T)))
res$a.breeder     <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.breeder[d$godname==x], na.rm = T)))
#res$a.architect   <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.architect[d$godname==x], na.rm = T)))
res$a.shipbuilder <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.shipbuilder[d$godname==x], na.rm = T)))
res$a.sailor      <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.sailor[d$godname==x], na.rm = T)))
#res$a.fowler      <- sapply(res$name, function(x) FUN = as.numeric(median(d$a.fowler[d$godname==x], na.rm = T)))


############# DATA CLEANING
res <- res[complete.cases(res),]
res <- res[-which(is.nan(res$arena.rate) | is.infinite(res$arena.rate)),]

########## XGBOOST
library(xgboost); library(corrplot); library(Rtsne); library(ggplot2); library(caret)
dm <- xgb.DMatrix(data = data.matrix(res[,4:33]), label = res[,3], missing = NA)
param <- list("objective" = "binary:logistic",    # binary classification 
              "num_class" = 2,    # number of classes 
              "eval_metric" = "auc",    # evaluation metric 
              "nthread" = 8,   # number of threads to be used 
              "max_depth" = 16,    # maximum depth of tree 
              "eta" = 1,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12  # minimum sum of instance weight needed in a child 
)
bst = xgboost(data = dm, params = param, nrounds = 120)
pred = predict(bst, dm)
cv.res <- xgb.cv(data = dm, nfold = 5, label = label, nrounds = 20,
                 objective = "binary:logistic", eval_metric = "auc", prediction=TRUE)



################### CONFUSION MATRIX WITH CARET?
# get CV's prediction decoding
num.class = 2
pred.cv = matrix(cv.res$pred, nrow=length(cv.res$pred)/num.class, ncol=num.class)
# confusion matrix
y = as.matrix(as.integer(label))
confusionMatrix(data=pred.cv, active)

################### ROC
library(ROCR)
library(MKmisc)
library(caret); library(e1071)
library(ggplot2); library(DAAG)

predicted <- predict(bst, newdata = dm)[1]

#predicted[is.na(predicted)] <- 0
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

##### ggplot2
ggplot(roc) + geom_line(aes(x = fpr, y = tpr), size = 1.8, color = "green") + 
  geom_abline(intercept = 0, slope = 1, colour = "gray") +
  ggtitle("ROC-curve for logistic model") +
  ylab("Sensitivity") + 
  xlab("1 - Specificity") +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)))

# CV of models
CVbinary(m.pc2) # as optimal choosed first model

##### k-fold
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_fit <- train(t.formula,  data=data.frame(pc.predict), method="glm", family="binomial", trControl = ctrl, tuneLength = 5)
pred = predict(mod_fit, newdata=data.frame(pc.predict)) #for good testing needed replace d for testing set :(
confusionMatrix(data=pred, res$active)














data <- res[,3:33]
corrplot.mixed(cor(data[,2:31]), lower="circle", upper="color", tl.pos="lt", diag="n", order="hclust", hclust.method="complete")

tsne = Rtsne(as.matrix(data), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)
embedding = as.data.frame(tsne$Y)
embedding$Class = as.factor(data$active)
g = ggplot(embedding, aes(x=V1, y=V2, color=Class)) +
  geom_point(size=1.25) +
  guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE 2D Embedding of 'Classe' Outcome") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
print(g)
bstSparse <- xgboost(data = as.matrix(data[,2:31]), label = data$active, max.depth = 20, eta = 1, nthread = 4, nround = 20, objective = "binary:logistic")

bst <- xgboost(data = as.matrix(data), label = data$active, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic", verbose = 1)

###############
num.class = 2
param <- list("objective" = "binary:logistic",    # binary classification 
              "num_class" = num.class,    # number of classes 
              "eval_metric" = "merror",    # evaluation metric 
              "nthread" = 8,   # number of threads to be used 
              "max_depth" = 16,    # maximum depth of tree 
              "eta" = 1,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12  # minimum sum of instance weight needed in a child 
)
nround.cv = 200
system.time( bst.cv <- xgb.cv(param=param, data=as.matrix(data[,2:31]), label=data$active,nfold=4, nrounds=nround.cv, prediction=TRUE, verbose=FALSE) )
tail(bst.cv$dt)
# get CV's prediction decoding
pred.cv = matrix(bst.cv$pred, nrow=length(bst.cv$pred)/num.class, ncol=num.class)
# confusion matrix
y = as.matrix(as.integer(active))
confusionMatrix(data=pred.cv, active)










############# PCA
m.pca <- prcomp(res[,4:33], scale. = T)

library("factoextra")
fviz_screeplot(pc, ncp=10)

predicted.pca <- data.frame(active = res$active, m.pca$x)
m.logit <- glm(active ~ ., data = predicted.pca, family=binomial(logit))
summary(m.logit)

############ Predictions
pc.predict <- predict(m.pca, newdata = res[res$name=="Capsula",4:33])
pred <- predict(m.logit, newdata = data.frame(pc.predict), type = "response")
pred

library(caret)
vi <- varImp(m.logit)
vi <- rownames(vi)[order(-vi)][1:10]
vi

library(glmulti)
full.formula <- as.formula(paste0("active ~ ", paste(vi, sep = " ", collapse = "+")))

full.formula <- as.formula(paste0("active ~ ", paste(names(m.logit$coefficients)[2:21], sep = " ", collapse = "+")))
g1 <- glmulti(full.formula, data = predicted.pca, level = 2, method = "d",family = binomial(link = logit))

g1 <- glmulti(full.formula,
              data = predicted.pca,level = 2, method = "g", crit = "aic", confsetsize = 5, plotty = F, report = F, maxsize = 1e9,
              fitfunction = "glm", family = binomial(link = logit))

t.formula <- g1@formulas[[1]]
m.logit2 <- glm(t.formula, data = predicted.pca, family = binomial(link=logit))
summary(m.logit2)
predict(m.logit2, newdata = data.frame(pc.predict), type = "response") # my result
predict.glm(m.logit2, newdata = data.frame(pc.predict), type = "response")


################## ROC analysis
library(ROCR)
library(MKmisc)
library(caret); library(e1071)
library(ggplot2); library(DAAG)

predicted <- predict(m.logit2, newdata = predicted.pca, type = "response")

#predicted[is.na(predicted)] <- 0
prob <- prediction(predicted, res2$active)
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

##### ggplot2
ggplot(roc) + geom_line(aes(x = fpr, y = tpr), size = 1.8, color = "green") + 
  geom_abline(intercept = 0, slope = 1, colour = "gray") +
  ggtitle("ROC-curve for logistic model") +
  ylab("Sensitivity") + 
  xlab("1 - Specificity") +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)))

# CV of models
CVbinary(m.pc2) # as optimal choosed first model

##### k-fold
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_fit <- train(t.formula,  data=data.frame(pc.predict), method="glm", family="binomial", trControl = ctrl, tuneLength = 5)
pred = predict(mod_fit, newdata=data.frame(pc.predict)) #for good testing needed replace d for testing set :(
confusionMatrix(data=pred, res$active)























################
# ordinal logistic regression
m1 <- glm(as.formula(paste0("active ~ ", paste(names(res)[4:33], sep = " ", collapse = "+"))), data = res, family=binomial(logit))
summary(m1)

library(caret)
vi <- varImp(m1)
vi <- rownames(vi)[order(-vi)][1:10]

library(glmulti)
full.formula <- as.formula(paste0("active ~ ", paste(vi, sep = " ", collapse = "+")))
g1 <- glmulti(full.formula,
              data = res,level = 2, method = "g", crit = "aic", confsetsize = 5, plotty = F, report = F, maxsize = 500,
              fitfunction = "glm", family = binomial(link = logit))

t.formula <- g1@formulas[[1]]
m1 <- glm(t.formula, data = res, family = binomial(link=logit))
summary(m1)
predict.glm(m1, res[res$name=="Capsula",], type = "response")








m1 <- glm(active ~ alignment+arena.rate+gold+level+might+templehood+gladiatorship+mastery+taming+survival+savings+align.r+a.lamb+a.imp+a.martyr+a.favorite+a.scoffer+a.warrior+a.maniac+a.champion+a.tutor+a.hunter+a.plunderer+a.careerist+a.breeder+a.shipbuilder+a.sailor, data = res, family=binomial(logit))
summary(m1)
predict.glm(m1, res[res$name=="Capsula",], type = "response")
################ GLMULTI
library(glmulti);library(ROCR)
library(MKmisc)
library(caret); library(e1071)
library(ggplot2); library(DAAG)

#res$active <- as.factor(res$active)
#full.formula <- as.formula(active ~ alignment+arena.rate+gold+level+might+templehood+gladiatorship+mastery+taming+survival+savings+align.r+a.lamb+a.imp+a.martyr+a.favorite+a.scoffer+a.warrior+a.maniac+a.champion+a.tutor+a.hunter+a.plunderer+a.careerist+a.breeder+a.shipbuilder+a.sailor)
full.formula <- as.formula(active ~ a.lamb+a.imp+a.martyr+a.favorite+a.scoffer+a.warrior+a.maniac+a.champion+a.tutor+a.hunter+a.plunderer+a.careerist+a.breeder+a.shipbuilder+a.sailor)
g1 <- glmulti(full.formula,
              data = res,level = 2, method = "g", crit = "aic", confsetsize = 5, plotty = F, report = F, maxsize = 500,
              fitfunction = "glm", family = binomial(link = logit))
# look at the formulas
g1@formulas
t.formula <- g1@formulas[[1]]
m1 <- glm(t.formula, data = res, family = binomial(link=logit))
summary(m1)

#################
# проверка модели
HLgof.test(fit = m1$fitted.values, obs = res$active)$C
# ROC analysis
predicted <- predict(m1, res, type="response")
predicted[is.na(predicted)] <- 0
prob <- prediction(predicted, res$active)
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

##### ggplot2
ggplot(roc) + geom_line(aes(x = fpr, y = tpr), size = 1.8, color = "green") + 
  geom_abline(intercept = 0, slope = 1, colour = "gray") +
  ggtitle("ROC-curve for logistic model") +
  ylab("Sensitivity") + 
  xlab("1 - Specificity") +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)))

# CV of models
CVbinary(m1) # as optimal choosed first model

##### k-fold
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_fit <- train(t.formula,  data=res, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)
pred = predict(mod_fit, newdata=res) #for good testing needed replace d for testing set :(
confusionMatrix(data=pred, res$active)

#####
res$woods[res$name=="Capsula"]
capsula.d = read.csv("capsula2.csv", sep = ";")
plot(wood_cnt ~ time, data = capsula.d, type = "l")
capsula.woods = round(as.numeric(lm(wood_cnt ~ time, data = capsula.d)$coefficients[2])*60*60*24,2)
plot(wood_cnt ~ time, data = d[d$godname=="Capsula",], type = "l")

######### ideal woodrate
x = 1:100; y = seq(0, 396, by = 4)+cumsum(rep(c(rep(0,19),6),5))
idealwoodrate = round(as.numeric(lm(y ~ x)$coefficients[2]),2)
###### EXTRACT ACHIEVMENTS
###### 
god = "GodWin"
html = GET(sprintf("http://godville.net/gods/%s",god))
if(length(readHTMLList(htmlParse(html))) > 1) a <- gsub(pattern = "\n\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\t", " ", readHTMLList(htmlParse(html))[[2]])
a <- data.frame(achievment = sapply(strsplit(a, ","), "[[", 1), value = as.numeric(gsub("[^0-9]", "", sapply(strsplit(a, ","), "[", 2))), stringsAsFactors = F)
a$value[grep("Заслуженный", a$achievment)] <- 0
a$achievment <- gsub("Заслуженный ", "", a$achievment)
a.lamb        <- a$value[a$achievment=="Агнец"]
a.imp         <- a$value[a$achievment=="Чертяка"]
a.martyr      <- a$value[a$achievment=="Мученик"]
a.favorite    <- a$value[a$achievment=="Фаворит"]
a.scoffer     <- a$value[a$achievment=="Безбожник"]
a.warrior     <- a$value[a$achievment=="Вояка"]
a.maniac      <- a$value[a$achievment=="Маньяк"]
a.champion    <- a$value[a$achievment=="Чемпион"]
a.tutor       <- a$value[a$achievment=="Наставник"]
a.hunter      <- a$value[a$achievment=="Охотник"]
a.plunderer   <- a$value[a$achievment=="Расхититель"]
a.careerist   <- a$value[a$achievment=="Карьерист"]
a.breeder     <- a$value[a$achievment=="Заводчик"]
a.architect   <- a$value[a$achievment=="Зодчий"]
a.shipbuilder <- a$value[a$achievment=="Корабел"]
a.sailor      <- a$value[a$achievment=="Мореход"]
a.fowler      <- a$value[a$achievment=="Ловец"]
achievment <- sapply(list(a.lamb,a.imp,a.martyr,a.favorite,a.scoffer,a.warrior,a.maniac,a.champion,a.tutor,a.hunter,a.plunderer,a.careerist,a.breeder,a.architect,a.shipbuilder,a.sailor,a.fowler), function(x) ifelse(length(x) > 0,x,4))
names(achievment) <- c("a.lamb","a.imp","a.martyr","a.favorite","a.scoffer","a.warrior","a.maniac","a.champion","a.tutor","a.hunter","a.plunderer","a.careerist","a.breeder","a.architect","a.shipbuilder","a.sailor","a.fowler")
#####
# получаем цифробуквенные данные и перезаписываем БД
#####
monsters_killed[grep("ни одного", d$monsters_killed)] <- 0
monsters_killed[grep("дес", d$monsters_killed)] <- 10
monsters_killed[grep("сот", d$monsters_killed)] <- 100
monsters_killed[grep("тыс", d$monsters_killed)] <- 1000
i <- which(!is.na(as.numeric(gsub("[^0-9]", "", d$monsters_killed))))
monsters_killed[i] <- monsters_killed[i]*as.numeric(gsub("[^0-9]", "", d$monsters_killed))[i]
d$monsters_killed <- monsters_killed
write.table(d, "DungeonsDB.csv", sep = ";", col.names = T, row.names = F)
#####