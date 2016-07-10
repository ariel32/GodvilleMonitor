library("jsonlite"); library("XML"); library("httr"); library("xgboost"); library("ggplot2"); library("caret"); library("ROCR")
monitor <- function(god, res.return = FALSE) {
  print(god)
  js = GET(sprintf("http://godville.net/gods/api/%s.json", god))
  html = GET(sprintf("http://godville.net/gods/%s",god))
  if (js$status_code == 200 & html$status_code == 200) {
    js <- content(js)
    # парсим ачивки
    if(length(readHTMLList(htmlParse(html))) > 1) a <- gsub(pattern = "\n\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\t", " ", readHTMLList(htmlParse(html))[[2]])
    html = readHTMLTable(htmlParse(html), header = F, stringsAsFactors = FALSE, encoding = "UTF-8")
    if(!is.null(html$characteristics) & !is.null(html$panteons)) {
      godname         = js$godname
      time            = as.numeric(as.POSIXct(Sys.time()))
      # gold conversion
      k = as.numeric(gsub("[^0-9]", "", js$gold_approx))
      if(is.na(k)) k = 1
      if(length(grep("ни одного", js$gold_approx))) gold_approx <- 0
      if(length(grep("неизвестно", js$gold_approx))) gold_approx <- NA
      if(length(grep("дес", js$gold_approx))) gold_approx <- k*10
      if(length(grep("сот", js$gold_approx))) gold_approx <- k*100
      if(length(grep("тыс", js$gold_approx))) gold_approx <- k*1000
      if(length(grep("несколько", js$gold_approx))) gold_approx <- 5
      level           = js$level
      equip.level     = median(as.numeric(html$equipment$V3), na.rm = T)
      if(js$alignment=="нейтральный") alignment <- 0
      if(js$alignment=="недовольный") alignment <- -1
      if(js$alignment=="озлобленный") alignment <- -2
      if(js$alignment=="агрессивный") alignment <- -3
      if(js$alignment=="злобный")     alignment <- -4
      if(js$alignment=="чистое зло")  alignment <- -5
      if(js$alignment=="чистое зло!") alignment <- -6
      if(js$alignment=="беззлобный")        alignment <- 1
      if(js$alignment=="добродушный")       alignment <- 2
      if(js$alignment=="миролюбивый")       alignment <- 3
      if(js$alignment=="добродетельный")    alignment <- 4
      if(js$alignment=="абсолютное добро")  alignment <- 5
      if(js$alignment=="абсолютное добро!") alignment <- 6
      wood_cnt        = js$wood_cnt
      health          = js$max_health
      pet_level       = ifelse(is.null(js$pet$pet_level), "NA", js$pet$pet_level)
      # age conversion
      age             = html$characteristics$V2[2]
      age <- unlist(strsplit(age, "[^[:digit:]]"))
      age <- as.numeric(age[age != ""])
      if(length(age)==1) {if(grepl("мес", age)) {age = age*30} else {age = age*360}}
      if(length(age)==2) age <- age[1]*360 + age[2]*30
      # killed monsters conversion
      monsters_killed = html$characteristics$V2[6]
      k = as.numeric(gsub("[^0-9]", "", monsters_killed))
      if(is.na(k)) k = 1
      if(length(grep("ни одного", monsters_killed))) monsters_killed <- 0
      if(length(grep("дес", monsters_killed))) monsters_killed <- k*10
      if(length(grep("сот", monsters_killed))) monsters_killed <- k*100
      if(length(grep("тыс", monsters_killed))) monsters_killed <- k*1000
      deaths          = html$characteristics$V2[7]
      arena.wins      = as.numeric(strsplit(html$characteristics$V2[8], split = " / ")[[1]][1])
      arena.loses     = as.numeric(strsplit(html$characteristics$V2[8], split = " / ")[[1]][2])
      p.might         = html$panteons$V2[html$panteons$V1 == "Мощи"]
      p.templehood    = html$panteons$V2[html$panteons$V1 == "Храмовничества"]
      p.gladiatorship = ifelse(length(html$panteons$V2[html$panteons$V1 == "Гладиаторства"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Гладиаторства"])
      p.mastery       = html$panteons$V2[html$panteons$V1 == "Мастерства"]
      p.taming        = ifelse(length(html$panteons$V2[html$panteons$V1 == "Звероводства"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Звероводства"])
      p.survival      = html$panteons$V2[html$panteons$V1 == "Живучести"]; if(identical(p.survival,character(0))) p.survival = NA
      p.savings       = html$panteons$V2[html$panteons$V1 == "Зажиточности"]
      p.alignment     = ifelse(length(html$panteons$V2[html$panteons$V1 == "Созидания" | html$panteons$V1 == "Разрушения"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Созидания" | html$panteons$V1 == "Разрушения"])
      ##### achievments
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
      achievment <- as.data.frame(t(achievment))
      a <- cbind(godname,time,gold_approx,level,equip.level,alignment,wood_cnt,health,pet_level,age,monsters_killed,
                            deaths,arena.wins,arena.loses,p.might,p.templehood,p.gladiatorship,p.mastery,p.taming,p.survival,p.savings,p.alignment)
      a <- cbind(a,achievment)
      #### return result if we just monitoring one god
      if(res.return == TRUE) return(cbind(alignment = a$alignment, arena.wins = a$arena.wins, arena.loses = a$arena.loses,
                                          arena.rate = as.numeric(as.character(a$arena.wins))/as.numeric(as.character(a$arena.loses)),
                                          gold_approx = a$gold_approx, level = a$level, age = a$age,
                                          equip.rate = as.numeric(as.character(a$equip.level))/as.numeric(as.character(a$level)),
                                          might = a$p.might, templehood = a$p.templehood,
                                          gladiatorship = a$p.gladiatorship, mastery = a$p.mastery, taming = a$p.taming, survival = a$p.survival,
                                          savings = a$p.savings, alignment.p = a$p.alignment, a.lamb = a$a.lamb, a.imp = a$a.imp, a.martyr = a$a.martyr,
                                          a.favorite = a$a.favorite, a.scoffer = a$a.scoffer, a.warrior = a$a.warrior, a.maniac = a$a.maniac, a.champion = a$a.champion,
                                          a.tutor = a$a.tutor, a.hunter = a$a.hunter, a.plunderer = a$a.plunderer, a.careerist = a$a.careerist, a.breeder = a$a.breeder,
                                          a.shipbuilder = a$a.shipbuilder, a.sailor = a$a.sailor))
      if(file.exists("DungeonsDB.csv")) {
        write.table(a, "DungeonsDB.csv", sep = ";", col.names = F, append = T, row.names = F)
      } else {
        write.table(a, "DungeonsDB.csv", sep = ";", col.names = T, row.names = F)
      }
    }
  } else { print("Status not OK"); write.table(data.frame(time = Sys.time(), godname = god, js.status = js$status_code, html.status = html$status_code), file = "log.csv", sep = ";", row.names = F, append = T, col.names = F)}
  Sys.sleep(5)
}
source("res.function.R")
res.initial <- res.function()

###################################################
# XGBOOST
res <- res.initial
res[is.na(res)] <- -999
label = res$active
res <- res[,4:34]

xgb.data.matrix <- xgb.DMatrix(data = data.matrix(res), label = label, missing = -999)

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
xgb.model = xgboost(data = xgb.data.matrix, params = param, nrounds = 200)

xgb.model.cv <- xgb.cv(data = xgb.data.matrix, nfold = 8, label = label, nrounds = 100, objective = "binary:logistic", eval_metric = "auc", prediction=TRUE)
xgb.model.cv$pred
# confusion matrix
xgb.model.cv$pred <- ifelse(as.numeric(xgb.model.cv$pred) > 0.5, 1, 0)
confusionMatrix(data=xgb.model.cv$pred, label)

##### choosing parameters with caret
xgb.grid = expand.grid(nrounds = 1000, eta = 1, max_depth = c(2, 4, 6, 8, 10), gamma = c(0,1), colsample_bytree=1, min_child_weight=12)
xgb.ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

xgb.model.caret <- train(x = res, y = as.factor(label), method="xgbTree", trControl = xgb.ctrl, tuneGrid = xgb.grid)

pred = predict(xgb.model.caret, newdata=res) #for good testing needed replace d for testing set :(
confusionMatrix(data=pred, label)

newdata <- monitor("Nekonekoneko", res.return = TRUE)
newdata <- monitor("Capsula", res.return = TRUE)
newdata <- monitor("Крень", res.return = TRUE)

predict(xgb.model.caret, newdata = data.matrix(newdata), type = "prob")

################### ROC
predicted <- predict(xgb.model.caret, newdata = res.initial[,4:34])
prob <- prediction(as.numeric(predicted), res.initial[,3])
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






#xgb.plot.tree(feature_names = names(res[,4:33]),model = m.xgboost)
#importance_matrix <- xgb.importance(names(res[,4:33]), model = m.xgboost)
#xgb.plot.importance(importance_matrix)

