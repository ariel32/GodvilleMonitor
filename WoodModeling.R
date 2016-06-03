d = read.csv("DungeonsDB.csv", sep = ";", stringsAsFactor = F, encoding = "UTF-8")
godnames = unique(d$godname)
#save(godnames, file = "godnames")

coeff <- sapply(godnames, FUN = function(x) {
  data = d[d$godname==x,]
  as.numeric(lm(wood_cnt ~ time, data = data)$coefficients[2])
  } )

# переводим коэффициент наклона прямой в количество бревен
res = data.frame(name = godnames, woods = round(as.numeric(coeff*60*60*24),2))
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

res <- res[complete.cases(res),]
res <- res[-which(is.nan(res$arena.rate) | is.infinite(res$arena.rate)),]
l <- glm(active ~ alignment+arena.rate+gold+level+might+templehood+gladiatorship+mastery+taming+survival+savings+align.r+a.lamb+a.imp+a.martyr+a.favorite+a.scoffer+a.warrior+a.maniac+a.champion+a.tutor+a.hunter+a.plunderer+a.careerist+a.breeder+a.shipbuilder+a.sailor, data = res, family=binomial(logit))
summary(l)
predict.glm(l, res[res$name=="Capsula",], type = "response")

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