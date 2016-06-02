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

res <- res[-which(is.nan(res$arena.rate) | is.infinite(res$arena.rate)),]

l <- glm(active ~ alignment+arena.rate+gold+level+might+templehood+gladiatorship+mastery+taming+survival+savings+align.r, data = res, family=binomial(logit))
summary(l)
predict.glm(l, res[res$name=="Capsula",], type = "response")

#####
res$woods[res$name=="Capsula"]
capsula.d = read.csv("capsula2.csv", sep = ";")
capsula.woods = round(as.numeric(lm(wood_cnt ~ time, data = capsula.d)$coefficients[2])*60*60*24,2)
plot(wood_cnt ~ time, data = d[d$godname=="Capsula",], type = "l")

######### ideal woodrate
x = 1:100; y = seq(0, 396, by = 4)+cumsum(rep(c(rep(0,19),6),5))
idealwoodrate = round(as.numeric(lm(y ~ x)$coefficients[2]),2)

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