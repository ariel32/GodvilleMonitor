d = read.csv("DungeonsDB.csv", sep = ";", stringsAsFactor = F, encoding = "UTF-8")
godnames = unique(d$godname)
#save(godnames, file = "godnames")

coeff <- sapply(godnames, FUN = function(x) {
  data = d[d$godname==x,]
  as.numeric(lm(wood_cnt ~ time, data = data)$coefficients[2])
  }
)

# переводим коэффициент наклона прямой в количество бревен
res = data.frame(name = godnames, woods = round(as.numeric(coeff*60*60*24),2))
res <- res[order(-res$woods),]
head(res)

res$woods[res$name=="Capsula"]
capsula.d = read.csv("capsula2.csv", sep = ";")
capsula.woods = round(as.numeric(lm(wood_cnt ~ time, data = capsula.d)$coefficients[2])*60*60*24,2)
plot(wood_cnt ~ time, data = d[d$godname=="Изерка",], type = "l")



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