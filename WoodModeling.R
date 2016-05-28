d = read.csv("DungeonsDB.csv", sep = ";", stringsAsFactor = F, encoding = "UTF-8")
godnames = unique(d$godname)

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

# получаем золото
gold_approx[grep("ни одного", d$gold_approx)] <- 0
gold_approx[grep("дес", d$gold_approx)] <- 10
gold_approx[grep("сот", d$gold_approx)] <- 100
gold_approx[grep("тыс", d$gold_approx)] <- 1000
i <- which(!is.na(as.numeric(gsub("[^0-9]", "", d$gold_approx))))
gold_approx[i] <- gold_approx[i]*as.numeric(gsub("[^0-9]", "", d$gold_approx))[i]

