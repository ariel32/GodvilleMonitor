d = read.csv("DungeonsDB.csv", sep = ";", stringsAsFactor = F)
godnames = unique(d$godname)

coeff <- sapply(godnames, FUN = function(x) {
  data = d[d$godname==x,]
  as.numeric(lm(wood_cnt ~ time, data = data)$coefficients[2])
  }
)

res = data.frame(name = godnames, woods = round(as.numeric(coeff*60*60*24),2))
res <- res[order(-res$woods),]
head(res)

res$woods[res$name=="Capsula"]

capsula.d = read.csv("capsula2.csv", sep = ";")
capsula.woods = round(as.numeric(lm(wood_cnt ~ time, data = capsula.d)$coefficients[2])*60*60*24,2)
