d = dir("Dungeons/", pattern = "*.csv")

for(x in d){
  data = read.csv(paste0("Dungeons/", x), sep = ";", encoding = "UTF-8")
  god = gsub(pattern = "%20", replacement = " ", x = unlist(strsplit(x, ".d.csv")))
  data = cbind(god, data)
  write.table(data, "DungeonsDB.csv", sep = ";", col.names = F, append = T, row.names = F)
}


coeff <- sapply(d, FUN = function(x) {
  data = read.csv(paste0("Dungeons/", x), sep = ";")
  as.numeric(lm(wood_cnt ~ time, data = data)$coefficients[2])
  }
)

n = gsub(pattern = "%20", replacement = " ", x = unlist(strsplit(d, ".d.csv")))
res = data.frame(name = n, woods = round(as.numeric(coeff*60*60*24),2))
res <- res[order(-res$woods),]
head(res)

res$woods[res$name=="Capsula"]

capsula.d = read.csv("capsula2.csv", sep = ";")
capsula.woods = round(as.numeric(lm(wood_cnt ~ time, data = capsula.d)$coefficients[2])*60*60*24,2)
