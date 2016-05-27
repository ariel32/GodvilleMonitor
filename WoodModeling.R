d = dir("Dungeons/", pattern = "*.csv")
d = unlist(strsplit(d, ".d.csv"))

coeff <- sapply(d, FUN = function(x) {
  data = read.csv(paste0("Dungeons/", x), sep = ";")
  as.numeric(lm(wood_cnt ~ time, data = data)$coefficients[2])
  }
)

  
head(sort(coeff, decreasing = T))*60*60*24
