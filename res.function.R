res.function = function() {
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
  res$gold_approx <- sapply(res$name, function(x) FUN = median(d$gold_approx[d$godname==x]))
  res$level <- sapply(res$name, function(x) FUN = median(d$level[d$godname==x]))
  res$age <- sapply(res$name, function(x) FUN = median(d$age[d$godname==x]))
  res$equip.rate    <- sapply(res$name, function(x) FUN = median(d$equip.level[d$godname==x], na.rm = T)/median(d$level[d$godname==x], na.rm = T))
  res$might         <- sapply(res$name, function(x) FUN = median(d$p.might[d$godname==x]))
  res$templehood    <- sapply(res$name, function(x) FUN = median(d$p.templehood[d$godname==x]))
  res$gladiatorship <- sapply(res$name, function(x) FUN = median(d$p.gladiatorship[d$godname==x]))
  res$mastery       <- sapply(res$name, function(x) FUN = median(d$p.mastery[d$godname==x]))
  res$taming        <- sapply(res$name, function(x) FUN = median(d$p.taming[d$godname==x]))
  res$survival      <- sapply(res$name, function(x) FUN = median(d$p.survival[d$godname==x]))
  res$savings       <- sapply(res$name, function(x) FUN = median(d$p.savings[d$godname==x]))
  res$alignment.p   <- sapply(res$name, function(x) FUN = median(d$p.alignment[d$godname==x]))
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
  return(res)
}
