library("jsonlite"); library("XML")
setwd("/home/capsula/work/_archive/GodvilleMonitor")

monitor <- function(god) {
  js = fromJSON(sprintf("http://godville.net/gods/api/%s.json", god))
  html = readHTMLTable(sprintf("http://godville.net/gods/%s",god), header = F, stringsAsFactors = FALSE)
  Sys.sleep(5)
  godname         = js$godname
  time            = as.numeric(as.POSIXct(Sys.time()))
  gold_approx     = js$gold_approx
  level           = js$level
  alignment       = js$alignment
  wood_cnt        = js$wood_cnt
  health          = js$max_health
  pet_level       = js$pet$pet_level
  age             = html$characteristics$V2[2]
  monsters_killed = html$characteristics$V2[6]
  deaths          = html$characteristics$V2[7]
  arena_score     = html$characteristics$V2[8]
  p.might         = html$panteons$V2[html$panteons$V1 == "Мощи"]
  p.templehood    = html$panteons$V2[html$panteons$V1 == "Храмовничества"]
  p.gladiatorship = ifelse(length(html$panteons$V2[html$panteons$V1 == "Гладиаторства"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Гладиаторства"])
  p.mastery       = html$panteons$V2[html$panteons$V1 == "Мастерства"]
  p.taming        = ifelse(length(html$panteons$V2[html$panteons$V1 == "Звероводства"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Звероводства"])
  p.survival      = html$panteons$V2[html$panteons$V1 == "Живучести"]
  p.savings       = html$panteons$V2[html$panteons$V1 == "Зажиточности"]
  p.alignment     = ifelse(length(html$panteons$V2[html$panteons$V1 == "Созидания" | html$panteons$V1 == "Разрушения"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Созидания" | html$panteons$V1 == "Разрушения"])
  
  a <- cbind(godname,time,gold_approx,level,alignment,wood_cnt,health,pet_level,age,monsters_killed,
    deaths,arena_score,p.might,p.templehood,p.gladiatorship,p.mastery,p.taming,p.survival,p.savings,p.alignment)
  
  if(file.exists("DungeonsDB.csv")) {
    write.table(a, "DungeonsDB.csv", sep = ";", col.names = F, append = T, row.names = F)
  } else {
    write.table(a, "DungeonsDB.csv", sep = ";", col.names = T, row.names = F)
  }
}

### god names for monitoring
load("god.names")
sapply(god.names, monitor)
