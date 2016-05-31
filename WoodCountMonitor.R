library("jsonlite"); library("XML"); library("httr")
setwd("/home/capsula/work/_archive/GodvilleMonitor")

monitor <- function(god) {
  Sys.sleep(5); print(god)
  js = GET(sprintf("http://godville.net/gods/api/%s.json", god))
  html = GET(sprintf("http://godville.net/gods/%s",god))
  if (js$status_code == 200 & html$status_code == 200) {
    js <- content(js)
    html = readHTMLTable(htmlParse(html), header = F, stringsAsFactors = FALSE, encoding = "UTF-8")
    if(!is.null(html$characteristics) & !is.null(html$panteons)) {
      godname         = js$godname
      time            = as.numeric(as.POSIXct(Sys.time()))
      # gold conversion
      k = as.numeric(gsub("[^0-9]", "", js$gold_approx))
      if(is.na(k)) k = 1
      if(length(grep("ни одного", js$gold_approx))) gold_approx <- 0
      if(length(grep("дес", js$gold_approx))) gold_approx <- k*10
      if(length(grep("сот", js$gold_approx))) gold_approx <- k*100
      if(length(grep("тыс", js$gold_approx))) gold_approx <- k*1000
      if(length(grep("несколько", js$gold_approx))) gold_approx <- 5
      level           = js$level
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
      age             = html$characteristics$V2[2]
      monsters_killed = html$characteristics$V2[6]
      # killed monsters conversion
      k = as.numeric(gsub("[^0-9]", "", monsters_killed))
      if(is.na(k)) k = 1
      if(length(grep("ни одного", monsters_killed))) monsters_killed <- 0
      if(length(grep("дес", monsters_killed))) monsters_killed <- k*10
      if(length(grep("сот", monsters_killed))) monsters_killed <- k*100
      if(length(grep("тыс", monsters_killed))) monsters_killed <- k*1000
      deaths          = html$characteristics$V2[7]
      arena.wins      = strsplit(html$characteristics$V2[8], split = " / ")[[1]][1]
      arena.loses     = strsplit(html$characteristics$V2[8], split = " / ")[[1]][2]
      p.might         = html$panteons$V2[html$panteons$V1 == "Мощи"]
      p.templehood    = html$panteons$V2[html$panteons$V1 == "Храмовничества"]
      p.gladiatorship = ifelse(length(html$panteons$V2[html$panteons$V1 == "Гладиаторства"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Гладиаторства"])
      p.mastery       = html$panteons$V2[html$panteons$V1 == "Мастерства"]
      p.taming        = ifelse(length(html$panteons$V2[html$panteons$V1 == "Звероводства"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Звероводства"])
      p.survival      = html$panteons$V2[html$panteons$V1 == "Живучести"]
      p.savings       = html$panteons$V2[html$panteons$V1 == "Зажиточности"]
      p.alignment     = ifelse(length(html$panteons$V2[html$panteons$V1 == "Созидания" | html$panteons$V1 == "Разрушения"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Созидания" | html$panteons$V1 == "Разрушения"])
      
      a <- cbind(godname,time,gold_approx,level,alignment,wood_cnt,health,pet_level,age,monsters_killed,
                 deaths,arena.wins,arena.loses,p.might,p.templehood,p.gladiatorship,p.mastery,p.taming,p.survival,p.savings,p.alignment)
      
      if(file.exists("DungeonsDB.csv")) {
        write.table(a, "DungeonsDB.csv", sep = ";", col.names = F, append = T, row.names = F)
      } else {
        write.table(a, "DungeonsDB.csv", sep = ";", col.names = T, row.names = F)
      }
    }
  } else { print("Status not OK")}
}

### god names for monitoring
load("godnames")
sapply(godnames, monitor)
