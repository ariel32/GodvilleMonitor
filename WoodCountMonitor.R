library("jsonlite"); library("XML"); library("httr")
monitor <- function(god, res.return = FALSE) {
  print(god)
  js = GET(sprintf("http://godville.net/gods/api/%s.json", god))
  html = GET(sprintf("http://godville.net/gods/%s",god))
  if (js$status_code == 200 & html$status_code == 200) {
    js <- content(js)
    # парсим ачивки
    if(length(readHTMLList(htmlParse(html))) > 1) a <- gsub(pattern = "\n\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\t", " ", readHTMLList(htmlParse(html))[[2]])
    html = readHTMLTable(htmlParse(html), header = F, stringsAsFactors = FALSE, encoding = "UTF-8")
    if(!is.null(html$characteristics) & !is.null(html$panteons)) {
      godname         = js$godname
      time            = as.numeric(as.POSIXct(Sys.time()))
      # gold conversion
      k = as.numeric(gsub("[^0-9]", "", js$gold_approx))
      if(is.na(k)) k = 1
      if(length(grep("ни одного", js$gold_approx))) gold_approx <- 0
      if(length(grep("неизвестно", js$gold_approx))) gold_approx <- NA
      if(length(grep("дес", js$gold_approx))) gold_approx <- k*10
      if(length(grep("сот", js$gold_approx))) gold_approx <- k*100
      if(length(grep("тыс", js$gold_approx))) gold_approx <- k*1000
      if(length(grep("несколько", js$gold_approx))) gold_approx <- 5
      level           = js$level
      equip.level     = median(as.numeric(html$equipment$V3), na.rm = T)
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
      # age conversion
      age             = html$characteristics$V2[2]
      age <- unlist(strsplit(age, "[^[:digit:]]"))
      age <- as.numeric(age[age != ""])
      if(length(age)==1) {if(grepl("мес", age)) {age = age*30} else {age = age*360}}
      if(length(age)==2) age <- age[1]*360 + age[2]*30
      # killed monsters conversion
      monsters_killed = html$characteristics$V2[6]
      k = as.numeric(gsub("[^0-9]", "", monsters_killed))
      if(is.na(k)) k = 1
      if(length(grep("ни одного", monsters_killed))) monsters_killed <- 0
      if(length(grep("дес", monsters_killed))) monsters_killed <- k*10
      if(length(grep("сот", monsters_killed))) monsters_killed <- k*100
      if(length(grep("тыс", monsters_killed))) monsters_killed <- k*1000
      deaths          = html$characteristics$V2[7]
      arena.wins      = as.numeric(strsplit(html$characteristics$V2[8], split = " / ")[[1]][1])
      arena.loses     = as.numeric(strsplit(html$characteristics$V2[8], split = " / ")[[1]][2])
      p.might         = ifelse(length(html$panteons$V2[html$panteons$V1 == "Мощи"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Мощи"])
      p.templehood    = html$panteons$V2[html$panteons$V1 == "Храмовничества"]
      p.gladiatorship = ifelse(length(html$panteons$V2[html$panteons$V1 == "Гладиаторства"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Гладиаторства"])
      p.mastery       = html$panteons$V2[html$panteons$V1 == "Мастерства"]
      p.taming        = ifelse(length(html$panteons$V2[html$panteons$V1 == "Звероводства"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Звероводства"])
      p.survival      = html$panteons$V2[html$panteons$V1 == "Живучести"]; if(identical(p.survival,character(0))) p.survival = NA
      p.savings       = html$panteons$V2[html$panteons$V1 == "Зажиточности"]
      p.alignment     = ifelse(length(html$panteons$V2[html$panteons$V1 == "Созидания" | html$panteons$V1 == "Разрушения"]) == 0, 0, html$panteons$V2[html$panteons$V1 == "Созидания" | html$panteons$V1 == "Разрушения"])
      ##### achievments
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
      achievment <- as.data.frame(t(achievment))
      a <- cbind(godname,time,gold_approx,level,equip.level,alignment,wood_cnt,health,pet_level,age,monsters_killed,
                 deaths,arena.wins,arena.loses,p.might,p.templehood,p.gladiatorship,p.mastery,p.taming,p.survival,p.savings,p.alignment)
      a <- cbind(a,achievment)
      #### return result if we just monitoring one god
      if(res.return == TRUE) {
        df <- data.frame(gold_approx = a$gold_approx, level = a$level, equip.level = a$equip.level,
                         alignment = a$alignment, wood_cnt = a$wood_cnt, health = a$health, pet_level = a$pet_level,
                         age = a$age, monsters_killed = a$monsters_killed,
                         deaths = a$deaths, arena.wins = a$arena.wins, arena.loses = a$arena.loses,
                         p.might = a$p.might, p.templehood = a$p.templehood, p.gladiatorship = a$p.gladiatorship,
                         p.mastery = a$p.mastery, p.taming = a$p.taming, p.survival = a$p.survival, p.savings = a$p.savings,
                         p.alignment = a$p.alignment,
                         a.lamb = achievment$a.lamb, a.imp = achievment$a.imp, a.martyr = achievment$a.martyr, a.favorite = achievment$a.favorite,
                         a.scoffer = achievment$a.scoffer, a.warrior = achievment$a.warrior, a.maniac = achievment$a.maniac,
                         a.champion = achievment$a.champion, a.tutor = achievment$a.tutor, a.hunter = achievment$a.hunter,
                         a.plunderer = achievment$a.plunderer, a.careerist = achievment$a.careerist, a.breeder = achievment$a.breeder,
                         a.architect = achievment$a.architect, a.shipbuilder = achievment$a.shipbuilder, a.sailor = achievment$a.sailor,
                         a.fowler = achievment$a.fowler,
                         ### Secondary features
                         arena.rate = as.numeric(as.character(a$arena.wins))/as.numeric(as.character(a$arena.loses)),
                         equip.rate = as.numeric(as.character(a$equip.level))/as.numeric(as.character(a$level))
                         )
        indx <- sapply(df, is.factor)
        df[indx] <- lapply(df[indx], function(x) as.numeric(as.character(x)))
        return(df)}
      if(file.exists("DungeonsDB.csv")) {
        write.table(a, "DungeonsDB.csv", sep = ";", col.names = F, append = T, row.names = F)
      } else {
        write.table(a, "DungeonsDB.csv", sep = ";", col.names = T, row.names = F)
      }
    }
  } else { print("Status not OK"); write.table(data.frame(time = Sys.time(), godname = god, js.status = js$status_code, html.status = html$status_code), file = "log.csv", sep = ";", row.names = F, append = T, col.names = F)}
  Sys.sleep(5)
}
#setwd("/home/capsula/work/_archive/GodvilleMonitor")
### god names for monitoring
load("godnames")
sapply(godnames, monitor)

################# TOPS
load("godnames.tops")
sapply(gsub(" ", replacement = "%20", godnames.tops), monitor)


#### ОБРАБОТКА ОШИБОК
if(file.exists("log.csv")) {
  err = read.csv("log.csv", sep = ";", header = F, stringsAsFactors = F)
  names(err) <- c("time", "godname", "js.code", "html.code")
  file.remove("log.csv")
  sapply(err$godname, monitor) }