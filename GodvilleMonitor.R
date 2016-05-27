setwd("/home/capsula/work/_archive/GodvilleMonitor")

library("jsonlite")
monitor <- function(god) {
  js = fromJSON(sprintf("http://godville.net/gods/api/%s.json", god))

  a <- rbind(c(time=as.numeric(as.POSIXct(Sys.time()))
               ,unlist(js[c("gold_approx"
                            , "level"
                            , "alignment"
                            , "wood_cnt"
                            , "health"
                            , "exp_progress"
                            , "quest_progress")])))
  
  
  if(file.exists(sprintf("%s2.csv", god))) {
    write.table(a, sprintf("%s2.csv", god),
                sep = ";", col.names = F, append = T, row.names = F)
  } else {
    write.table(a, sprintf("%s2.csv", god),
                sep = ";", col.names = T, row.names = F)
  }
}

monitor("capsula")
