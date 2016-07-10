library("jsonlite"); library("XML"); library("httr")
#setwd("/home/capsula/work/_archive/GodvilleMonitor")
source("monitor.function.r")
### god names for monitoring
load("godnames")
sapply(godnames, monitor)
#### ОБРАБОТКА ОШИБОК
if(file.exists("log.csv")) {
  err = read.csv("log.csv", sep = ";", header = F, stringsAsFactors = F)
  names(err) <- c("time", "godname", "js.code", "html.code")
  file.remove("log.csv")
  sapply(err$godname, monitor) }