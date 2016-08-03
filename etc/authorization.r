rm(list = ls())
library(XML);library(RCurl)

gen_a_parameter <- function(){
  
  sapply(c(3,5),function(n){paste(sample(c(LETTERS,letters,0:9),n,replace = T),collapse = '')})
  
}

loginurl  <- 'https://godville.net/login/login'
logouturl <- 'https://godville.net/blog/logout'

dataurl   <- 'https://godville.net/fbh/feed'

LOGIN     <- 'login'
NAME      <- 'name'
PASSWORD  <- 'password'

login.params    <- list('username' = LOGIN, 
                        'password' = PASSWORD, 
                        'commit'   = 'Login')

agent="curl/7.35.0" #or whatever 

curl = getCurlHandle()
curlSetOpt(curl = curl,  
           #cookiejar = "gv-cookies.txt",         
           cookiejar = "",
           useragent = agent, 
           followlocation = F)

# LOGIN
html = postForm(loginurl, 
                .params = login.params, 
                curl = curl,
                style= 'post')
# QUERY DATA
data.params    <- list('a' = '7hdRywCvjEbxoW9YSaLT7vKDxbWsW')

html = postForm(dataurl, 
                .params = data.params, 
                curl = curl,
                style= 'post')
jsonlite::fromJSON(html)

require(stringi)
rand.dungeons <- stri_rand_strings(n=10000, length=5, pattern="[A-Za-z0-9]")

i = 0
for(x in rand.dungeons){
  log = paste0("https://godville.net/duels/log/", x)
  d.cont <- htmlParse(getURLContent(log, curl = curl), encoding = "UTF-8")
  if(length(xpathSApply(d.cont, "//*[@id='page_wrapper']/div[1]/a", xmlValue) != 0)) {
    print(xpathSApply(d.cont, "//*[@id='page_wrapper']/div[1]/a", xmlValue))
    break
  }
  cat(i, x, "\n")
  i=i+1
  Sys.sleep(0.5)
}

log = "https://godville.net/duels/log/gnl20"
d.cont       <- htmlParse(getURLContent(log, curl = curl), encoding = "UTF-8")
dungeon.name <- xpathSApply(d.cont, "//*[@id='page_wrapper']/div[1]/a", xmlValue)
dungeon.res  <- length(grep("Герои потрошат сокровищницу и делят награбленное. ", xpathSApply(d.cont, "//*[@id='last_items_arena']/fieldset", xmlValue)))
gods.list    <- sapply(strsplit(as.character(xpathSApply(d.cont, "//*[@id='hero1_info']/fieldset/div/label/a/@href")), "/"), "[[", 3)
gods.json    <- paste0("https://godville.net/gods/api/",gods.list,".json")
gods.table   <- paste0("http://godville.net/gods/",gods.list)

lapply(gods.table, function(x) readHTMLTable(x, header = F))