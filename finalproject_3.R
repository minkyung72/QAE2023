install.packages("RSelenium")
install.packages("dplyr")
install.packages("stringr")
install.packages("rvest")
install.packages("KeyboardSimulator")
install.packages("lubridate")
install.packages("jsonlite")
install.packages("glue")
library(jsonlite)
library(tidyverse)
library(lubridate)
library(KeyboardSimulator)
library(RSelenium)
library(dplyr)
library(stringr)
library(rvest)
library(wdman)
library(httr)
library(readr)
library(data.table)
library(bizdays)
library(readxl)
library(glue)
# install.packages(c("av", "gifski")) # 이 두 패키지가 설치되어야 정상 실행됨.
install.packages("gganimate")
install.packages("gapminder")
library(gganimate)
library(gapminder)
setwd("C:/Users/sec/Desktop/수업/3. 경제데이터분석/project")

# 1. 배출권 거래가격 데이터 수집하기
# http://data.krx.co.kr/contents/MDC/MDI/mdiLoader/index.cmd?menuId=MDC0201060201

# business days 구하기
year3 <- read_xls("data_2018.xls")
year4 <- read_xls("data_2019.xls")
year5 <- read_xls("data_2020.xls")
year6 <- read_xls("data_2021.xls")
year7 <- read_xls("data_2022.xls")
holiday <- rbind(year3, year4, year5, year6, year7) %>% .[,1] 
holidays <- holiday %>% as.data.frame()
holidayss <- holidays$`일자 및 요일`

start <- glue('2018-01-01')
end <- glue('2023-06-01')
krx_cal <- create.calendar(name='krx_cal', holidays=holidayss, weekdays=c("saturday", "sunday"), start.date = start, end.date= end)
krx_cal
workday <- bizseq(from=start, to=end, cal='krx_cal')
workday

date.list <- workday %>% str_remove_all('-')
date.list


# KRX 시세 파악하기
Stack <- NULL
for (i in date.list) {
  gen_otp_url = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
  gen_otp_data = list(
    locale= "ko_KR",
    trdDd= i,
    share= "1",
    money= "1",
    csvxls_isNo= "false",
    name= "fileDown",
    url= "dbms/MDC/STAT/standard/MDCSTAT15601"
  )
  otp = POST(gen_otp_url, query = gen_otp_data) %>% read_html() %>% html_text()
  
  down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
  down_eprice = POST(down_url, query = list(code = otp), add_headers(referer = gen_otp_url)) %>% read_html(encoding = 'EUC-KR') %>%  html_text() %>% read_csv()
  
  date <- i
  down_emprice <- cbind(date, down_eprice)
  
  Stack <- rbind(Stack, down_emprice)
  Sys.sleep(1)
}

e_price <- Stack %>% subset(시가 != 0) %>% as.data.frame()
KAU <- e_price %>% filter(종목명 %in% c("KAU17", "KAU18", "KAU19"))
KOC<- e_price %>% filter(종목명 %in% "KOC")

head(KAU)

write.csv(Stack, 'emission_price.csv', fileEncoding = 'EUC-KR')


# 2. ETRS 기업별 배출인증량 수집하기
# i 1차,2차, 3차 j 년도

gen_etrs_url = 'https://etrs.gir.go.kr/home/index.do?menuId=20'
gen_etrs_data = list(
  pagerOffset= "0",
  maxPageItems= "10",
  maxIndexPages= "10",
  menuId= "20",
  condition.plPeriDgr= "2",
  condition.infoOpenYn= "Y",
  condition.pfYy= "2018",
  csvYn= "Y")

down_ets = POST(gen_etrs_url, query = gen_etrs_data) %>% read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv()

Stack < rbind(Stack, down_ets)
write.csv(Stack,"emission.csv", fileEncoding = 'EUC-KR')   

# 3. 네이버 기사 수집
install.packages("xml2")  # Install the xml2 package
library(xml2)
library(dplyr)
library(stringr)
library(httr)
library(XML)
library(jsonlite)
library(writexl)
library(readxl)
## Naver News 검색
searchUrl <- "https://openapi.naver.com/v1/search/news.json"
client_id <- "M4g9rW2aSU0zNVXbp0Ve"
client_secret <- "gfr6xh20HF"

search.word <- "배출권" %>% 
  enc2utf8() %>% 
  URLencode()

url <- str_c(searchUrl, 
             "?query=", search.word, 
             "&display=50")

res <- GET(url, 
           add_headers("X-Naver-Client-Id"=client_id, 
                       "X-Naver-Client-Secret"=client_secret))
doc <- toString(res)
return <- fromJSON(doc)
tab <- return$items

glimpse(tab)

write_xlsx(tab, "NAVER_API_News_Search.xlsx") # Excel Data로 저장


# 4. Google scholar 수집
# (1) 배출권
library(rvest)
library(dplyr)
library(stringr)
URL <- "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=%EB%B0%B0%EC%B6%9C%EA%B6%8C&btnG="
res <- read_html(URL)

# title
pattern <- ".gs_rt a"
title <- res %>% 
  html_nodes(pattern) %>% 
  html_text()
title

# journal
pattern <- ".gs_a"
citation <- res %>% 
  html_nodes(pattern) %>% 
  html_text()
citation

# link
pattern <- ".gs_rt a"
link <- res %>% 
  html_nodes(pattern) %>% 
  html_attr("href") %>% 
  str_c("https://scholar.google.com", .)
link

table <- cbind(title, author, journal, publication_year, link)
View(table)



