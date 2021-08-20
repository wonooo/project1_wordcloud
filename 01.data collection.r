## 데이터 수집

# 21년 1월 399개 기사 수집
library(rvest)

url_21 <- c()
for(i in 1:40){
  html <- read_html(paste0('https://news.joins.com/Search/JoongangNews?page=', i,'&Keyword=%EB%B6%80%EB%8F%99%EC%82%B0&PeriodType=DirectInput&StartSearchDate=01%2F01%2F2021%2000%3A00%3A00&EndSearchDate=01%2F31%2F2021%2000%3A00%3A00&SortType=New&SearchCategoryType=JoongangNews'))
  
  u <- html_nodes(html, '.headline.mg') %>% 
    html_node('a') %>% html_attr('href')
  url_21 <- c(url_21, u)
}
length(url_21)

# url속에서 각 기사의 본문 크롤링
text_21 <- c()
news_21 <- c()

for(i in 1:length(url_21)){
  html_21 <- read_html(url_21[i])
  article_21 <- html_node(html_21, '#article_body')
  xml_remove(article_21 %>% html_nodes('div'))
  text_21 <- article_21 %>% html_text()
  news_21 <- c(news_21, trimws(text_21))
  Sys.sleep(2)
}
length(news_21)


# 20년 1월 300개 기사 수집
library(rvest)

url_20 <- c()
for(i in 1:30){
  html <- read_html(paste0('https://news.joins.com/Search/JoongangNews?page=',i,'&Keyword=%EB%B6%80%EB%8F%99%EC%82%B0&PeriodType=DirectInput&StartSearchDate=01%2F01%2F2020%2000%3A00%3A00&EndSearchDate=01%2F31%2F2020%2000%3A00%3A00&SortType=New&SearchCategoryType=JoongangNews'))
  
  u <- html_nodes(html, '.headline.mg') %>% 
    html_node('a') %>% html_attr('href')
  url_20 <- c(url_20, u)
}
length(url_20)

# url속에서 각 기사의 본문 크롤링
text_20 <- c()
news_20 <- c()

for(i in 1:length(url_20)){
  html_20 <- read_html(url_20[i])
  article_20 <- html_node(html_20, '#article_body')
  xml_remove(article_20 %>% html_nodes('div'))
  text_20 <- article_20 %>% html_text()
  news_20 <- c(news_20, trimws(text_20))
  Sys.sleep(2)
}
length(news_20)
