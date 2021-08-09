## 데이터 분석
## 품사를 테그하고 corpus단위의 단어 형성
## 불용어를 제거하여 워드클라우드 그리기

# ---------------------- 분석을 위한 말뭉치 생성 및 코퍼스 단위의 전처리
library(tm)
library(KoNLP)
useSejongDic()

# 말뭉치 생성을 위한 100개의 뉴스를 하나의 벡터로 변환
news_21_2 <- paste(news_21_1, collapse = ' ')

# 품사 태깅
pos_news_21 <- SimplePos09(news_21_1)
pos_news_21
str(pos_news_21)

# 명사(N)만 추출
library(stringr)
str_match(unlist(pos_news_21), '([가-힣]+)/N')   # N가 없는 문자들만 하나의 그룹으로 뽑아줌
str_match(unlist(pos_news_21), '([가-힣]+)/N')[,2]
news_21_word <- as.vector(na.omit(str_match(unlist(pos_news_21), '([가-힣]+)/N')[,2]))

# 두글자 이상의 단어만 추출
news_21_word2 <- Filter(function(x){nchar(x) >= 2}, news_21_word)
news_21_word2

# 단어 빈도로 확인
head(sort(table(news_21_word2), decreasing = T), n=200)
View(sort(table(news_21_word2), decreasing = T))

# 불용어단어 제거
stop_words_news <- readLines("c:/data/stop_words_news.txt", encoding = 'UTF-8')
str(stop_words_news)

word_new <- c()
for(i in news_21_word2){
  if(!i %in% stop_words_news){
    word_new <- c(word_new, i)
  }
}
head(sort(table(word_new), decreasing = T))

# corpus 변환
word_new2 <- paste(word_new, collapse = ' ')
corp_news_21 <- VCorpus(VectorSource(word_new2))
inspect(corp_news_21)          
corp_news_21[[1]]$content

# corpus 단어 정제
str_extract_all(corp_news_21, '대통령[[:alpha:]]{0,}')
str_extract_all(corp_news_21, '수요[[:alpha:]]{0,}')
str_extract_all(corp_news_21, '전세|전셋집')
str_extract_all(corp_news_21, '서울주택[[:alpha:]]{0,}')
str_extract_all(corp_news_21, '서울[[:alpha:]]{0,2}')
str_extract_all(corp_news_21, '투자[[:alpha:]]{0,}')

corp_news_21_2 <- tm_map(corp_news_21, content_transformer(function(x) gsub('대통령[[:alpha:]]{0,}', '대통령', x)))
corp_news_21_2 <- tm_map(corp_news_21_2, content_transformer(function(x) gsub('민주당[[:alpha:]]{0,}|더불어민주당[[:alpha:]]{0,}', '더불어민주당', x)))
corp_news_21_2 <- tm_map(corp_news_21_2, content_transformer(function(x) gsub('수요[[:alpha:]]{0,}', '수요', x)))
corp_news_21_2 <- tm_map(corp_news_21_2, content_transformer(function(x) gsub('서울주택[[:alpha:]]{0,}', '서울주택도시공사', x)))
corp_news_21_2 <- tm_map(corp_news_21_2, content_transformer(function(x) gsub('서울[[:alpha:]]{0,2}', '서울시', x)))
corp_news_21_2 <- tm_map(corp_news_21_2, content_transformer(function(x) gsub('투자[[:alpha:]]?', '투자', x)))

# 매트릭스 변환
corp_news_21_dtm <- DocumentTermMatrix(corp_news_21_2, control = list(wordLengths = c(2, Inf)))
inspect(corp_news_21_dtm)  # 단어 수: 16881

# 빈도 추출
termfreq_news_21 <- data.frame(freq=colSums(as.matrix(corp_news_21_dtm)))
termfreq_news_21$word <- rownames(termfreq_news_21)
rownames(termfreq_news_21) <- NULL
termfreq_news_21 <- termfreq_news_21[,c(2,1)]
arrange(termfreq_news_21, -freq)

# 간단한 wordcloud
library(wordcloud2)
wordcloud2(termfreq_news_21)

wordcloud(termfreq_news_21$word, termfreq_news_21$freq, 
          random.order = F, scale = c(5, 0.5), max.words = 200,
          colors = brewer.pal(10, 'Set1')[c(2,3,4,5,7)]) 

wordcloud(termfreq_news_21$word, termfreq_news_21$freq, 
          random.order = F, random.color = F,
          scale = c(5, 0.5), min.freq = 30,
          colors = brewer.pal(10, 'Set1')[c(2,3,4,5,7)]) 
