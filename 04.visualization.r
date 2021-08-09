## 데이터 시각화

# 20년도와 21년도의 상위 200개의 데이터셋 생성
termfreq_news_20$year <- rep(20,times = nrow(termfreq_news_20))
termfreq_news_21$year <- rep(21,times = nrow(termfreq_news_21))
news_20_200 <- head(arrange(termfreq_news_20, -freq), n=200)
news_21_200 <- head(arrange(termfreq_news_21, -freq), n=200)
setdiff(news_21_200$word, news_20_200$word)    # 21년도에만 있는 것
setdiff(news_20_200$word, news_21_200$word)    # 20년도에만 있는 것

news_200 <- rbind(news_20_200, news_21_200)

# 20년도와 21년도 모두 등장한 단어: 130개
news_20_arg <- arrange(news_20_200[news_20_200$word %in% intersect(news_20_200$word, news_21_200$word),], word)
news_21_arg <- arrange(news_21_200[news_21_200$word %in% intersect(news_20_200$word, news_21_200$word),], word)
news_inte_freq <- data.frame(word=news_20_arg$word, freq_20=news_20_arg$freq, freq_21=news_21_arg$freq)

# 21년도에 증가한 단어: 108개
news_inte_freq %>%
  mutate(plus = freq_21 - freq_20) %>%
  filter(plus > 0) %>% 
  arrange(-plus)

# 21년도에 감소한 단어: 22개
nnews_inte_freq %>%
  mutate(plus = freq_21 - freq_20) %>%
  filter(plus < 0) %>% 
  arrange(plus)



# 증가단어 30개, 감소단어 15개
# 양쪽으로 그려지는 그래프
library(ggplot2)
news_inte_plus <- news_inte_freq %>% 
  mutate(plus = freq_21 - freq_20, years = ifelse(plus>=0, '21년 증가', '21년 감소')) 

ggplot(data=news_inte_plus, aes(x=word, y=plus, fil=years)) +
  geom_col(data= news_inte_plus %>% arrange(-plus) %>% head(n=30), fill=rgb(47,102,114, max=255)) +
  geom_col(data= news_inte_plus %>% arrange(plus) %>% head(n=15), fill='#d2b4a9') + coord_flip() +
  labs(x="", y="", title='20년도에 비해 21년도에 증감한 단어') +
  theme(plot.title = element_text(face="bold",hjust=0.5)) +
  geom_text(data= news_inte_plus %>% arrange(-plus) %>% head(n=30), aes(label=plus), hjust=-.1) +
  geom_text(data= news_inte_plus %>% arrange(plus) %>% head(n=15), aes(label=plus), hjust=1)



# -------------------- 기사의 수가 다르기때문에 표준화를 실시
news_z <- news_inte_freq %>% mutate(z_20 = round((freq_20-mean(freq_20))/sd(freq_20), 2),
                                    z_21 = round((freq_21-mean(freq_21))/sd(freq_21), 2)) %>%
  select(word, z_20, z_21)

# 21년도에 증가한 단어: 79개
news_z %>%
  mutate(plus = z_21 - z_20) %>%
  filter(plus > 0) %>% 
  arrange(-plus)

# 21년도에 감소한 단어: 50개
news_z %>%
  mutate(plus = z_21 - z_20) %>%
  filter(plus < 0) %>% 
  arrange(plus)

# 표준화된 단어들에 대한 그래프(양쪽으로 그려지는 그래프)
library(ggplot2)
news_z_plus <- news_z %>% 
  mutate(plus = z_21 - z_20, years = ifelse(plus>=0, '21년 증가', '21년 감소'))

ggplot(data=news_z_plus, aes(x=word, y=plus, fil=years)) +
  geom_col(data= news_z_plus %>% arrange(-plus) %>% head(n=30), fill=rgb(47,102,114, max=255)) +
  geom_col(data= news_z_plus %>% arrange(plus) %>% head(n=15), fill='#d2b4a9') + coord_flip() +
  labs(x="", y="", title='20년도에 비해 21년도에 증감한 단어_표준화') +
  theme(plot.title = element_text(face="bold",hjust=0.5)) +
  geom_text(data= news_z_plus %>% arrange(-plus) %>% head(n=30), aes(label=plus), hjust=-.1, vjust=0.3, size=3.5) +
  geom_text(data= news_z_plus %>% arrange(plus) %>% head(n=15), aes(label=plus), hjust=1, vjust=0.3, size=3.5)



# 표준화된 데이터로 21년도에 언급이 많아진 단어를 막대그래프로 그래보기
# 특징을 가지는 단어들에 대해 색상을 달리하여 그리기

# 21년도에 등장한 단어
setdiff(news_21_200$word, news_20_200$word)  
news_21_200[news_21_200$word %in% setdiff(news_21_200$word, news_20_200$word),]
nrow(news_21_200[news_21_200$word %in% setdiff(news_21_200$word, news_20_200$word),])
head(news_21_200[news_21_200$word %in% setdiff(news_21_200$word, news_20_200$word),], n=50)

# 전체 
ggplot(data=news_21_200[news_21_200$word %in% setdiff(news_21_200$word, news_20_200$word),],
       aes(x=word, y=freq)) +
  geom_col(fill=rgb(47,102,114, max=255)) + coord_flip() +
  labs(x="", y="") +
  theme(plot.title = element_text(face="bold",hjust=0.5)) +
  geom_text(data=news_21_200[news_21_200$word %in% setdiff(news_21_200$word, news_20_200$word),], 
            aes(label=freq), hjust=-.1, vjust= 0.3, size=3.5) 


# 주식
ggplot(data=news_21_200[news_21_200$word %in% setdiff(news_21_200$word, news_20_200$word),],
       aes(x=word, y=freq)) +
  geom_col(fill=rgb(47,102,114, max=255)) + coord_flip() +
  labs(x="", y="") +
  theme(plot.title = element_text(face="bold",hjust=0.5)) +
  geom_text(data=news_21_200[news_21_200$word %in% setdiff(news_21_200$word, news_20_200$word),], 
            aes(label=freq), hjust=-.1, vjust= 0.3, size=3.5)  +
  geom_col(data=news_21_200[news_21_200$word %in% c('펀드', '코스피', '증시', '증권사', '주식', '주가', '수익', '유동성'),], fill='#d2b4a9')

# 코로나
ggplot(data=news_21_200[news_21_200$word %in% setdiff(news_21_200$word, news_20_200$word),],
       aes(x=word, y=freq)) +
  geom_col(fill=rgb(47,102,114, max=255)) + coord_flip() +
  labs(x="", y="") +
  theme(plot.title = element_text(face="bold",hjust=0.5)) +
  geom_text(data=news_21_200[news_21_200$word %in% setdiff(news_21_200$word, news_20_200$word),], 
            aes(label=freq), hjust=-.1, vjust= 0.3, size=3.5)  +
  geom_col(data=news_21_200[news_21_200$word %in% c('확산', '코로나바이러스', '재난지원금', '백신', '방역', '감염증', '코로나', '접종'),], fill='#d2b4a9')

# 정치
ggplot(data=news_21_200[news_21_200$word %in% setdiff(news_21_200$word, news_20_200$word),],
       aes(x=word, y=freq)) +
  geom_col(fill=rgb(47,102,114, max=255)) + coord_flip() +
  labs(x="", y="") +
  theme(plot.title = element_text(face="bold",hjust=0.5)) +
  geom_text(data=news_21_200[news_21_200$word %in% setdiff(news_21_200$word, news_20_200$word),], 
            aes(label=freq), hjust=-.1, vjust= 0.3, size=3.5)  +
  geom_col(data=news_21_200[news_21_200$word %in% c('후보자', '취임', '지지층', '지지율', '여론조사', '여당', '야당', '야권',
                                                    '부총리', '보궐선거', '대선', '국민의힘', '국민의당', '정치권', '당선', '기획재정부', '기재부'),], fill='#d2b4a9')



# 표준화된 데이터로 21년도에 언급이 적어진 단어를 막대그래프로 그래보기
# 특징을 가지는 단어들에 대해 색상을 달리하여 그리기

# 21년도에 등장하지 않은 단어
setdiff(news_20_200$word, news_21_200$word)  
news_20_200[news_20_200$word %in% setdiff(news_20_200$word, news_21_200$word) ,]
nrow(news_20_200[news_20_200$word %in% setdiff(news_20_200$word, news_21_200$word) ,])

head(news_20_200[news_20_200$word %in% setdiff(news_20_200$word, news_21_200$word) ,], n=50)

# 전체
ggplot(data=news_20_200[news_20_200$word %in% setdiff(news_20_200$word, news_21_200$word), ],
       aes(x=word, y=freq)) +
  geom_col(fill=rgb(47,102,114, max=255)) + coord_flip() +
  labs(x="", y="") +
  geom_text(data=news_20_200[news_20_200$word %in% setdiff(news_20_200$word, news_21_200$word), ], 
            aes(label=freq), hjust=-.1, vjust= 0.3, size=3)

# 대출, 집
ggplot(data=news_20_200[news_20_200$word %in% setdiff(news_20_200$word, news_21_200$word), ],
       aes(x=word, y=freq)) +
  geom_col(fill=rgb(47,102,114, max=255)) + coord_flip() +
  labs(x="", y="") +
  geom_text(data=news_20_200[news_20_200$word %in% setdiff(news_20_200$word, news_21_200$word), ], 
            aes(label=freq), hjust=-.1, vjust= 0.3, size=3) +
  geom_col(data=news_20_200[news_20_200$word %in% c('집주인', '주택담보대출', '전세대출', '저금리', '이사', '월세', '세입자', '땅값', '고가주택', '토지', '부동산시장', '공시지가'),], fill='#d2b4a9')

# 세금
ggplot(data=news_20_200[news_20_200$word %in% setdiff(news_20_200$word, news_21_200$word), ],
       aes(x=word, y=freq)) +
  geom_col(fill=rgb(47,102,114, max=255)) + coord_flip() +
  labs(x="", y="") +
  geom_text(data=news_20_200[news_20_200$word %in% setdiff(news_20_200$word, news_21_200$word), ], 
            aes(label=freq), hjust=-.1, vjust= 0.3, size=3) +
  geom_col(data=news_20_200[news_20_200$word %in% c('세무사', '국세청', '과세', '종합부동산세', '상속'),], fill='#d2b4a9')

# 제도
ggplot(data=news_20_200[news_20_200$word %in% setdiff(news_20_200$word, news_21_200$word), ],
       aes(x=word, y=freq)) +
  geom_col(fill=rgb(47,102,114, max=255)) + coord_flip() +
  labs(x="", y="") +
  geom_text(data=news_20_200[news_20_200$word %in% setdiff(news_20_200$word, news_21_200$word), ], 
            aes(label=freq), hjust=-.1, vjust= 0.3, size=3) +
  geom_col(data=news_20_200[news_20_200$word %in% c('허가제', '총리', '제도', '재정', '정책실장', '장기보유특별공제', '개정'),], fill='#d2b4a9')

