## 데이터 전처리
# stringr 패키지를 이용

library(stringr)

# 1. 기자이름, 이메일 제거
str_extract_all(news_21, '([a-zA-Z0-9._]+)@([a-zA-Z0-9.]+)', simplify = T)
str_extract_all(news_21, '([가-힣·]{2,8})\\ 기자', simplify = T)

news_21_1 <- str_replace_all(news_21, '([a-zA-Z0-9._]+)@([a-zA-Z0-9.]+)', '')
news_21_1 <- str_replace_all(news_21_1, '([가-힣·]{2,8})\\ 기자', '')

# 2. 숫자 제거(2억 3000만원, 3%)
str_extract_all(news_21_1, '[[:digit:]]{1,}[[:alpha:]]{0,}')
str_extract_all(news_21_1, '[[:punct:]]{0,}[[:digit:]]{1,}[[:punct:]]{1,}[[:digit:]]{0,}[[:punct:]]{0,}')

news_21_1 <- str_replace_all(news_21_1, '[[:digit:]]{1,}[[:alpha:]]{0,}',' ')
news_21_1 <- str_replace_all(news_21_1, '[[:punct:]]{0,}[[:digit:]]{1,}[[:punct:]]{1,}[[:digit:]]{0,}[[:punct:]]{0,}',' ')
news_21_1[100]

# 3. 따옴표 제거
str_extract_all(news_21_1, '(\\d{2,3})?(-|\\))?\\d{3,4}-\\d{4,}', simplify = T)   # 전화번호 -> 제거
str_extract_all(news_21_1, '[[:punct:]]')     # 특수문자만 추출

news_21_1 <- str_replace_all(news_21_1, '[[:punct:]]',' ')