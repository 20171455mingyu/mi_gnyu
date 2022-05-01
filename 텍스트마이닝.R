# 2022-1 텍스트마이닝 중간고사

# 패키지 설치
install.packages("stringr")
install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("multilinguer")
install_jdk()
install.packages('rJava', type = 'binary'); library(rJava);.jinit();rstudioapi::restartSession()
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP", upgrade = "never", INSTALL_opts = c("--no-multiarch"))
install.packages("stringr")
install.packages("showtext")
install.packages("tidyr")
install.packages("readr")
install.packages("textclean")

# 패키지 부착
library(stringr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(multilinguer)
library(KoNLP)
library(stringr)
library(showtext)
library(tidyr)
library(readr)
library(textclean)

# 문제 2번 가장 많이 사용된 단어 추출, 그래프 생성
mid_exam <- readLines("test_news.txt", encoding = "UTF-8")  # 텍스트 파일 불러오기 / 한글 인코딩
head(mid_exam)

new = mid_exam %>% str_replace_all("[^가-R]", " ") %>% str_squish() %>% as_tibble() # 불필요한 문자, 공백 제거, tibbke 구조 변경
head(new)

word_space <- new %>% unnest_tokens(input = value, output = word, token = "words")  # 토큰화, 단어 빈도 분석으로 words(띄어쓰기) 옵션 사용
word_space

word_space <- word_space %>% count(word, sort = T)
word_space

word_space = word_space %>% filter(str_count(word) > 1) # 한글자 단어는 크게 의미 없기에 그 이상의 단어가 출력되게 설정
word_space

top20 <- word_space %>% head(20)  # 가장 많이 사용된 단어 20가지 추출
top20

ggplot(top20, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() # 막대그래프로 시각화하기
ggplot(top20, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() + geom_text(aes(label = n), hjust = -0.3) + labs(title = "기사 단어사용 빈도", x = NULL, y = NULL) + theme(title = element_text(size = 20))  # 막대 그래프의 간격(hjust), 타이틀과 크기, xy 축의 이름 제거를 통해 그래프 정리

# 문제 3번 오즈비 또는 TF-IDF 활용 분석
bot_exam <- readLines("test_news.txt", encoding = "UTF-8")  # 데이터 불러오기
bot <- bot_exam %>% as_tibble() %>% mutate(president = "bot")

sup_exam <- readLines("test_mews.txt", encoding = "UTF-8")  # 데이터 불러오기
sup <- sup_exam %>% as_tibble() %>% mutate(president = "sup")

test_news <- bind_rows(bot, sup) %>% select(president, value) # 두 데이터를 행 방향(bind_row)으로 결합, select()로 변수 순서 변경
head(test_news)
tail(test_news)

news <- test_news %>% mutate(value = str_replace_all(value, "[^가-R]", " "), value = str_squish(value))  # 전처리, 토큰화 / 공백, 문자 제거
news

news <- news %>% unnest_tokens(input = value, output = word, token = extractNoun)
news

freq <- news %>% count(president, word) %>% filter(str_count(word) > 1) # 집단별 단어 빈도 구하기 / 두글자 이상
freq

freq_wide <- freq %>% pivot_wider(names_from = president, values_from = n, values_fill = list(n = 0)) # 연설문 단어 빈도를 wide form으로 변환
freq_wide

freq_wide <- freq_wide %>% mutate(ratio_bot = (bot) / (sum(bot)), ratio_sup = ((sup) / (sum(sup)))) # 각 단어의 빈도 / 모든 단어 빈도의 합
freq_wide

freq_wide <- freq_wide %>% mutate(ratio_bot = ((bot + 1) / (sum(bot + 1))), ratio_sup = ((sup + 1) / (sum(sup + 1)))) # 빈도 0은 계산시 0으로 나누는 경우가 발생하기에 전부 +1을 해준다
freq_wide

freq_wide <- freq_wide %>% mutate(odds_ratio = ratio_bot / ratio_sup) # 오즈비 변수 추가(한 텍스트의 단어 비중을 다른 텍스트의 단어 비중으로 나눔)
freq_wide

freq_wide %>% arrange(-odds_ratio)  # bot에서 상대적 비중이 클 수록 1보다 크고, 반대의 경우 1보다 작은 값, 같으면 1이다. / 내림차순 정렬
freq_wide %>% arrange(odds_ratio) # bot에서 상대적 비중이 클 수록 1보다 크고, 반대의 경우 1보다 작은 값, 같으면 1이다. / 오름차순 정렬
freq_wide %>% arrange(abs(1 - odds_ratio))  # bot에서 상대적 비중이 클 수록 1보다 크고, 반대의 경우 1보다 작은 값, 같으면 1이다. / 1에 가까운 정렬

freq_wide <- freq_wide %>% mutate(odds_ratio = ((bot + 1) / sum(bot + 1)) / ((sup + 1) / sum(sup + 1)))
freq_wide

# 문제 4번 감정사전 적용 후 감정 경향 분석
raw_news_comment <- read_csv("ko_test_label.csv")  # csv 데이터 불러오기
raw_news_comment

news_comment <- raw_news_comment %>% mutate(id = row_number(), reply = str_squish(replace_html(reply))) # 고유번호 생성, 특수문자 제거
glimpse(news_comment) # 데이터 구조 확인

word_comment <- news_comment %>% unnest_tokens(input = reply, output = word, token = "words", drop = F)  # 단어 기준 토큰화
word_comment %>% select(word, reply)

dic <- read_csv("knu_sentiment_lexicon.csv") # 감정사전 csv 파일 불러오기

word_comment <- word_comment %>% left_join(dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity)) # 단어기준 토큰화, 감정점수 부여하기
word_comment %>% select(word, polarity)

word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))  # 감정이 드러난 단어를 분류
word_comment %>% count(sentiment)

top10_sentiment <- word_comment %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n = 20)  # 긍정, 부정 단어 10개를 추출
top10_sentiment

ggplot(top10_sentiment, aes(x = reorder(word, n), y = n, fill = sentiment)) + geom_col() + coord_flip() + geom_text(aes(label = n), hjust = -0.3) + facet_wrap(~sentiment, scales = "free") + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + labs(x = NULL) # 추출한 10개를 그래프화

score_comment <- word_comment %>% group_by(id, reply) %>% summarise(score = sum(polarity)) %>% ungroup()  # 리뷰 별 감정 점수 구하고 계산
score_comment %>% select(score, reply)

score_comment %>% select(score, reply) %>% arrange(-score) # 감정점수 높은 긍정 댓글 찾기
score_comment %>% select(score, reply) %>% arrange(score)  # 감정점수 높은 부정 댓글 찾기

score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))  # 긍정, 부정에 따른 감정 분류
freq_score <- score_comment %>% count(sentiment) %>% mutate(ratio = n / sum(n) * 100) # 비율을 나타내어 표시
freq_score

ggplot(freq_score, aes(x = sentiment, y = n, fill = sentiment)) + geom_col() + geom_text(aes(label = n), vjust = -0.3) + scale_x_discrete(limits = c("pos", "neu", "neg"))  # 감정 분류한 항목을 그래프로 표시

# 문제 5번 감정사전 수정, 비교
score_comment %>% filter(str_detect(reply, "미친")) %>% select(reply)
score_comment %>% filter(str_detect(reply, "소름")) %>% select(reply)

dic %>% filter(word %in% c("미친", "소름")) # 단어 분류

new_dic <- dic %>% mutate(polarity = ifelse(word %in% c("미친", "소름"), 2, polarity)) # 특정 단어를 재 분류
new_dic %>% filter(word %in% c("미친", "소름"))  # 재 분류가 된 단어를 다시 확인

new_word_comment <- word_comment %>% select(-polarity) %>% left_join(new_dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))  # 수정한 감정사전으로 점수 부여
new_word_comment

new_score_comment <- word_comment %>% group_by(id, reply) %>% summarise(score = sum(polarity)) %>% ungroup()
new_score_comment %>% select(score, reply) %>% arrange(-score) # 댓글 별 감정 점수

score_comment %>% select(score, reply) %>% arrange(-score) # 감정 점수 재 확인

new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= 0, "neg", "neu")))  # 점수 별 감정분류

score_comment %>% count(sentiment) %>% mutate(ratio = n / sum(n) * 100) # 감정 범주별 빈도와 비율 계산 - 원본
new_score_comment %>% count(sentiment) %>% mutate(ratio = n / sum(n) * 100) # 감정 범주별 빈도와 비율 계산 - 수정

word = "미친|소름"
score_comment %>% filter(str_detect(reply, word)) %>% count(sentiment)  # 소름, 미친이 사용된 댓글의 감정범주 빈도 변화 - 원본
new_score_comment %>% filter(str_detect(reply, word)) %>% count(sentiment)  # 소름, 미친이 사용된 댓글의 감정범주 빈도 변화 - 수정
