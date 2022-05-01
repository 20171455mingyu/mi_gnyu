# 2022-1 ÅØ½ºÆ®¸¶ÀÌ´× Áß°£°í»ç

# ÆĞÅ°Áö ¼³Ä¡
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

# ÆĞÅ°Áö ºÎÂø
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

# ¹®Á¦ 2¹ø °¡Àå ¸¹ÀÌ »ç¿ëµÈ ´Ü¾î ÃßÃâ, ±×·¡ÇÁ »ı¼º
mid_exam <- readLines("test_news.txt", encoding = "UTF-8")  # ÅØ½ºÆ® ÆÄÀÏ ºÒ·¯¿À±â / ÇÑ±Û ÀÎÄÚµù
head(mid_exam)

new = mid_exam %>% str_replace_all("[^°¡-ÆR]", " ") %>% str_squish() %>% as_tibble() # ºÒÇÊ¿äÇÑ ¹®ÀÚ, °ø¹é Á¦°Å, tibbke ±¸Á¶ º¯°æ
head(new)

word_space <- new %>% unnest_tokens(input = value, output = word, token = "words")  # ÅäÅ«È­, ´Ü¾î ºóµµ ºĞ¼®À¸·Î words(¶ç¾î¾²±â) ¿É¼Ç »ç¿ë
word_space

word_space <- word_space %>% count(word, sort = T)
word_space

word_space = word_space %>% filter(str_count(word) > 1) # ÇÑ±ÛÀÚ ´Ü¾î´Â Å©°Ô ÀÇ¹Ì ¾ø±â¿¡ ±× ÀÌ»óÀÇ ´Ü¾î°¡ Ãâ·ÂµÇ°Ô ¼³Á¤
word_space

top20 <- word_space %>% head(20)  # °¡Àå ¸¹ÀÌ »ç¿ëµÈ ´Ü¾î 20°¡Áö ÃßÃâ
top20

ggplot(top20, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() # ¸·´ë±×·¡ÇÁ·Î ½Ã°¢È­ÇÏ±â
ggplot(top20, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() + geom_text(aes(label = n), hjust = -0.3) + labs(title = "±â»ç ´Ü¾î»ç¿ë ºóµµ", x = NULL, y = NULL) + theme(title = element_text(size = 20))  # ¸·´ë ±×·¡ÇÁÀÇ °£°İ(hjust), Å¸ÀÌÆ²°ú Å©±â, xy ÃàÀÇ ÀÌ¸§ Á¦°Å¸¦ ÅëÇØ ±×·¡ÇÁ Á¤¸®

# ¹®Á¦ 3¹ø ¿ÀÁîºñ ¶Ç´Â TF-IDF È°¿ë ºĞ¼®
bot_exam <- readLines("test_news.txt", encoding = "UTF-8")  # µ¥ÀÌÅÍ ºÒ·¯¿À±â
bot <- bot_exam %>% as_tibble() %>% mutate(president = "bot")

sup_exam <- readLines("test_mews.txt", encoding = "UTF-8")  # µ¥ÀÌÅÍ ºÒ·¯¿À±â
sup <- sup_exam %>% as_tibble() %>% mutate(president = "sup")

test_news <- bind_rows(bot, sup) %>% select(president, value) # µÎ µ¥ÀÌÅÍ¸¦ Çà ¹æÇâ(bind_row)À¸·Î °áÇÕ, select()·Î º¯¼ö ¼ø¼­ º¯°æ
head(test_news)
tail(test_news)

news <- test_news %>% mutate(value = str_replace_all(value, "[^°¡-ÆR]", " "), value = str_squish(value))  # ÀüÃ³¸®, ÅäÅ«È­ / °ø¹é, ¹®ÀÚ Á¦°Å
news

news <- news %>% unnest_tokens(input = value, output = word, token = extractNoun)
news

freq <- news %>% count(president, word) %>% filter(str_count(word) > 1) # Áı´Üº° ´Ü¾î ºóµµ ±¸ÇÏ±â / µÎ±ÛÀÚ ÀÌ»ó
freq

freq_wide <- freq %>% pivot_wider(names_from = president, values_from = n, values_fill = list(n = 0)) # ¿¬¼³¹® ´Ü¾î ºóµµ¸¦ wide formÀ¸·Î º¯È¯
freq_wide

freq_wide <- freq_wide %>% mutate(ratio_bot = (bot) / (sum(bot)), ratio_sup = ((sup) / (sum(sup)))) # °¢ ´Ü¾îÀÇ ºóµµ / ¸ğµç ´Ü¾î ºóµµÀÇ ÇÕ
freq_wide

freq_wide <- freq_wide %>% mutate(ratio_bot = ((bot + 1) / (sum(bot + 1))), ratio_sup = ((sup + 1) / (sum(sup + 1)))) # ºóµµ 0Àº °è»ê½Ã 0À¸·Î ³ª´©´Â °æ¿ì°¡ ¹ß»ıÇÏ±â¿¡ ÀüºÎ +1À» ÇØÁØ´Ù
freq_wide

freq_wide <- freq_wide %>% mutate(odds_ratio = ratio_bot / ratio_sup) # ¿ÀÁîºñ º¯¼ö Ãß°¡(ÇÑ ÅØ½ºÆ®ÀÇ ´Ü¾î ºñÁßÀ» ´Ù¸¥ ÅØ½ºÆ®ÀÇ ´Ü¾î ºñÁßÀ¸·Î ³ª´®)
freq_wide

freq_wide %>% arrange(-odds_ratio)  # bot¿¡¼­ »ó´ëÀû ºñÁßÀÌ Å¬ ¼ö·Ï 1º¸´Ù Å©°í, ¹İ´ëÀÇ °æ¿ì 1º¸´Ù ÀÛÀº °ª, °°À¸¸é 1ÀÌ´Ù. / ³»¸²Â÷¼ø Á¤·Ä
freq_wide %>% arrange(odds_ratio) # bot¿¡¼­ »ó´ëÀû ºñÁßÀÌ Å¬ ¼ö·Ï 1º¸´Ù Å©°í, ¹İ´ëÀÇ °æ¿ì 1º¸´Ù ÀÛÀº °ª, °°À¸¸é 1ÀÌ´Ù. / ¿À¸§Â÷¼ø Á¤·Ä
freq_wide %>% arrange(abs(1 - odds_ratio))  # bot¿¡¼­ »ó´ëÀû ºñÁßÀÌ Å¬ ¼ö·Ï 1º¸´Ù Å©°í, ¹İ´ëÀÇ °æ¿ì 1º¸´Ù ÀÛÀº °ª, °°À¸¸é 1ÀÌ´Ù. / 1¿¡ °¡±î¿î Á¤·Ä

freq_wide <- freq_wide %>% mutate(odds_ratio = ((bot + 1) / sum(bot + 1)) / ((sup + 1) / sum(sup + 1)))
freq_wide

# ¹®Á¦ 4¹ø °¨Á¤»çÀü Àû¿ë ÈÄ °¨Á¤ °æÇâ ºĞ¼®
raw_news_comment <- read_csv("ko_test_label.csv")  # csv µ¥ÀÌÅÍ ºÒ·¯¿À±â
raw_news_comment

news_comment <- raw_news_comment %>% mutate(id = row_number(), reply = str_squish(replace_html(reply))) # °íÀ¯¹øÈ£ »ı¼º, Æ¯¼ö¹®ÀÚ Á¦°Å
glimpse(news_comment) # µ¥ÀÌÅÍ ±¸Á¶ È®ÀÎ

word_comment <- news_comment %>% unnest_tokens(input = reply, output = word, token = "words", drop = F)  # ´Ü¾î ±âÁØ ÅäÅ«È­
word_comment %>% select(word, reply)

dic <- read_csv("knu_sentiment_lexicon.csv") # °¨Á¤»çÀü csv ÆÄÀÏ ºÒ·¯¿À±â

word_comment <- word_comment %>% left_join(dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity)) # ´Ü¾î±âÁØ ÅäÅ«È­, °¨Á¤Á¡¼ö ºÎ¿©ÇÏ±â
word_comment %>% select(word, polarity)

word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))  # °¨Á¤ÀÌ µå·¯³­ ´Ü¾î¸¦ ºĞ·ù
word_comment %>% count(sentiment)

top10_sentiment <- word_comment %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n = 20)  # ±àÁ¤, ºÎÁ¤ ´Ü¾î 10°³¸¦ ÃßÃâ
top10_sentiment

ggplot(top10_sentiment, aes(x = reorder(word, n), y = n, fill = sentiment)) + geom_col() + coord_flip() + geom_text(aes(label = n), hjust = -0.3) + facet_wrap(~sentiment, scales = "free") + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + labs(x = NULL) # ÃßÃâÇÑ 10°³¸¦ ±×·¡ÇÁÈ­

score_comment <- word_comment %>% group_by(id, reply) %>% summarise(score = sum(polarity)) %>% ungroup()  # ¸®ºä º° °¨Á¤ Á¡¼ö ±¸ÇÏ°í °è»ê
score_comment %>% select(score, reply)

score_comment %>% select(score, reply) %>% arrange(-score) # °¨Á¤Á¡¼ö ³ôÀº ±àÁ¤ ´ñ±Û Ã£±â
score_comment %>% select(score, reply) %>% arrange(score)  # °¨Á¤Á¡¼ö ³ôÀº ºÎÁ¤ ´ñ±Û Ã£±â

score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))  # ±àÁ¤, ºÎÁ¤¿¡ µû¸¥ °¨Á¤ ºĞ·ù
freq_score <- score_comment %>% count(sentiment) %>% mutate(ratio = n / sum(n) * 100) # ºñÀ²À» ³ªÅ¸³»¾î Ç¥½Ã
freq_score

ggplot(freq_score, aes(x = sentiment, y = n, fill = sentiment)) + geom_col() + geom_text(aes(label = n), vjust = -0.3) + scale_x_discrete(limits = c("pos", "neu", "neg"))  # °¨Á¤ ºĞ·ùÇÑ Ç×¸ñÀ» ±×·¡ÇÁ·Î Ç¥½Ã

# ¹®Á¦ 5¹ø °¨Á¤»çÀü ¼öÁ¤, ºñ±³
score_comment %>% filter(str_detect(reply, "¹ÌÄ£")) %>% select(reply)
score_comment %>% filter(str_detect(reply, "¼Ò¸§")) %>% select(reply)

dic %>% filter(word %in% c("¹ÌÄ£", "¼Ò¸§")) # ´Ü¾î ºĞ·ù

new_dic <- dic %>% mutate(polarity = ifelse(word %in% c("¹ÌÄ£", "¼Ò¸§"), 2, polarity)) # Æ¯Á¤ ´Ü¾î¸¦ Àç ºĞ·ù
new_dic %>% filter(word %in% c("¹ÌÄ£", "¼Ò¸§"))  # Àç ºĞ·ù°¡ µÈ ´Ü¾î¸¦ ´Ù½Ã È®ÀÎ

new_word_comment <- word_comment %>% select(-polarity) %>% left_join(new_dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))  # ¼öÁ¤ÇÑ °¨Á¤»çÀüÀ¸·Î Á¡¼ö ºÎ¿©
new_word_comment

new_score_comment <- word_comment %>% group_by(id, reply) %>% summarise(score = sum(polarity)) %>% ungroup()
new_score_comment %>% select(score, reply) %>% arrange(-score) # ´ñ±Û º° °¨Á¤ Á¡¼ö

score_comment %>% select(score, reply) %>% arrange(-score) # °¨Á¤ Á¡¼ö Àç È®ÀÎ

new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= 0, "neg", "neu")))  # Á¡¼ö º° °¨Á¤ºĞ·ù

score_comment %>% count(sentiment) %>% mutate(ratio = n / sum(n) * 100) # °¨Á¤ ¹üÁÖº° ºóµµ¿Í ºñÀ² °è»ê - ¿øº»
new_score_comment %>% count(sentiment) %>% mutate(ratio = n / sum(n) * 100) # °¨Á¤ ¹üÁÖº° ºóµµ¿Í ºñÀ² °è»ê - ¼öÁ¤

word = "¹ÌÄ£|¼Ò¸§"
score_comment %>% filter(str_detect(reply, word)) %>% count(sentiment)  # ¼Ò¸§, ¹ÌÄ£ÀÌ »ç¿ëµÈ ´ñ±ÛÀÇ °¨Á¤¹üÁÖ ºóµµ º¯È­ - ¿øº»
new_score_comment %>% filter(str_detect(reply, word)) %>% count(sentiment)  # ¼Ò¸§, ¹ÌÄ£ÀÌ »ç¿ëµÈ ´ñ±ÛÀÇ °¨Á¤¹üÁÖ ºóµµ º¯È­ - ¼öÁ¤