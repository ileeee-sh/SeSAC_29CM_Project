install.packages(c("readxl", "wordcloud2", "dplyr", "stringr"))
library(readxl)
library(wordcloud2)
library(dplyr)
library(stringr)

# 1. file upload
file_path <- file.choose()
df <- read_excel(file_path)

token_col <- colnames(df)[9]  
cat("토큰 컬럼명:", token_col, "\n")
cat("총 행수:", nrow(df), "\n")

# token parsing
parse_tokens <- function(x) {
  x <- str_replace_all(x, "\\[|\\]|'", "")
  tokens <- str_split(x, ",\\s*")[[1]]
  str_trim(tokens)
}

all_tokens <- df[[token_col]] %>%
  na.omit() %>%
  lapply(parse_tokens) %>%
  unlist()

# Removing stopwords
stopwords <- c("의", "가", "이", "은", "들", "는", "좀", "잘", "걍",
               "과", "도", "를", "으로", "자", "에", "와", "한", "하",
               "것", "그", "저", "수", "더", "다", "고", "서", "을",
               "에서", "부터", "까지", "하다", "있다", "되다", "이다",
               "않다", "없다", "같다", "많다", "크다", "작다",
               "ㅋ", "ㅋㅋ", "ㅠ", "ㅠㅠ", "ㅎ", "ㅎㅎ")

all_tokens <- all_tokens[!all_tokens %in% stopwords]
all_tokens <- all_tokens[nchar(all_tokens) >= 2]

# Counting Frequency
freq_df <- as.data.frame(table(all_tokens)) %>%
  rename(word = all_tokens, freq = Freq) %>%
  arrange(desc(freq))

cat("상위 20개:\n")
print(head(freq_df, 20))

# Wordcloud
wordcloud2(
  data     = freq_df %>% head(150),
  size     = 0.8,
  color = rep(c("#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", "#BAE1FF", "#E8BAFF"),
              length.out = nrow(freq_df)),
  backgroundColor = "#1a1a1a",
  fontFamily = "맑은 고딕",  
  minRotation = 0,
  maxRotation = 0
)

getwd()