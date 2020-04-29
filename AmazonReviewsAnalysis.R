library(tidyverse)
library(rvest)
library(DT)
library(tidytext)
library(dplyr)
library(stringr)
library(sentimentr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(SnowballC)
library(tm)
library(wordcloud)
library(reticulate)
library(crfsuite)

scrape_amazon <- function(ASIN, page_num){
  
  url_reviews <- paste0("https://www.amazon.com/product-reviews/",ASIN,"/?pageNumber=",page_num)
  
  doc <- read_html(url_reviews) # Assign results to `doc`
  
  # Review Title
  doc %>% 
    html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
    html_text() -> review_title
  
  # Review Text
  doc %>% 
    html_nodes("[class='a-size-base review-text review-text-content']") %>%
    html_text() -> review_text
  
  # Number of stars in review
  doc %>%
    html_nodes("[data-hook='review-star-rating']") %>%
    html_text() -> review_star
  
  # Return a tibble
  tibble(review_title,
         review_text,
         review_star,
         page = page_num) %>% return()
}



ASIN <- "change to real ASIN" # Specify ASIN
page_range <- 1:10 # Let's say we want to scrape pages 1 to 10

# Create a table that scrambles page numbers using `sample()`
# For randomising page reads!
match_key <- tibble(n = page_range,
                    key = sample(page_range,length(page_range)))

lapply(page_range, function(i){
  j <- match_key[match_key$n==i,]$key
  
  message("Getting page ",i, " of ",length(page_range), "; Actual: page ",j) # Progress bar
  
  Sys.sleep(3) # Take a three second break
  
  if((i %% 3) == 0){ # After every three scrapes... take another two second break
    
    message("Taking a break...") # Prints a 'taking a break' message on your console
    
    Sys.sleep(2) # Take an additional two second break
  }
  scrape_amazon(ASIN = ASIN, page_num = j) # Scrape
}) -> amazon_reviews_list


amazon_reviews_df <- amazon_reviews_list %>% 
  bind_rows()


df <-amazon_reviews_df %>%
         separate(review_star, c("review", "max review"), sep = "out"  )

df$review <- as.numeric(df$review)

glimpse(df$review)

data <- df %>%
            select(1:3)


words <- data %>%
  select(c("review_title", "review_text", "review")) %>%
  unnest_tokens(word, review_text) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))



afinn <- get_sentiments("afinn") %>% mutate(word = wordStem(word))
data.afinn <- words %>%
  inner_join(afinn, by = "word")


word_summary <- data.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(review), score = max(value), count_word = n()) %>%
  arrange(desc(count_word))

datatable(head(word_summary))

ggplot(filter(word_summary1, count_word < 50000), aes(mean_rating, score)) + 
  geom_text(aes(label = word, color = count_word, size=count_word), position= position_jitter()) + 
  scale_color_gradient(low = "lightblue", high = "darkblue") + 
  coord_cartesian(xlim=c(3.5,4.5)) + guides(size = FALSE, color=FALSE)

wordcloud(words = word_summary1$word, freq = word_summary1$count_word, scale=c(5,.5), max.words=300, colors=brewer.pal(8, "Dark2"))

good_reviews <- data.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(review), score = max(value), count_word = n()) %>%
  filter(mean_rating>mean(mean_rating)) %>%
  arrange(mean_rating)
wordcloud(words = good$word, freq = good$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Dark2"))

bad_reviews <- data.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(review), score = max(value), count_word = n()) %>%
  filter(mean_rating<mean(mean_rating)) %>%
  arrange(mean_rating)
wordcloud(words = bad$word, freq = bad$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(n = 8 ,name = "Dark2"))

review_summary <- data.afinn %>%
  group_by(review_title) %>%
  summarise(mean_rating = mean(review), sentiment = mean(value))
datatable(head(review_summary))

review_summary$sentiment

y_mid = 0
x_mid = 3

review_summary %>% 
  mutate(quadrant = case_when(mean_rating > x_mid & sentiment > y_mid   ~ "Positive Review/Postive Sentiment",
                              mean_rating <= x_mid & sentiment > y_mid  ~ "Negative Review/Positive Sentiment",
                              mean_rating <= x_mid & sentiment <= y_mid ~ "Negative Review/Negative Sentiment",
                              TRUE                                      ~ "Positive Review/Negative Sentiment")) %>% 
  ggplot(aes(x = mean_rating, y = sentiment, color = quadrant)) + 
  geom_hline(yintercept=y_mid, color = "black", size=.5) + 
  geom_vline(xintercept=x_mid, color = "black", size=.5) +
  guides(color=FALSE) +
  scale_color_manual(values=c("green", "red", "red","green")) +
  ggtitle("How buyers rated and comment the purchase") +
  ggplot2::annotate("text", x = 4.33, y=3.5,label="Positive Review/Postive Sentiment") +
  ggplot2::annotate("text", x = 2, y=3.5,label="Negative Review/Positive Sentiment") +
  ggplot2::annotate("text", x = 4.33, y=-2.5,label="Positive Review/Negative Sentiment") +
  ggplot2::annotate("text", x = 2, y=-2.5,label="Negative Review/Negative Sentiment") +
  geom_point()

dd <-amazon_reviews_df %>%
  unnest_tokens(ngram, "review_text", token = "ngrams", n = 4)
