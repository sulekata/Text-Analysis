
# Setup -------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(rvest)
library(httr)
library(ggplot2)
library(tidytext)
library(scales)
library(GGally)
library(textdata)
library(ggthemr)
library(wordcloud)
library(reshape2)
library(topicmodels)
library(widyr)
library(igraph)
library(ggraph)
library(forcats)

# set theme for plots
theme_set(theme_light() +
            theme( panel.grid.minor.x = element_blank(), 
                                 plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) ))

# Scrape Top 100 songs from Billboard.com ---------------------------------

# creating the links for every page to be scraped
links <- NULL 
for ( i in 2006:2020 ){
  link <- paste0( 'https://www.billboard.com/charts/year-end/', i, '/hot-100-songs')
  links <- c(links, link)
}

# creating a function which scrapes the ranking of the song, the title and the artist
# and saves these into a named list
get_one_page <- function( url ) {
  # create empty list to save the data into
  tlist <- list()
    
  # read page
  page <- read_html( url )
  
  # extract rankings of songs
  ranking <- 
    page %>% 
    html_nodes('.ye-chart-item__rank')%>%
    html_text()
  
  # remove line breaks before and after the numbers
  ranking <- str_replace_all(ranking, "\n", "")
  
  # save the rankings into a named list
  tlist[[ 'ranking' ]] <- ranking
  
  # extract titles of songs
  title <- 
    page %>% 
    html_nodes('.ye-chart-item__title')%>%
    html_text()
  
  # remove line breaks before and after the titles
  title <- str_replace_all(title, "\n", "")
  
  # save the titles to the list
  tlist[[ 'title' ]] <- title
  
  # extract names of artists
  artist <- 
    page %>% 
    html_nodes('.ye-chart-item__artist')%>%
    html_text()
  
  # remove line breaks before and after the titles
  artist <- str_replace_all(artist, "\n", "")
  
  # save the titles to the list
  tlist[[ 'artist' ]] <- artist
  
  # extract year from url
  r <- regexec("[0-9]+", url)
  year <- regmatches(url, r)[[1]]
  
  # save year to list
  tlist[['year']] <- rep(year, length(artist))
  
  return( tlist )
}

# scraping the pages for every available year with the function
content <- lapply( links, get_one_page )

# writing all the scraped content into a data frame
df <- rbindlist( content, fill = T )

# save the df to csv
write_csv(df, 'C:/CEU/Spring_Term/Data_Science_3/Assignment/billboard.csv')

# import df
df <- read_csv('C:/CEU/Spring_Term/Data_Science_3/Assignment/billboard.csv')

# Scrape the lyrics of songs ----------------------------------------------

# split artist and create new column with the first artist's name
# initialize empty list
artist_clean <- NULL

for (i in 1:nrow(df)){
  if (str_detect(df$artist[i], 'Featuring')){
    first_artist <- strsplit(df$artist[i], ' Featuring')[[1]][1]
    if (str_detect(first_artist, '&')){
      first_artist <- strsplit(first_artist, ' &')[[1]][1]
      artist_clean <- c(artist_clean, first_artist)
    } else {
      artist_clean <- c(artist_clean, first_artist)
    }
  } else if (str_detect(df$artist[i], ',')){
    first_artist <- strsplit(df$artist[i], ',')[[1]][1]
    artist_clean <- c(artist_clean, first_artist)
  } else if (str_detect(df$artist[i], '&')){
    first_artist <- strsplit(df$artist[i], ' &')[[1]][1]
    artist_clean <- c(artist_clean, first_artist)
  } else if (str_detect(df$artist[i], 'With')){
    first_artist <- strsplit(df$artist[i], ' With')[[1]][1]
    artist_clean <- c(artist_clean, first_artist)
  } else {
    first_artist <- df$artist[i]
    artist_clean <- c(artist_clean, first_artist)
  }
  
}

# remove *-s from artists names
for (i in 1: length(artist_clean)){
  if (str_detect(artist_clean[i], '\\*')){
    clean <- str_replace_all(artist_clean[i], '\\*', ' ')
    artist_clean[i] <- clean
  } else {
    next
  }
}

# assign clean names to df
df$artist_first <- artist_clean

# change e at the end of Beyonce to é
df$artist_first <- ifelse(df$artist_first == 'Beyonce', 'Beyoncé', df$artist_first)

# create function to scrape url for one song
get_one_lyrics_url <- function(i){
  
  # construct search term
  search_term <- paste(tolower(df$artist_first[i]), tolower(df$title[i]), sep = '+')
  
  # construct search url
  search_url <- paste0('https://search.azlyrics.com/search.php?q=', search_term)
  
  # scrape search page
  search_page <- tryCatch( 
    {
      read_html(search_url)
    },
    error=function(cond) 
    {
      message(paste("URL caused an error:", search_url))
      message("Here's the original error message:")
      message(cond)
      return(NA)
    },
    finally={
      message(paste("Scraped URL:", search_url))
    }
  )
  
  # sleep to avoid getting blocked
  Sys.sleep(sample(1:5, 1))
  
  # extract url to lyrics page
  lyrics_url <- tryCatch(
    {
      search_page %>%
      html_node('.visitedlyr') %>% 
      html_node('a') %>%
      html_attr('href')
    },
    error=function(cond) 
    {
      message(paste("URL caused an error:", search_url))
      message("Here's the original error message:")
      message(cond)
      return(NA)
    }
  )
  
  return(lyrics_url)
  
}

# run the function
lyrics_url_sum <- lapply(1:1498, get_one_lyrics_url)

# unlist the list of lists
lyrics_url_sum <- unlist(lyrics_url_sum)

# save lyrics urls to df
df$lyrics_url <- lyrics_url_sum

# save df to csv
write_csv(df, 'C:/CEU/Spring_Term/Data_Science_3/Assignment/df_with_lyrics_url.csv')

# create function to scrape lyrics for one song
get_one_lyrics <- function(i){
  
  # read lyrics page
  lyrics_page <- tryCatch( 
    {
      read_html(df$lyrics_url[i])
    },
    error=function(cond) 
    {
      message(paste("URL caused an error:", df$lyrics_url[i]))
      message("Here's the original error message:")
      message(cond)
      return(NA)
    },
    finally={
      message(paste("Scraped URL:", df$lyrics_url[i]))
    }
  )
  
  # sleep to avoid getting blocked
  Sys.sleep(sample(1:5, 1))
  
  # extract lyrics
  lyrics_text <- tryCatch(
    {
    lyrics_page %>%
    html_nodes('div') %>%
    html_text()
    },
    error=function(cond) 
    {
      message(paste("URL caused an error:", df$lyrics_url[i]))
      message("Here's the original error message:")
      message(cond)
      return(NA)
    },
    finally={
      message(paste("Scraped lyrics for song number:", i))
    }
  )
  
  one_lyrics <- lyrics_text[21]
  
  return(one_lyrics)
}

# run the function
lyrics_sum <- lapply(1:1498, get_one_lyrics)

# flatten list
lyrics_sum <- unlist(lyrics_sum)

# save lyrics to df
df$lyrics <- lyrics_sum

# check the number of observations where lyrics is missing
df %>% filter(is.na(lyrics)) %>% nrow()

# filter observations where lyrics are missing
df <- df %>% filter(is.na(lyrics) == FALSE)

# save the df
write_csv(df, 'C:/CEU/Spring_Term/Data_Science_3/Assignment/df_with_lyrics_filtered.csv')

# Clean lyrics ------------------------------------------------------------

df <- read_csv('C:/CEU/Spring_Term/Data_Science_3/Assignment/df_with_lyrics_filtered.csv')

# clean lyrics from linebreaks and extra white spaces
df$lyrics <- unlist(lapply(df$lyrics, function(x){
              gsub('\\s+',' ', x)
              }))

# clean lyrics from extra information in square brackets such as [Verse 1]
df$lyrics <- unlist(lapply(df$lyrics, function(x){
              gsub("\\[[^\\)]+\\]", "", x)
              }))


# Prepare df ---------------------------------------------------

# top 10 most popular artists
df %>% 
  count(artist_first, sort = TRUE) %>% 
  head(10)

# check number of duplicates
nrow(df[duplicated(df$lyrics_url),])

# filter duplicates
df <- df[!duplicated(df$lyrics_url),]

# select columns
df <- df %>% select(title, artist_first, year, lyrics)

# Word Frequency -----------------------------------------------------------

# tokenize songs
df_tokens <- df %>% unnest_tokens(word, lyrics)

# import stopwords
data(stop_words)

# filter stopwords from the tokens
df_tokens <- df_tokens %>%
  anti_join(stop_words)

# get most common words
df_tokens %>%
  count(word, sort = TRUE) %>% 
  head(10)

# plot 10 most common words
df_tokens %>%
  count(word, sort = TRUE) %>%
  head(10) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip() + ylab('Frequency') +
  ggtitle('Top 10 Most Frequent Words in Billboard Top 100 Songs')

# wordcloud with most common words
df_tokens %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# plot top 10 most popular artists with different songs
df %>% 
  count(artist_first, sort = TRUE) %>% 
  head(10) %>% 
  mutate(artist_first = reorder(artist_first, n)) %>%
  ggplot(aes(n, artist_first)) +
  geom_col() +
  labs(y = NULL, x = 'Number of Songs', title = 'Top 10 Most Popular Artists') 

# compare most frequent words for Taylor Swift and Drake
# plot top 10 words for Taylor Swift
df_tokens %>% 
  filter(artist_first %in% c('Taylor Swift')) %>% 
  count(word, sort = T) %>% 
  head(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL, x = 'Frequency', title = 'Top 10 Most Frequent Words for Taylor Swift')

# plot top 10 words for Drake
df_tokens %>% 
  filter(artist_first %in% c('Drake')) %>% 
  count(word, sort = T) %>% 
  head(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL, x = 'Frequency', title = 'Top 10 Most Frequent Words for Drake')

# compare frequency of words to 2006
frequency <- df_tokens %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(year, word) %>%
  group_by(year) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(year, proportion) %>% 
  pivot_longer(names_to = "year", values_to = "proportion", cols = as.character(setdiff(unique(df_tokens$year), c(2006))))

# plot frequent words by year
ggplot(frequency, aes(x = proportion, y = `2006`, 
                      color = abs(`2006` - proportion))) +
  geom_abline(color = "gray", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~year, ncol = 3) +
  theme(legend.position="none") +
  labs(y = "2006", x = NULL, title = 'Comparison of Frequently Used Words Across Years')

# check the correlation between years
df_corr <- df_tokens %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(year, word) %>%
  group_by(year) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(year, proportion) %>% 
  select(-word)

# replace NAs with 0
df_corr[is.na(df_corr)] <- 0

# create plot
ggcorr(df_corr, 
       midpoint = 0.5, 
       limits = c(0,1),
       low = "#fde0dd",
       mid = "#fa9fb5",
       high = "#c51b8a") +
  ggtitle('Correlation of Years based on Frequent Words')

# Sentiment analysis ------------------------------------------------------

# download lexicons
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")

# join tokens with sentiment
df_sentiment_nrc <- df_tokens %>% inner_join(nrc)

# check the number of words in different sentiment categories
df_sentiment_nrc %>% 
  group_by(sentiment) %>% 
  summarise(num_words = n()) %>% 
  arrange(-num_words)
  
# compare the three dictionaries
# calculate sentiment with afinn
afinn <- df_tokens %>% 
  inner_join(afinn) %>%
  mutate(sentiment = ifelse(value < 0, "negative", 
                            ifelse(value == 0, "neutral", "positive")))%>%
  count(index = title, sentiment)%>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  mutate(method = "AFINN")

# calculate sentiment with nrc and bing
bing_and_nrc <- bind_rows(df_tokens %>% 
                            inner_join(bing) %>%
                            mutate(method = "Bing et al."),
                          df_tokens %>% 
                            inner_join(nrc %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# bind the two df-s and plot sentiment across songs
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  scale_fill_viridis_d() +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = 'Sentiment', title = 'Comparison of Sentiment Lexicons') +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

# compare sentiments across top 4 artists
# get names of top 4 artists
top4 <- df %>% 
  count(artist_first, sort = T) %>% 
  head(4)

# calculate sentiment for their songs
top4_sent <- df_tokens %>% 
              filter(artist_first %in% top4$artist_first) %>% 
              inner_join(bing) %>%
              count(artist_first, index = title, sentiment) %>%
              pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
              mutate(sentiment = positive - negative)

# plot sentiment
ggplot(top4_sent, aes(index, sentiment, fill = artist_first)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d() +
  facet_wrap(~artist_first, ncol = 2, scales = "free_x") +
  labs(x = NULL, y = 'Sentiment', title = 'Song Sentiments of Top 4 Artists') +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

# most common positive and negative words
bing_word_counts <- df_tokens %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# plot
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d() +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = NULL,
       y = NULL, title = 'Most Frequent Positive and Negative Words')

# create wordcloud for positive and negative words
df_tokens %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
 
# sentiment with the afinn lexicon
df_tokens %>%
  count(word, title, sort = TRUE) %>%
  rename(index = title) %>% 
  inner_join(get_sentiments('afinn'), by = 'word') %>%
  group_by(word) %>%
  summarize(contribution = sum(n * value)) %>%
  top_n(10, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution < 0)) +
  geom_col(show.legend = F) +
  coord_flip() +
  labs(y = "Frequency of word * AFINN score", x = NULL, title = 'Sentiment of Most Used Positive and Negative Words')


# Topic modelling ---------------------------------------------------------

# count words by song
df_tokens_counts <- df_tokens %>% 
  count(title, word, sort = T) %>% 
  ungroup()

# create document-term matrix
df_dtm <- df_tokens_counts %>%
  cast_dtm(title, word, n)

# do LDA
songs_lda <- LDA(df_dtm, k = 2, control = list(seed = 1234))

# check topics
songs_topics <- tidy(songs_lda, matrix = "beta")

# get top terms for each topic
top_terms <- songs_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# visualize top words by topic
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_fill_viridis_d() +
  coord_flip() +
  labs(x = NULL, y = 'Probability', title = 'Top 5 Words by Topics')

# calculate differences in beta between topics
beta_wide <- songs_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

# create plot
beta_wide %>% 
  top_n(20, abs(log_ratio)) %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio, fill = log_ratio < 0)) +
  geom_col(show.legend = F) +
  scale_fill_viridis_d() +
  coord_flip() +
  labs(y = "Log2 Ratio of Beta in Topic 2/Topic 1", x = NULL, title = 'Words with the Greatest Differences Between Topics')

# check topics for songs
songs_gamma <- tidy(songs_lda, matrix = "gamma")

# get top topics for each song
top_songs <- songs_gamma %>%
  group_by(topic) %>%
  top_n(5, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

# visualize top songs by topic
top_songs %>%
  mutate(document = reorder(document, gamma)) %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d() +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(x = NULL, y = 'Probability', title = 'Top 5 Songs by Topic')

# group songs by artist and count words
item_counts <- df_tokens %>% 
  filter(artist_first %in% c('Rihanna', 'Lil Wayne', 'Ariana Grande', 'Chris Brown')) %>% 
  unite(item, artist_first, title) %>% 
  select(item, word) %>% 
  count(item, word, sort = TRUE) %>%
  ungroup()

# create document-term matrix
items_dtm <- item_counts %>%
  cast_dtm(item, word, n)

# do LDA
items_lda <- LDA(items_dtm, k = 4, control = list(seed = 1234))

# check the topics
items_gamma <- tidy(items_lda, matrix = "gamma")

# separate artist from song title
items_gamma <- items_gamma %>%
  separate(document, c("artist", "title"), sep = "_", convert = TRUE)

# plot topic probability across artists
items_gamma %>%
  mutate(artist = reorder(artist, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  scale_color_viridis() +
  geom_boxplot() +
  facet_wrap(~ artist) +
  labs(x = "Topic", y = expression(gamma), title = 'Testing LDA')

# get topic most associated with each song
artist_classifications <- items_gamma %>%
  group_by(artist, title) %>%
  slice_max(gamma) %>%
  ungroup()

# assign artists to topics
artist_topics <- artist_classifications %>%
  count(artist, topic) %>%
  group_by(artist) %>%
  slice_max(n, n = 1) %>% 
  ungroup() %>%
  transmute(consensus = artist, topic)

# assign topics to words
assignments <- augment(items_lda, data = items_dtm)

# separate artist and title and join 'consensus' artist
assignments <- assignments %>%
  separate(document, c("artist", "title"), 
           sep = "_", convert = TRUE) %>%
  inner_join(artist_topics, by = c(".topic" = "topic"))

# create plot
assignments %>%
  count(artist, consensus, wt = count) %>%
  mutate(across(c(artist, consensus), ~str_wrap(., 20))) %>%
  group_by(artist) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, artist, fill = percent)) +
  geom_tile() +
  #scale_fill_gradient2(high = "darkred", label = percent_format()) +
  scale_fill_viridis(label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Lyric words were assigned to",
       y = "Lyric words came from",
       fill = "% of assignments", title = "Comparison of Artists' Choices of Words")

# check incorrectly assigned words
wrong_words <- assignments %>%
  filter(artist != consensus)

# order it by word count
wrong_words %>%
  count(artist, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n)) %>% 
  head(10) %>% 
  kable()

# TF-IDF ------------------------------------------------------------------

##### TF-IDF for years
# count tokens by year
song_words <- df %>%
  unnest_tokens(word, lyrics) %>%
  count(year, word, sort = TRUE)

# count total frequency by year
total_words <- song_words %>% 
  group_by(year) %>% 
  summarize(total = sum(n))

# join the df-s together
song_words <- left_join(song_words, total_words)

# plot distributions
ggplot(song_words, aes(n/total, fill = factor(year))) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~year, ncol = 2, scales = "free_y") +
  labs(x = 'N/Total', y = 'Count', title = 'Distribution of Word Frequency')
  
##### TF-IDF for artist
# count tokens by year
song_words <- df %>%
  unnest_tokens(word, lyrics) %>%
  count(artist_first, word, sort = TRUE)

# count total frequency by year
total_words <- song_words %>% 
  group_by(artist_first) %>% 
  summarize(total = sum(n))

# join the df-s together
song_words <- left_join(song_words, total_words)

# get tf-idf
song_tf_idf <- song_words %>%
  bind_tf_idf(word, artist_first, n)

# look at words with high tf-idf scores
song_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  head(10) %>% 
  select(artist_first, word, tf_idf)

# plot 
song_tf_idf %>%
  group_by(artist_first) %>%
  filter(artist_first %in% c('Drake', 'Maroon 5')) %>% 
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = artist_first)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d() +
  facet_wrap(~artist_first, ncol = 2, scales = "free") +
  labs(x = 'TF-IDF Score', title = 'Words with Highest TF-IDF Scores', y = NULL)

# popular artists use generic lyrics

# Word co-occurrences and correlations ------------------------------------

# create df with artist + title as IDs
df_cooc <- df_tokens %>% 
  mutate(id = paste0(artist_first, title))

# get words that occur together frequently
content_word_pairs <- df_cooc %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

# plot them on a network
set.seed(1234)
content_word_pairs %>%
  filter(n >= 80) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Most Frequent Co-occuring Words') +
  theme_void() +
  theme(plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) )


# N-gram Analysis ---------------------------------------------------------

# do bigrams
billboard_bigrams <- df %>%
  unnest_tokens(bigram, lyrics, token = "ngrams", n = 2)

# group bigrams by year
billboard_bigrams <- billboard_bigrams %>%
  count(year, bigram, sort = TRUE) %>%
  ungroup() %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# add custom stopwords
my_stopwords <- tibble(word = c(as.character(1:16), 
                                "la", "na", "ha", "yeah", 'doh', 'du', 'ooh', 'whoa',
                                'wee', 'ay', 'doo', 'aah', 'hey', 'ah', 'eh', 'ey',
                                'uh', 'huh'))

# check top bigrams by year
bigrams_filtered <- billboard_bigrams %>% 
  filter(!(word1 %in% my_stopwords$word) & !(word2 %in% my_stopwords$word)) %>% 
  filter(!(word1 %in% stop_words$word) & !(word2 %in% stop_words$word))

# unite bigrams
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# calculate tf-idf for bigrams
bigram_tf_idf <- bigrams_united %>%
  count(year, bigram) %>%
  bind_tf_idf(bigram, year, n) %>%
  arrange(desc(tf_idf)) %>% 
  head(10)

# bigrams for sentiment analysis
not_words <- billboard_bigrams %>%
  filter(word1 == "not") %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

# plot
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d() +
  labs(x = "Sentiment Value * Number of Occurrences",
       y = "Words Preceded by \"not\"",
       title = 'Top Words Contributing to the "Wrong" Sentiment')

# make graph from bigram
bigram_graph <- billboard_bigrams %>%
  select(-year) %>% 
  filter(n > 50) %>%
  graph_from_data_frame()

set.seed(2017)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
