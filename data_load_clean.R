library(tidyverse)
library(tidytext)
library(yaml)

# read in & parse to tibble
filename <- "sara_faq.yml"
dfname <- strsplit(filename, "\\.")[[1]][1]
sara_faq <- readChar(filename, file.info(filename)$size) %>%
  yaml.load()

dfname <- unlist(sara_faq$nlu) %>%
  matrix(ncol = 2, byrow=T) %>%
  as_tibble() %>%
  rename(intent = V1, text = V2)

# look at word freq by intent
freq_by_intent <- dfname %>%
  unnest_tokens(word, text) %>%
  group_by(intent) %>%
  count(word)

# create matrix for all words
ggplot(freq_by_intent, aes(word, intent)) +
  geom_tile(aes(fill = n))

### Look at unique words

# look at unique words in the corpus, sorted by intent
unique_words <- dfname %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  filter(n == 1)

unique_words_by_intent <- freq_by_intent %>%
  filter(word %in% unique_words$word)

### look at words that only show up in a single intent

# get a list of words that show up in a single intent
words_unique_to_intent <- freq_by_intent %>%
  ungroup() %>%
  count(word) %>%
  filter(n == 1) 

intents_freq_unique_words <- 
  freq_by_intent %>%
  filter(word %in% words_unique_to_intent$word) 

ggplot(intents_freq_unique_words, aes(intent, word)) +
  geom_tile(aes(n))

# get the words that are most likely to be used as heuristics
intents_freq_unique_words %>%
  arrange(n) %>%
  tail(50)
