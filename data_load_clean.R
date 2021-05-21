# TODO: remake into a markdown report generator

library(tidyverse)
library(tidytext)
library(yaml)

# read in & parse to tibble
filename <- "sara_faq.yml" 
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

unique_words_by_intent %>%
  group_by(intent) %>%
  arrange(-n) %>%
  ggplot(aes(reorder(intent, intent, function(x) -length(x)))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x=element_blank()) +
  ggtitle(paste(strsplit(filename, "\\.")[[1]][1],
                "\nNumber of hapax legomenon per intent",
                "\n(intents with more unique words may need more training data)"))

### look at words that only show up in a single intent

# get a list of words that show up in a single intent
words_unique_to_intent <- freq_by_intent %>%
  ungroup() %>%
  count(word) %>%
  filter(n == 1) 

intents_freq_unique_words <- 
  freq_by_intent %>%
  filter(word %in% words_unique_to_intent$word) 

# remove hapax legomena
intents_freq_unique_words_nHL <- 
  freq_by_intent %>%
  filter(word %in% words_unique_to_intent$word) %>%
  filter(n != 1)

intents_freq_unique_words_nHL %>%
  group_by(intent) %>%
  arrange(-n) %>%
  ggplot(aes(reorder(intent, intent, function(x) -length(x)))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x=element_blank()) +
  ggtitle(paste(strsplit(filename, "\\.")[[1]][1],
                "\nNumber of tokens unique to an intent",
                "\nexcluding words that only show up once"))

ggplot(intents_freq_unique_words, aes(intent, word)) +
  geom_tile(aes(n))

# get the words that are most likely to be used as heuristics
# it is important to check this for spurious words/ones that 
# are v. statistically informative but may not actually be
# associated with your intended intent by your users in prod.
intents_freq_unique_words %>%
  arrange(-n) %>%
  head(50)

# list of intents likely not to have a strong token-level heuristic'
# (may have a hapex lagomena though). These intents are likely not to
# be strongly influenced by single tokens 
dfname$intent[!(dfname$intent %in% intents_freq_unique_words_nHL$intent)]

# TODO: break down by intent and add to markdown
