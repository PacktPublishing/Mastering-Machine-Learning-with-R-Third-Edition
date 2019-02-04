install.packages("caret")
install.packages("classifierplots")
install.packages("ggplot2")
install.packages("ggraph")
install.packages("ggthemes")
install.packages("igraph")
install.packages("quanteda")
install.packages("qdap")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("SnowballC")
install.packages("sotu")
install.packages("text2vec")
install.packages("tm")
install.packages("topicmodels")


#tidytext ###################
library(magrittr)
library(sotu)
data(sotu_text) 
data(sotu_meta) 
sotu_meta$text <- sotu_text
colnames(sotu_meta)

sotu_meta %>%
  tidytext::unnest_tokens(word, text) -> sotu_unnest
# words, n-grams, sentences
sotu_unnest 

library(tidytext)
data(stop_words)
sotu_tidy <- sotu_unnest %>%
  dplyr::anti_join(stop_words, by = "word")

sotu_tidy %>%
  dplyr::count(word, sort = TRUE)

sotu_tidy %>%
  dplyr::count(word, sort = TRUE) %>%
  dplyr::filter(n > 2500) %>%
  dplyr::mutate(word = reorder(word, n)) %>%
  ggplot2::ggplot(ggplot2::aes(word, n)) +
  ggplot2::geom_col() +
  ggplot2::xlab(NULL) +
  ggplot2::coord_flip() +
  ggthemes::theme_igray()

sotu_tidy %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(totalWords = length(word)) %>%
  dplyr::arrange(desc(totalWords))

sotu_tidy %>%
  dplyr::filter(year > 1860 & year < 1865) %>%
  dplyr::count(word, sort = TRUE)

sotu_cloud <- sotu_tidy %>%
  dplyr::filter(year > 1860 & year < 1865)

qdap::trans_cloud(
  text.var = sotu_cloud$word,
  grouping.var = sotu_cloud$year,
  stem = FALSE,
  min.freq = 7
)

# nest and conduct word_associate?
nested_1862 <- sotu_tidy %>%
  dplyr::filter(year == 1862) %>%
  dplyr::select(year, word) %>%
  tidyr::nest(word) %>%
  dplyr::mutate(
    text = purrr::map(data, unlist),
    text = purrr::map_chr(text, paste, collapse = " ")
  )

# nested_1862 <- dplyr::filter(sotu_meta, year == 1862)
myCorpus <- tm::Corpus(tm::VectorSource(nested_1862$text))
quanteda::kwic(x = myCorpus$content, pattern = "emancipation", window = 7)

table(sentiments$lexicon)

get_sentiments("nrc")

nrc_anger <- tidytext::get_sentiments("nrc") %>% 
  dplyr::filter(sentiment == "anger")
# table(nrc_joy$sentiment)

sotu_tidy %>%
  dplyr::filter(year == 1862) %>%
  dplyr::inner_join(nrc_anger) %>%
  dplyr::count(word, sort = TRUE)

sentiment <- sotu_tidy %>%
  dplyr::inner_join(tidytext::get_sentiments("bing")) %>%
  dplyr::filter(year > 1852 & year <1873) %>%
  dplyr::count(president, year, sentiment) %>%
  tidyr::spread(sentiment, n, fill = 0) %>%
  dplyr::mutate(sentiment = positive - negative) %>%
  dplyr::arrange(year)

ggplot2::ggplot(sentiment, ggplot2::aes(year, sentiment, fill = president)) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~ president, ncol = 2, scales = "free_x") +
  ggthemes::theme_pander()

sotu_tidy %>%
  dplyr::inner_join(tidytext::get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  dplyr::ungroup()

# n-grams
sotu_bigrams <- sotu_meta %>%
  dplyr::filter(year > 1860 & year < 1865) %>%
  tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2, to_lower = FALSE)

sotu_bigrams %>%
  dplyr::count(bigram, sort = TRUE)

bigrams_separated <- sotu_bigrams %>%
  tidyr::separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  dplyr::filter(!word1 %in% stop_words$word) %>%
  dplyr::filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  dplyr::count(word1, word2, sort = TRUE)

bigram_counts
# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  dplyr::filter(n > 4) %>%
  igraph::graph_from_data_frame()

set.seed(1861)

ggraph::ggraph(bigram_graph, layout = "fr") +
  ggraph::geom_edge_link() +
  ggraph::geom_node_point() +
  ggraph::geom_node_text(ggplot2::aes(label = name), vjust = 1, hjust = 1)

# topicmodeling
sotu_meta[185:191, 1:4]
sotu_meta[188, 2] <- "1972_2"
sotu_meta[190, 2] <- "1974_2"
sotu_meta[157, 2] <- "1945_2"
sotu_meta[166, 2] <- "1953_2"
sotu_meta[170, 2] <- "1956_2"
sotu_meta[176, 2] <- "1961_2"
sotu_meta[195, 2] <- "1978_2"
sotu_meta[197, 2] <- "1979_2"
sotu_meta[199, 2] <- "1980_2"
sotu_meta[201, 2] <- "1981_2"

sotu_meta_recent <- sotu_meta %>%
  dplyr::filter(year > 1964)

sotu_meta_recent %>%
  tidytext::unnest_tokens(word, text) -> sotu_unnest_recent

sotu_recent <- sotu_unnest_recent %>%
  dplyr::anti_join(stop_words, by = "word")

sotu_recent %>%
  dplyr::group_by(year) %>%
  dplyr::count(word) -> lda_words

sotu_dtm <- tidytext::cast_dtm(lda_words, year, word, n)

sotu_lda <-
  topicmodels::LDA(
    sotu_dtm,
    k = 6,
    method = "Gibbs",
    control = list(seed = 1965, verbose = 1)
  )
sotu_lda  

topicmodels::topics(sotu_lda)
table(topicmodels::topics(sotu_lda))
topicmodels::terms(sotu_lda, 5)

lda_topics <- tidytext::tidy(sotu_lda, matrix = "beta")
# lda_topics
ap_top_terms <- lda_topics %>%
  dplyr::group_by(topic) %>%
  dplyr::top_n(10, beta) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(topic, -beta)
# table(ap_top_terms$topic, ap_top_terms$term)
ap_top_terms %>%
  dplyr::mutate(term = reorder(term, beta)) %>%
  ggplot2::ggplot(ggplot2::aes(term, beta, fill = factor(topic))) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~ topic, scales = "free") +
  ggplot2::coord_flip() +
  ggthemes::theme_economist_white()

ap_documents <- tidytext::tidy(sotu_lda, matrix = "gamma")

dplyr::filter(ap_documents, document == "1981")

# classification ----------------------------------------------------------


sotu_party <- sotu_meta %>%
  dplyr::filter(year > 1899)
table(sotu_party$party)

sotu_party$text <- tm::removeNumbers(sotu_party$text)
sotu_party$text <- tm::removePunctuation(sotu_party$text)
sotu_party$text <- tolower(sotu_party$text)
sotu_party$text <- tm::stemDocument(sotu_party$text)
sotu_party$text <- tm::removeWords(sotu_party$text, tm::stopwords("en"))


set.seed(222)
index <- caret::createDataPartition(sotu_party$party, p = 0.8, list = F)
train <- sotu_party[index, ]
test <- sotu_party[-index, ]

tok_fun = text2vec::word_tokenizer
it_train = text2vec::itoken(
  train$text,
  tokenizer = tok_fun,
  ids = train$year,
  progressbar = FALSE
)

vocab = text2vec::create_vocabulary(it_train)
pruned <- text2vec::prune_vocabulary(vocab, term_count_min = 4)

vectorizer = text2vec::vocab_vectorizer(pruned)

dtm_train = text2vec::create_dtm(it_train, vectorizer)

dim(dtm_train)

tfidf = text2vec::TfIdf$new()

dtm_train_tfidf = text2vec::fit_transform(dtm_train, tfidf)

dtm_test_tfidf = text2vec::create_dtm(it_test, vectorizer)
dtm_test_tfidf = transform(dtm_test_tfidf, tfidf)

x <- dtm_train_tfidf
y <- as.factor(train$party)

set.seed(123)
lasso <- glmnet::cv.glmnet(
  x,
  y,
  nfolds = 3,
  type.measure = "auc",
  alpha = 1,
  family = "binomial"
)

plot(lasso)

lasso_test <-
  data.frame(predict(lasso, newx = dtm_test_tfidf,
                     type = 'response'), s = "lambda.1se")

testY <- as.numeric(ifelse(test$party == "Republican", 1, 0))
Metrics::auc(testY, lasso_test$X1)

classifierplots::density_plot(testY, lasso_test$X1)

# advanced ----------------------------------------------------------------
tr <- paste(readLines("C:/Users/cory/Desktop/data/corpus/tr.txt"), collapse=" ") 
tr <- iconv(tr, "latin1", "ASCII", "") 


library(qdap)
prep_tr <- qdap::qprep(tr) 
prep_tr <- qdap::replace_contraction(prep_tr)
prep_tr <- qdap::rm_stopwords(prep_tr, Top100Words, separate = F)
prep_tr <- qdap::strip(prep_tr, char.keep = c("?", ".", "!")) 
address_tr <- data.frame(speech = prep_tr)
address_tr <- qdap::sentSplit(address_tr, "speech")
address_tr$pres <- "TR"

reagan <- paste(readLines("C:/Users/cory/Desktop/data/corpus/reagan.txt"), collapse=" ") 
reagan <- iconv(reagan, "latin1", "ASCII", "") 
prep_reagan <- qdap::qprep(reagan)
prep_reagan <- qdap::replace_contraction(prep_reagan)
prep_reagan <- qdap::rm_stopwords(prep_reagan, Top100Words, separate = F)
prep_reagan <- qdap::strip(prep_reagan, char.keep = c("?", ".", "!")) 
address_reagan <- data.frame(speech = prep_reagan)
address_reagan <- qdap::sentSplit(address_reagan, "speech")
address_reagan$pres <- "reagan"

sentences <- dplyr::bind_rows(address_tr, address_reagan)
plot(qdap::freq_terms(sentences$speech)) 

wordMat <- qdap::wfm(sentences$speech, sentences$pres)
head(wordMat[order(wordMat[, 1], wordMat[, 2],decreasing = TRUE),])

# qdap::trans_cloud(sentences$speech, sentences$pres, min.freq = 15)

ws <- qdap::word_stats(sentences$speech, sentences$pres, rm.incomplete = T)
# plot(ws)
ws$word.elem
ws$sent.elem

pol = qdap::polarity(sentences$speech, sentences$pres)
pol
plot(pol)

pol.df <- pol$all
which.min(pol.df$polarity) 
pol.df$text.var[86]

ari <- qdap::automated_readability_index(sentences$speech, sentences$pres)
ari$Readability

tr_sentences <- dplyr::filter(sentences, pres == "TR")
tr_sentences <- tr_sentences[1:300, ]
qdap::formality(tr_sentences$speech)

reagan_sentences <- dplyr::filter(sentences, pres == "reagan")
formality(reagan_sentences$speech)
#gc()
form$form.prop.by

diversity(sentences$speech, sentences$pres)

dispersion_plot(
  sentences$speech,
  rm.vars = sentences$pres,
  c("peace", "government", "marksmanship"),
  color = "black",
  bg.color = "white"
)
