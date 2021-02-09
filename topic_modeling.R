require(pacman)
p_load(tidyverse,pdftools,tabulizer,lubridate,rvest,yaml,jsonlite,tidytext,janeaustenr,forcats)


filelist <- list.files("~/congressional-record/data")

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

s <- NULL

for (i in c((length(filelist)-999):length(filelist))) {
  
  t <- loadRData(paste0("~/congressional-record/data/",filelist[i]))
  t <- t %>% str_remove_all("-\r\n")
  t <- data.frame(t)
  
  names(t) <- "text"
  t$text <- as.character(t$text)
  t$book <- filelist[i] %>% str_remove(".Rdata")
  
  s <- rbind(s,t)
  
}

book_words <- s %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words) %>% 
  dplyr::filter(!str_detect(word,"[:digit:]|â")) %>% 
  anti_join(stop_words)


book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_small <- book_tf_idf %>% group_by(book) %>% top_n(tf_idf,n=1)
book_small <- book_small[order(book_small$book),]

book_small %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

