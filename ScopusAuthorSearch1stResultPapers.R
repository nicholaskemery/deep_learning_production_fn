library(rscopus)
library(tidyverse)

rscopus::set_api_key(api_key = "")

#Note, to save computation time, this function returns all of the papers for the *first* author in a query.
scopus_author_fn2 <- function(data) {
  result <- data.frame(matrix(nrow=0,ncol=0)) #Instantiate df.
  for (i in 1:nrow(data)){
    last = paste(data$last[i], sep="")
    first = paste(data$first[i], sep="")
    search_res = tryCatch(author_df(last_name = last,
                                    first_name = first,
                                    #affil_name = affil,
                                    count=25,
                                    max_count=26,
                                    wait_time = 1),
                          error=function(e) NULL)#Query Scopus
    result = bind_rows(result, search_res)
    i = i+1
  }
  return(result)
}

ml_words <- c("artificial intelligence", "machine learning", "statistical learning", "deep learning", "NLP", "computer vision", "pattern recognition", "robotics", "neural", "evolutionary comput", "big data", "segmentation", "preprocess", "loss function", "adversarial", "reinforcement learning", "multiagent", "knowledge gradient", "robot", "computer vision")

scopus_papers = scopus_author_fn2(ms_papers)

#Filter to only output ML-related papers.
scopus_papers2 <- scopus_papers %>%
  filter(grepl(paste(ml_words, collapse="|"), `dc:description`, ignore.case = T))


