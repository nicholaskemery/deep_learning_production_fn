library(rscopus)
library(tidyverse)

rscopus::set_api_key(api_key = "")

scopus_all_au_fn <- function(data) {
  result <- data.frame(matrix(nrow=0,ncol=0)) #Instantiate df.
  for (i in 1:nrow(data)){
    search_res = tryCatch(
      get_complete_author_info(last_name = data$last[i],
                               first_name = data$first[i],
                               count=10),
      error=function(e) NULL)#Query Scopus
    search_df = gen_entries_to_df(search_res$content) #Convert to df
    search_out = search_df$entry #%>% #Add lists of authors, affiliations to df
    print(str_glue("{i} authors found.")) #print progress
    result = bind_rows(result, search_out)
    i = i+1
  }
  return(result)
}

ml_words <- c("artificial intelligence", "machine learning", "statistical learning", "deep learning", "NLP", "computer vision", "pattern recognition", "robotics", "neural", "evolutionary comput", "big data", "segmentation", "preprocess", "loss function", "adversarial", "reinforcement learning", "multiagent", "knowledge gradient", "robot", "computer vision")

scopus_papers = scopus_author_fn2(ms_papers)

#Filter to only output ML-related papers.
scopus_papers2 <- scopus_papers %>%
  filter(grepl(paste(ml_words, collapse="|"), `dc:description`, ignore.case = T))