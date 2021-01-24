library(dplyr)
library(rscopus)

#Function to extract paper info for *first* author per search.
scopus_author_fn <- function(data) {
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

#List of ML/AI words to filter paper titles.
ml_words <- c("artificial intelligence", "machine learning",
              "statistical learning", "deep learning", "NLP",
              "computer vision", "pattern recognition",
              "robotics", "neural", "evolutionary comput",
              "big data", "segmentation", "preprocess",
              "loss function", "adversarial",
              "reinforcement learning", "multiagent",
              "knowledge gradient", "robot")

#Run function.
data <- scopus_author_fn(data)

#Filter out non-AI authors/papers.
data <- data %>%
  filter(grepl(paste(ml_words, collapse="|"),
               `dc:description`,
               ignore.case = T))
