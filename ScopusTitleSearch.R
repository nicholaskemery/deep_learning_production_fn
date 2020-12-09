library(rscopus)
library(dplyr)
library(fuzzyjoin)

#Fuzzy string matching - build dataset
scopus_paper_fn <- function(data) {
  result <- data.frame(matrix(nrow=0,ncol=0)) #Instantiate df.
  for (i in 1:nrow(data)){
    query = paste("title(", #Add query
                  #                  "{", #Exact title match
                  data$title_ms[i],
                  #                  "}",
                  ")",
                  sep="")
    search_res = tryCatch(scopus_search(query = query, view="COMPLETE", count=24, max_count=100, wait_time = 1),
                          error=function(e) NULL) #Query Scopus
    search_df = tryCatch(gen_entries_to_df(search_res$entries),
                         error=function(e) NULL) #Convert to df
    auth_list = list(search_df$author)
    affil_list = list(search_df$affiliation)
    search_out = search_df$df %>% #Add lists of authors, affiliations to df
      mutate(authors_list = auth_list,
             affiliations_list = affil_list)
    #search_out_fuzzy = 
    print(str_glue("{i} papers found.")) #print progress
    #if (nrow(result) %% 100 == 0) print(str_glue("{i} papers found."))
    result = bind_rows(result, search_out)
    i = i+1
  }
  return(result)
}
articles = c("a", "an", "the", "A", "An", "The")
fuzz_ms = ms_papers
fuzz_scopus = scopus_paper_fn(fuzz_ms)

fuzz_scopus2 = fuzz_scopus %>%
  mutate(title_ms = `dc:title`) %>%
  drop_na(`dc:title`) %>%
  mutate(title_ms = removeWords(title_ms, articles)) #Prepare for joining.
fuzz_ms2 <- fuzz_ms %>%
  drop_na(title_ms) %>%
  mutate(title_ms = removeWords(title_ms, articles)) #Prepare for joining.

fuzz_out = stringdist_inner_join(fuzz_ms2, #Join datasets using DL distance.
                                 fuzz_scopus2,
                                 by="title_ms",
                                 method ="dl",
                                 ignore_case = T)

#In a random sample of 50 papers (4321 seed), extracted 23/25 available papers.
#Overall, retreived around 51% of the MS dataset.
#Missed around 500 papers in the DOI-based search.
