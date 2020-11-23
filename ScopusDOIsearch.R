#Scopus search by DOI

library(rscopus)
library(dplyr)

#API key
rscopus::set_api_key(api_key = "")

#Function
scopus_doi_fn <- function(data) {
  data <- data %>% drop_na(doi)
  result <- data.frame(matrix(nrow=0,ncol=0)) #Instantiate df.
  for (i in 1:nrow(data)){
    query = paste("DOI(", #Add query
                  #                  "{", #Exact title match
                  data$doi[i],
                  #                  "}",
                  ")",
                  sep="")
    search_res = try(scopus_search(query = query, view="COMPLETE", count=24, wait_time = 2)) #Query Scopus
    search_df = try(gen_entries_to_df(search_res$entries)) #Convert to df
    auth_list = list(search_df$author)
    affil_list = list(search_df$affiliation)
    search_out = search_df$df %>% #Add lists of authors, affiliations to df
      mutate(authors_list = auth_list,
             affiliations_list = affil_list)
    print(str_glue("{i} papers found.")) #print progress
    #if (nrow(result) %% 100 == 0) print(str_glue("{i} papers found."))
    result = bind_rows(result, search_out)
    i = i+1
  }
  return(result)
}

doi_data <- scopus_doi_fn(ms_papers[c(110001:115232),])
write_rds(doi_data, path = "~/Documents/doi_data_110001-115232.rds")