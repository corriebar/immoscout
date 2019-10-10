library(tidyverse)
library(rvest)         # for scraping
library(stringr)       # for string manipulation
library(glue)          # evaluate expressions in strings
library(jsonlite)      # to handle json data
library(foreach)       # to run in parallel
library(doMC)          # backend to run in parallel (only for linux)
library(here)          # relative paths
 
url <- "https://www.immobilienscout24.de/Suche/S-2/Wohnung-Miete"

first_page <- read_html(url)

last_page_number <- first_page %>%
    html_nodes("#pageSelection > select > option") %>%
    html_text %>%
    length 

   
page_url <- "https://www.immobilienscout24.de/Suche/S-2/P-{pages}/Wohnung-Miete"
pages <- 1:last_page_number
page_list <- glue(page_url)
 
listing_ids <- first_page %>% html_nodes(".result-list__listing") %>%
    xml_attr("data-id") 
 
listing_url <- "https://www.immobilienscout24.de/expose/"
listing_list <- str_c(listing_url, listing_ids)
 



get_listing_data <- function(listing_url){
  list_page <- try(read_html(listing_url))
  if (any(class(list_page) == "try-error")) return(NULL)
  list_l <- list_page %>% 
    html_text %>%
    str_extract("(?<=keyValues = )(\\{.*?\\})") %>%
    str_remove_all("obj_") %>%    # to save some typing later on
    str_replace_all("false", "FALSE") %>%
    str_replace_all("true", "TRUE") %>%
    fromJSON  
  
  list_l$description <- list_page %>%
    html_nodes(".is24qa-objektbeschreibung") %>%
    html_text 
  
  list_l$facilities <- list_page %>%
    html_nodes(".is24qa-ausstattung") %>%
    html_text
  
  # extract id back from url
  list_l$id <- str_extract(listing_url, "(?<=/)\\d+")
  
  
  list_l %>% map_if(is_empty, function(x) {NA}) %>%
    as_tibble
}


#### in parallel
n_cores <- detectCores()
registerDoMC(n_cores)

foreach(i=1:last_page_number) %dopar% {
  
  link <- page_list[i]
  page <- try(read_html(link))
  if (!any(class(page) == "try-error") ){
    listing_ids <- page %>% html_nodes(".result-list__listing") %>%
      xml_attr("data-id")
    listing_url <- "https://www.immobilienscout24.de/expose/"
    listing_list <- str_c(listing_url, listing_ids)
    
    map(listing_list, get_listing_data) %>%
      bind_rows() %>%
      write_csv(paste0(here::here("data/rawdata/"),str_pad(i, 4, pad="0"), "_", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv" ) )
  }
}
