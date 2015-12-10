# scrape the most popular celebs from IMDB
library(rvest)
library(stringr)
library(magrittr)

base.male <- "http://www.imdb.com/search/name?gender=male"
base.female <- "http://www.imdb.com/search/name?gender=female"

scrapeNames <- function(url){
  names <- read_html(url) %>%
           html_nodes(".name") %>%
           html_text() %>%
           strsplit("\\n") %>%
           sapply("[[",2) %>%
           str_trim()
  return(names)
}
page.nums <- seq(from = 1, to = 201, by = 50)
male.urls <- paste0(base.male, "&start=", page.nums)
female.urls <- paste0(base.female, "&start=", page.nums)

males <- lapply(male.urls, scrapeNames) %>%
         unlist()

females <- lapply(female.urls, scrapeNames) %>%
           unlist()

top.celebs <- data.frame(males = males, females = females)
save(top.celebs, file = "popular_celebs.Rdata")
