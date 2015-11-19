library(rvest)
library(XML)
# this function gets HTML and extracts the table with info
getTable <- function(url){
  table <- readHTMLTable(url)
  good.table <- table[[4]][6:105,]
  rownames(good.table) <- NULL
  colnames(good.table) <- c("Rank", "Title", "Studio", "Domestic Gross", "Total Theaters", "Opening", "Opening Theaters", "Open", "Close")
  return(good.table)
}
# make links to page with info
links <- vector(length = 7)
for (i in 1:7){
  links[i] <- paste0('http://www.boxofficemojo.com/yearly/chart/?page=', i, '&view=releasedate&view2=domestic&yr=2014&p=.htm' )
}
# scrape
tables <- lapply(links, getTable)
# bind into 1 data frame
box.office <- do.call(rbind, tables)
# clean table
# remove $ and ','
# replace N/A with NA
for (col in c("Domestic Gross", "Opening", "Total Theaters", "Opening Theaters")){
  box.office[,col] <- gsub("$", "", box.office[,col], fixed = TRUE)
  box.office[,col] <- gsub(",", "", box.office[,col], fixed = TRUE)
  box.office[,col] <- gsub("N/A", NA, box.office[,col], fixed = TRUE)
}
# convert numbers to numeric
numeric.cols <- c("Rank", "Domestic Gross", "Total Theaters", "Opening", "Opening Theaters")
box.office[,numeric.cols] <- sapply(box.office[,numeric.cols], as.numeric)
# save result
save(box.office, file = 'boxoffice.Rdata')