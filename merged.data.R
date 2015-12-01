library(magrittr)

# setwd(path.expand("Stat-523-Final-Project"))
setwd("~/Downloads/Stat-523-Final-Project")
load("boxoffice.Rdata")
setwd(paste0(getwd(), "/imdb"))
for (i in 1:length(list.files())) {
  load(files[i])

}
append.imdb <- rbind(imdb.info, imdb.info2, imdb.info3, imdb.info4, imdb.info5,imdb.info6, 
                     imdb.info7, imdb.info8)
# load imdb.Rdata since it has the same name as imdb2000.Rdata
load("imdb.Rdata")
append.imdb <- rbind(append.imdb, imdb.info)
# factor to characters
append.imdb$title <- as.character(append.imdb$title)
box.office$Title <- as.character(box.office$Title)

str.process <- function(Title) {
    tolower(Title) %>%
    gsub("[-???]"," ",.) %>%
    gsub("[^'`???[:alpha:][:space:]]","",.,perl=TRUE) %>%
    gsub(" ", "", ., fixed = TRUE)
    
}
append.imdb$Titles <- str.process(append.imdb$title)
box.office$Titles <- str.process(box.office$Title)
merge.data <- merge(box.office, append.imdb, by = "Titles") %>%
              .[!duplicated(.),] %>%
              .[,!(names(.) %in% "title")]
save(merge.data, file = "merged.Rdata")


