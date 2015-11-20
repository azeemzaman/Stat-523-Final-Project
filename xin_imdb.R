library(rvest)
library(magrittr)
library(stringr)
# Directory for the html files
dir.create("C:/2015Fall/523/final project/data/2014", recursive=TRUE, showWarnings = FALSE)

# For loop for downloading all html files of movies
num = seq(from=1,to=9009,by=50)
for (i in num)
{
  download.file(paste0("http://www.imdb.com/search/title?sort=moviemeter,asc&start=",i,"&title_type=feature&year=2014,2014"),
                destfile=paste0("C:/2015Fall/523/final project/data/2014/listings",i,".html"))
}

# Create a list of files
files = dir("C:/2015Fall/523/final project/data/2014/", pattern="*.html", full.names=TRUE) 

nodeset = NULL
for (file in files)
{
  # Extract the partial links to all movies
  nodes =  read_html(file) %>%
           html_nodes(".title") %>%
           as.character() %>%
           str_match("tt[0-9]{7}") 
  nodeset = rbind(nodeset,nodes)
}

# get the url of each movie
url = paste0("http://www.imdb.com/title/",nodeset)

get_data = function(tt)
{
  d = read_html(paste0("http://www.imdb.com/title/",tt))
  
  titles = html_nodes(d, "#overview-top")
  x=titles[[1]]
  
  df = data.frame(
    title = html_nodes(x,".itemprop")[1] %>% html_text() 
  )
  
  df$user_rating = tryCatch(html_nodes(x,".star-box-details") %>%
    html_text() %>%
    str_extract(pattern = "[0-9]\\.[0-9]") %>%
    as.numeric(), error = function(x)  {return(NA)})
  
  df$metascore = tryCatch(html_nodes(x, ".star-box-details") %>%
    html_text()%>%
    str_extract(pattern = "Metascore: .[0-9]{2}")%>%
    str_extract(pattern = "[0-9]{2}")%>%
    as.numeric(), error = function(x)  {return(NA)})
  
  info = tryCatch(html_nodes(x,".txt-block") %>% 
    html_text() %>%
    str_trim(), error = function(x)  {return(NA)})
  
  df$director = tryCatch(unlist(str_split(info[1], "\\n"))[2] %>%
    str_replace(", ",""), error = function(x)  {return(NA)})
  
  actor = tryCatch(str_split(info[3], "\\n") %>%
    unlist(), error = function(x)  {return(NA)})
  
  df$actor1 = tryCatch(str_replace(actor[2],", ",""), error = function(x)  {return(NA)})
  df$actor2 = tryCatch(str_replace(actor[3],", ",""), error = function(x)  {return(NA)})
  df$actor3 = tryCatch(str_replace(actor[4],"\\|","") %>% str_trim(), error = function(x)  {return(NA)})
  
  df$country = tryCatch(html_nodes(d, "#titleDetails , .txt-block:nth-child(4)")[2] %>%
    html_text() %>%
    str_split("\\n") %>%
    str_extract(pattern = "USA"), error = function(x)  {return(NA)})
  
  df$budget = tryCatch(read_html(paste0("http://www.imdb.com/title/",tt,"business?ref_=tt_dt_bus")) %>%  
    html_nodes("#tn15content") %>%
    html_text() %>%
    str_split("estimated") %>%
    .[[1]] %>%
    .[1] %>% 
    str_split("\\$") %>% 
    .[[1]] %>%
    .[2] %>%
    str_replace(" \\(",""), error = function(x)  {return(NA)})
  
  return(df)
}

imdb.data = lapply(nodeset[1:200],get_data)
imdb.info = do.call(rbind,imdb.data)
save(nodeset,file="C:/2015Fall/523/final project/data/2014/nodeset.Rdata")
save(imdb.info,file="C:/2015Fall/523/final project/data/2014/imdb.Rdata")


