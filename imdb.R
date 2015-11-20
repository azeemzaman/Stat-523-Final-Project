library(rvest)
library(stringr)

url = "http://www.imdb.com/title/tt0816692/"
d = read_html(url)

titles = html_nodes(d, "#overview-top")
x=titles[[1]]

df = data.frame(
  title = html_nodes(x,".itemprop")[1] %>% html_text() 
)

df$user_rating = html_nodes(x,".star-box-details") %>%
                 html_text() %>%
                 str_extract(pattern = "[0-9]\\.[0-9]") %>%
                 as.numeric()

df$metascore = html_nodes(x, ".star-box-details") %>%
               html_text()%>%
               str_extract(pattern = "Metascore: .[0-9]{2}")%>%
               str_extract(pattern = "[0-9]{2}")%>%
               as.numeric()

info = html_nodes(x,".txt-block") %>% 
       html_text() %>%
       str_trim()

df$director = str_replace(info[1], "^Director:\\n","")

actor = str_split(info[3], "\\n") %>%
        unlist()
df$actor1 = str_replace(actor[2],", ","")
df$actor2 = str_replace(actor[3],", ","")
df$actor3 = str_replace(actor[4],"\\|","")%>%
            str_trim()
           
df$country = html_nodes(d, "#titleDetails , .txt-block:nth-child(4)")[2]%>%
     html_text()%>%
     str_split("\\n")%>%
     str_extract(pattern = "USA")


