# Stat-523-Final-Project
Code for Stat 523 Final Project

### problems for scrapping imdb data
movie number tt4027148
>   df$country = html_nodes(d, "#titleDetails , .txt-block:nth-child(4)")[2]%>%
+     html_text()%>%
+     str_split("\\n")%>%
+     str_extract(pattern = "USA")
Error in UseMethod("xml_text") : 
  no applicable method for 'xml_text' applied to an object of class "NULL"
  
No country information
