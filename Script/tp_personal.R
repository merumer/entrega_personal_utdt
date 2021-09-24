
#Los c(*TP1 : TP3) (nerd!)* no tuvieron consignas "formales". Fueron indicaciones generales con propuestas para ejercitar parte de lo visto hasta ese momento en las clases. Que tengan libertad de elegir temas de su interes o que ya estuvieran trabajando, aplicando las herramientas que estamos aprendiendo.
#TP1: Consistía en elegir alguna fuente de datos, importarla en un proyecto de R, limpiarlos, transformalos, describirlos
#TP2: La propuesta original en clase fue  que intetaran hacer el scrape de los datos de TC de él cronista. Ayer hablamos de que pudieran entregar como TP un ejercicio análogo para cualquier otra fuente de datos
#TP3: será avanzar sobre lo anterior con el foco puesto en la entrega de una visualización de datos, siguiendo lo que avancemos las proximas dos semanas.


#En este trabajo voy a unificar el TP1 Y TP3, donde voy a desarrollar el trabajo realizado para centros de transbordo a partir de la palabra. 


#Se descargó desde Outscraper los datos de las reviews de Google del centro de Transbordo Once



#Tp 1:
library(tidyverse)
library(lubridate)
library(tidytext)
library(tokenizers)
library(readxl)
library(viridis)
library(dplyr)

once<- read_xlsx("Entradas/once_datos.xlsx") 

once <- once %>%  
  mutate(ano=  substr (review_datetime_utc, start = 7, stop =10)) %>% 
  mutate(mes=substr(review_datetime_utc, star=1, stop = 2)) 

once1 <- once %>% 
  select(c("review_rating", "ano", "mes")) %>% 
  group_by(ano, review_rating) %>%
  summarise(total = n()) 



ggplot(once) +
  aes(x = ano, fill = factor(review_rating)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Blues")+
  theme_light()+
  labs(title = "Puntuaciones por Año - Estación Once", 
       x = "Año",
       y = "Porcentaje de comentarios")



ggplot(once1) +
  geom_point(aes(y=total, x=ano, color =review_rating),
             size=10) +
  scale_fill_brewer(direction=-1,palette = "Blues") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title="Once",
       subtitle = "Cantidad de puntuaciones por año",
       x = "Año",
       y = "Puntuación",
       color="Total")



#///////////////////////////////////////////////////////


#Cuales son las palabras más utilizadas por las personas, para esto se  trabajó con una base de datos de las palabrás más utilizadas en el habla castellana de modo de realizan un "antijoin" y así evitar que aparezcan palabras del tipo "de, a, la", entre otras. 



once_tokenizado <- once %>% 
  unnest_tokens(word, review_text) 

once_tokenizado <- once_tokenizado %>% 
  
  filter(!is.na(once_tokenizado$word))

head(once_tokenizado)

stopwords_es <- read.csv("https://bitsandbricks.github.io/data/stopwords_es.csv",
                         stringsAsFactors = FALSE)

head(stopwords_es)


once_tokenizado <- once_tokenizado %>% 
  anti_join(stopwords_es, by = c("word" = "STOPWORD"))

toppalabras <- once_tokenizado %>% 
  count(word, sort = TRUE) %>% 
  head(n=40      )

once_resumido <- data.frame(word= c("Seguridad", "Servicio", "Baños", "Linda",
                                    "Limpieza", "Cuidado", "Horarios", "Pico", "Excelente",
                                    "Frecuencia"),
                            n= c(20, 19, 18,18,
                                 13, 16, 13, 11,11,9))

library(fmsb)



toppalabrasonce_radar <- as.data.frame(matrix( once_resumido$n , ncol=10))
colnames(toppalabrasonce_radar) <- c(once_resumido$word)

toppalabrasonce_radar <-   rbind(rep(20,10) , rep(0,10) ,toppalabrasonce_radar)


head(toppalabrasonce_radar)


radarchart(toppalabrasonce_radar,  axistype=1 , 
           
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=1 )




#///////////////////////////////////////////////////////
#genero



path <- 'https://www.dropbox.com/s/edm5383iffurv4x/nombres.csv?dl=1'
df <- read.csv(path, sep=",",  colClasses = "character")

libs<-c('tm','stringi')
lapply(libs,require, character.only= TRUE)
# FUNCIONES ------------------------------
clean_txt <- function(txt){
  txt <- stri_trim(gsub('[[:punct:][:digit:] ]+',' ',tolower(txt)))
  return(strsplit(txt, " ")[[1]])
}
get_gender2 <- function(nombre, lista_nombres="") {
  nombre <- clean_txt(nombre)
  nombre <- subset(nombre, nombre %in% lista_nombres)
  mylist <- list()
  mylist[c("f", "m","a")] <- 0
  for (i in 1:length(nombre)){
    g <- as.character(df[which(df$nombre == nombre[i]),"genero"])
    g <- ifelse(identical(g, character(0)), 'a', g)
    if(i==1){
      mylist[[g]] <- mylist[[g]] + 2
    } else{
      mylist[[g]] <- mylist[[g]] + 1
    }
    g2 = sapply(mylist, function(x) x[which.max(abs(x))])
    return(names(g2[g2==max(g2)])[1])
  }
}
# DATOS ---------------------------------
path <- 'https://www.dropbox.com/s/edm5383iffurv4x/nombres.csv?dl=1'
df <- read.csv(path, sep=",",  colClasses = "character")
get_gender2("jose maria altagracia", df$nombre)


once_genero <- once_tokenizado

once_genero %>% 
  #select(autor_name, autor_id) %>% 
  group_by(autor_id) %>% 
  nest() %>% 
  mutate(genero = map2(data, autor_id,
                       .f =  ~ get_gender2(nombre = .x$autor_name,
                                           lista_nombres = df$nombre))) %>% 
  unnest(cols = c(data, genero)) %>% 
  print(n = Inf)


once_genero <- once_genero %>% 
  #select(autor_name, autor_id) %>% 
  group_by(autor_id) %>% 
  nest() %>% 
  mutate(genero = map2(.x = data, 
                       .y = autor_id,
                       .f =  ~ get_gender2(nombre = .x$autor_name,
                                           lista_nombres = df$nombre))) %>% 
  unnest(cols = c(data, genero)) %>% 
  #select(genero, autor_name, everything()) %>% 
  print(n = Inf)



###graficos genero


once_f25 <- once_genero %>% 
   ungroup() %>% 
  count(word, genero,  sort = TRUE) %>% 
  data.frame() %>% 
  filter(genero == "f"| genero =="m") %>% 
  filter(word=="seguridad"| word=="servicio"|word=="baños"|word=="linda" |word=="limpieza"
         |word=="cuidado"|word=="horarios"|word=="pico"|word=="excelente"|word=="frecuencia") 



  ggplot(once_f25) +
    geom_bar(aes(x = word, weight =n, fill=genero))+
    labs(title = "Cantidad de siniestros por comuna según tipo de colisión",
         subtitle = "Ciudad Autónoma de Buenos Aires, 2015-2018",
         caption = "Fuente: portal de datos abiertos de la Ciudad - http://data.buenosaires.gob.ar",
         x = "Comuna",
         y = "Cantidad de siniestros ", 
         fill="Tipo de colisión")+
    scale_color_manual(values=c("#CC6666", "#9999CC"))+
    
    theme(  axis.text.x = element_text(size = 10, face = "italic"),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white", colour = "black", size = 1))+
    coord_flip() 



library(wordcloud2) 
library(wordcloud) 
wordcloud2(data=toppalabras, size=1.6)


png("#103_commonality_wordcloud.png", width = 480, height = 480)
wordcloud(commonality.cloud(once_tokenizado, max.words=100, random.order=FALSE))
dev.off()

#tp2: scrapping



# CARGO PAQUETES
library(rvest) # Easily Harvest (Scrape) Web Pages, CRAN v0.3.6
library(XML) # Tools for Parsing and Generating XML Within R and S-Plus, CRAN v3.99-0.5
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0

#### Wikipedia Scraping ######
#### Example: Parsing A Table From Wikipedia
####  https://steviep42.github.io/webscraping/book/index.html#example-parsing-a-table-from-wikipedia

## ARGENTINA ####

argentina <- "https://en.wikipedia.org/wiki/List_of_Argentine_provinces_by_population"


argentina2 <- read_html(argentina) %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table') %>% # Selector Gadget Copy Xpath
  html_table(fill = T)



argentina3 <- argentina2[[1]] %>% 
  rename("Provincia"= "Province/District") %>% 
  data.frame() 
 

 argentina3 [ argentina3  == "Autonomous City of Buenos Aires"] <- "Ciudad Autónoma de Buenos Aires"

 

 library(dplyr)
 library(stringr)
 argentina4 <- argentina3 %>% 
   mutate_all(list(~ str_replace_all(., ",", "") %>% 
                     as.numeric %>%
                     replace_na(0)))
 
 
 argentina5 <- argentina4 %>% 
   left_join(argentina3, by="Rank") %>% 
   ungroup()
 
 library(plyr)
 library(conflicted)
 conflict_prefer("rename", "dplyr")
 conflict_prefer("mutate", "dplyr")
 argentina5 <- argentina5 %>% 
   select(Provincia.y, "Population..2013..2..x") %>% 
  rename(Provincia  = Provincia.y) %>% 
  rename(Población = "Population..2013..2..x")
 
 

 
 argentina5 <- mutate(argentina5, porcentaje =Población/sum(Población)*100)
 
 
 argentina5<- ddply(argentina5, "Provincia", transform,
                               Etiquetas = paste(round(porcentaje, digits=1), '%'))

 
 ggplot(argentina5) +
   geom_col(aes(x = reorder(Provincia, Población), y = Población, 
                fill = ifelse(Provincia == "Buenos Aires", "A","B"))) +
   geom_hline(yintercept = 0) +
   scale_fill_manual(values=c(A="#CC6666", B="#F3E8CCFF")) +
   geom_text(aes(x = reorder(Provincia, as.numeric(Etiquetas)), 
                 y = as.numeric(porcentaje), 
                 label = Etiquetas),
             hjust = "inward") +
   labs(title = "Cantidad de siniestros por comuna",
        subtitle = "Ciudad Autónoma de Buenos Aires, 2015-2018",
        caption = "Fuente: portal de datos abiertos de la Ciudad - http://data.buenosaires.gob.ar",
        x = "Provincia",
        y = "Porcentaje") +
   coord_flip() +
   theme_minimal()+
   theme(plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"),
         plot.caption=element_text(face = "italic", colour = "gray35",size=10),
         title=element_text(size=10, face = "bold"), 
         legend.position = "none")+
   labs(title = "Cantidad de siniestros por comuna según tipo de colisión",
        subtitle = "Ciudad Autónoma de Buenos Aires, 2015-2018",
        caption = "Fuente: portal de datos abiertos de la Ciudad - http://data.buenosaires.gob.ar",
        x = "Provincias",
        y = "Población 2013 ", 
        fill="Tipo de colisión")+
   scale_color_manual(values=c("#CC6666", "#9999CC"))+
   
   theme(  axis.text.x = element_text(size = 10, face = "italic"),
           panel.grid = element_blank(),
           panel.background = element_rect(fill = "white", colour = "black", size = 1))+
   coord_flip() 
 
 
 
 ##### uno con el mapa
 
 
 
 library(geoAr)
 
 argentina_sf <- get_geo("ARGENTINA")
 
 library(sf)
 

 plot(argentina_sf)
 
 codigos <- show_arg_codes() %>% 
   print(n = Inf)
 
 
 argentina_sf2 <- get_geo("ARGENTINA",
                       simplified = FALSE)

 
 # MISCELANEAS
 
 argentina_sf2 %>% 
   add_geo_codes() %>% 
   mapview::mapview()
 
 
 mapedit::editFeatures(argentina)
 
 
 
 # SIMPLIFY GEOMETRY
 
 
 library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
 library(sf) # Simple Features for R, CRAN v0.9-7
 library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics, CRAN v3.3.3
 library(rmapshaper) # Client for 'mapshaper' for 'Geospatial' Operations, CRAN v0.4.4
 
 #### MAP ARGENTINA w/ PROVS ####
 
 #create a couple temp files
 
 temp <- tempfile()
 
 temp2 <- tempfile()
 
 #download the zip folder from the internet save to 'temp' 
 download.file("https://www.indec.gob.ar/ftp/cuadros/territorio/codgeo/Codgeo_Pais_x_prov_datos.zip",temp)
 #unzip the contents in 'temp' and save unzipped content in 'temp2'
 unzip(zipfile = temp, exdir = temp2)
 #finds the filepath of the shapefile (.shp) file in the temp2 unzip folder
 #the $ at the end of ".shp$" ensures you are not also finding files such as .shp.xml 
 your_SHP_file<-list.files(temp2, pattern = ".shp$",full.names=TRUE)
 
 #read the shapefile. Alternatively make an assignment, such as f<-sf::read_sf(your_SHP_file)
 mapa_arg <- sf::read_sf(your_SHP_file) %>% 
   select(codprov_censo = link, 
          geometry)
 
 plot(mapa_arg)
 
 
 mapa_arg <- mapa_arg %>%
   st_crop(xmin = -78.844299, ymin = -56.918980,
           xmax = -53.531800, ymax = -20.341163) 

 
 
 mapa_arg_final <- left_join(mapa_arg, codigos, by= "codprov_censo")
 
 mapa_arg_final <- mapa_arg_final %>% 
   rename(Provincia = name_iso)
 
 
 mapa_población <- left_join(mapa_arg_final, argentina5, by= "Provincia")
 
 ggplot() +
   geom_sf(data =  mapa_población, aes(fill=Población)) +
   geom_sf_text(data=mapa_población, aes(label = Etiquetas), size=2.5, colour = "black")+
   labs(title = "Distribución de la población de Argentina",
                fill = "Cantidad de habitantes por Provincia",
        caption= "Fuente: Indec",
        y="",
        x="") +
   scale_fill_gradient(low="khaki2", high="deeppink4")+
   theme_void()
 
 
 
 ###Scrapping
 
 url_cronista <- "https://www.cronista.com/MercadosOnline/dolar.html"
 
 
 
moneda <- read_html(url_cronista) %>% 
   html_nodes(xpath = '//*[@id="market-scrll-2"]/li/a/span[1]') %>% 
   html_text()
 
 
compra <- read_html(url_cronista) %>% 
  html_nodes(xpath = '//*[@id="market-scrll-2"]/li/a/span[3]/div/div[2]') %>% 
  html_text()


venta <- read_html(url_cronista) %>% 
  html_nodes(xpath = '//*[@id="market-scrll-2"]/li/a/span[4]/div/div[2]') %>% 
  html_text()      
               

variacion <- read_html(url_cronista) %>% 
  html_nodes(xpath='//*[@id="market-scrll-2"]/li/a/span[2]/span[2]') %>% 
  html_text()      



 tabla <- data.frame(moneda, variacion, compra)
 
 head(tabla)
 







