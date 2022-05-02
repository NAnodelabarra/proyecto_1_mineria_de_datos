
 _______   _______    ______   __      __  ________   ______   ________  ______           __         
/       \ /       \  /      \ /  \    /  |/        | /      \ /        |/      \        _/  |        
$$$$$$$  |$$$$$$$  |/$$$$$$  |$$  \  /$$/ $$$$$$$$/ /$$$$$$  |$$$$$$$$//$$$$$$  |      / $$ |        
$$ |__$$ |$$ |__$$ |$$ |  $$ | $$  \/$$/  $$ |__    $$ |  $$/    $$ |  $$ |  $$ |      $$$$ |        
$$    $$/ $$    $$< $$ |  $$ |  $$  $$/   $$    |   $$ |         $$ |  $$ |  $$ |        $$ |        
$$$$$$$/  $$$$$$$  |$$ |  $$ |   $$$$/    $$$$$/    $$ |   __    $$ |  $$ |  $$ |        $$ |        
$$ |      $$ |  $$ |$$ \__$$ |    $$ |    $$ |_____ $$ \__/  |   $$ |  $$ \__$$ |       _$$ |_       
$$ |      $$ |  $$ |$$    $$/     $$ |    $$       |$$    $$/    $$ |  $$    $$/       / $$   |      
$$/       $$/   $$/  $$$$$$/      $$/     $$$$$$$$/  $$$$$$/     $$/    $$$$$$/        $$$$$$/       
  
       __       __  ______  __    __  ________  _______   ______   ______                                  
      /  \     /  |/      |/  \  /  |/        |/       \ /      | /      \                                 
      $$  \   /$$ |$$$$$$/ $$  \ $$ |$$$$$$$$/ $$$$$$$  |$$$$$$/ /$$$$$$  |                                
      $$$  \ /$$$ |  $$ |  $$$  \$$ |$$ |__    $$ |__$$ |  $$ |  $$ |__$$ |                                
      $$$$  /$$$$ |  $$ |  $$$$  $$ |$$    |   $$    $$<   $$ |  $$    $$ |                                
      $$ $$ $$/$$ |  $$ |  $$ $$ $$ |$$$$$/    $$$$$$$  |  $$ |  $$$$$$$$ |                                
      $$ |$$$/ $$ | _$$ |_ $$ |$$$$ |$$ |_____ $$ |  $$ | _$$ |_ $$ |  $$ |                                
      $$ | $/  $$ |/ $$   |$$ | $$$ |$$       |$$ |  $$ |/ $$   |$$ |  $$ |                                
      $$/      $$/ $$$$$$/ $$/   $$/ $$$$$$$$/ $$/   $$/ $$$$$$/ $$/   $$/                                 
   
#############       SEGUNDO ARCHIVO
   
#LAS LINEAS QUE GENERAN ARCHIVOS ESTAN COMENTADAS PARA OPTIMIZAR RENDIMIENTO (ARCHIVOS ADJUNTOS EN GITHUB)

#############   Instalamos paquetes necesarios


#install.packages("devtools")
library(devtools)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("knitr")
library(knitr)

#install.packages("spotifyr")
library(spotifyr)

#install.packages('corrplot')
library('corrplot')

#install.packages('NbClust')
library('NbClust')

#install.packages("dbscan")
library(dbscan) 


#############   Cargamos los datos

dirpath <- paste(dirname(rstudioapi::getSourceEditorContext()$path), "/beats.rds", sep="")
Data <- readRDS(dirpath) #omitted 447595 rows
#observamos
#Data
#summary(Data)
#glimpse(Data)
#colnames(Data)



#############   Limpieza y preparacion 

Data <- Data[complete.cases(Data),] #omitted 272579 rows (aprox la mitad de los datos)
Data <- unique(Data) #omitted 271721 rows eliminamos los datos repetidos
#Data

#botamos datos considerados irrelevantes
drop <- c("album_release_date","album_type","album_release_date_precision", "available_markets","copyrights", "analysis_url", "disc_number", "time_signature", "is_local", "track_href", "track_preview_url", "type", "external_urls.spotify", "key_mode", "key_name", "mode_name")
Data = Data[,!(names(Data) %in% drop)]
#convertimos bool a int
Data$explicit <- as.integer(as.logical(Data$explicit))

data_numerica = Data[c("album_release_year", "danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "explicit", "track_number") ]
#data_numerica

data_categorica = Data[c("track_name","track_id", "artist_name", "artist_id", "album_name", "album_id", "duration_ms", "track_uri") ]
#data_categorica



#############   Preprocesamiento
#cor(data_numerica)
#corrplot(cor(data_numerica)) #plot_correlaciones
#boxplot(data_numerica) #boxplot_pre_normalizacion

data_numerica_normalizada <- scale(data_numerica) %>% as_tibble()

#boxplot(data_numerica_normalizada) #boxplot_post_normalizacion


#############   CODIGO PLAYLIST
#############   CODIGO PLAYLIST
#############   CODIGO PLAYLIST
#############   CODIGO PLAYLIST
#############   CODIGO PLAYLIST

#id de una canción de spotify 
#Para cada canción de la lista se deberá guardar el nombre del grupo, la canción seleccionada, y el tiempo.

ID_CANCION <- readline(prompt="ID CANCNION ESCOGIDA: ")

modelo_kmeans <- kmeans(data_numerica_normalizada, centers = 15)

data_numerica_normalizada$clus <- modelo_kmeans$cluster %>% as.factor()

ggplot(data_numerica_normalizada, aes(energy, acousticness, color=clus)) +
  geom_point(alpha=0.5, show.legend = F) +
  theme_bw()

modelo_kmeans$cluster


#buscamos en que cluster esta la cancion escogida

for(i in 1:15){
  index_del_cluster <- row.names(data_numerica[data_numerica_normalizada$clus==i,])
  index_de_LA_CANCION <- which(data_categorica$track_id %in% ID_CANCION)
  IS_IT_IN_CLUSTER <- length(as.logical(as.integer(which(index_del_cluster %in% index_de_LA_CANCION))))
  if(IS_IT_IN_CLUSTER!=0){
    ESTE_CLUSTER <- (i)
  }
}

#generamos la playlist

playlist_maxima <- data_categorica[data_numerica_normalizada$clus==ESTE_CLUSTER,]
duracion_playlist <- 0
hasta_donde_llegan <- 1

while(TRUE){
  duracion_playlist <- duracion_playlist + data_categorica[i,"duration_ms"]
  hasta_donde_llegan <- hasta_donde_llegan + 1
  if(10800000 < duracion_playlist){
    break
  }
}

duracion_playlist

hasta_donde_llegan

PLAYLIST_LISTA <- playlist_maxima[1:hasta_donde_llegan,]

PLAYLIST_LISTA


export(PLAYLIST_LISTA, "PLAYLIST.csv")



