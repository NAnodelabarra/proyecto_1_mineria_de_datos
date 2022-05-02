
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
 
#install.packages("fpc")
library(fpc)


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
Data

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



#############   PROCESAMIENTO K MEANS
#############   PROCESAMIENTO K MEANS
#############   PROCESAMIENTO K MEANS
#############   PROCESAMIENTO K MEANS

# buscamos el mejor k
numeros_k <- numeric(50)

for(k in 1:50){
  modelo <- kmeans(data_numerica_normalizada, centers = k)
  numeros_k[k] <- modelo$tot.withinss
}

plot(numeros_k,  type = "b", pch = 19, col = "red", main="Buscando el optimo numero k con el metodo del codo", 
     xlab = "Numero de clusters.", ylab = "Cuantas entidades abarcan.")


# DETERMINAMOS EL MEJOR K COMO 15

modelo_kmeans <- kmeans(data_numerica_normalizada, centers = 15)

data_numerica_normalizada$clus <- modelo_kmeans$cluster %>% as.factor()

ggplot(data_numerica_normalizada, aes(energy, acousticness, color=clus)) +
  geom_point(alpha=0.5, show.legend = F) +
  theme_bw()


#############   datos del clustering

# tamaño de los clusters
modelo_kmeans$size 
# centros de los clusters
modelo_kmeans$centers
# suma de cuadrados
modelo_kmeans$totss
# suma de cuadrados dentro de clusters
modelo_kmeans$withinss
#  de cuadrados dentro de clusters total
modelo_kmeans$tot.withinss
# suma de cuadrados entre de clusters
modelo_kmeans$betweenss
 


#############   PROCESAMIENTO DB SCAN
#############   PROCESAMIENTO DB SCAN
#############   PROCESAMIENTO DB SCAN
#############   PROCESAMIENTO DB SCAN






kNNdistplot(data_numerica_normalizada, k=3)
abline(h=1.5, lty=2)

# DETERMINAMOS EL eps = 1.5


modelo_dbscan <- dbscan(data_numerica_normalizada, eps = 1.5, minPts = 1000)

data_numerica_normalizada$clus <- modelo_dbscan$clus %>% as.factor()

ggplot(data_numerica_normalizada, aes(energy, acousticness, color=clus)) +
  geom_point(alpha=0.5, show.legend = F) +
  theme_bw()


# tamaño de los clusters
modelo_dbscan$size 
# centros de los clusters
modelo_dbscan$centers
# suma de cuadrados
modelo_dbscan$totss
# suma de cuadrados dentro de clusters
modelo_dbscan$withinss
#  de cuadrados dentro de clusters total
modelo_dbscan$tot.withinss
# suma de cuadrados entre de clusters
modelo_dbscan$betweenss



#cluster.stats(d, data_numerica_normalizada$clus, data_numerica_normalizada$clus ) 



#preferimos el algoritmo kmeans ya que es el que nos dara los clusters 
# con menor distancia entre el centro y el punto mas alejado del centro.
# En el enunciado nos piden las canciones mas parecidas a la escogida y 
# con DBSCAN queda la posibilidad de alejarse demasiado de esta. 

#DBSCAN podria servir si uno quisiera hacer una playlist donde las canciones
# se parecen cada una con la  haciendo una buena transicion pero al comparar 
# la primera con la ultima no se parezcan tanto.
























