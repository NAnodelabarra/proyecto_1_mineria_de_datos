#install.packages("devtools")
library(devtools)

#install.packages("rtools")
library(rtools)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("knitr")
library(knitr)

#install.packages("spotifyr")
library(spotifyr)

#install.packages('lubridate')
library('lubridate')


#devtools::install_github('charlie86/spotifyr')
#1

#Sys.setenv(SPOTIFY_CLIENT_ID = '71ed90d87b9c40f8b3151636b042ff71')
#Sys.setenv(SPOTIFY_CLIENT_SECRET = 'bd616f0596044a6f9a8fc12d6ff29a96')

#access_token <- get_spotify_access_token()

dirpath <- paste(dirname(rstudioapi::getSourceEditorContext()$path), "/beats.rds", sep="")
Data <- readRDS(dirpath) #omitted 447595 rows

#Data
#summary(Data)
#glimpse(Data)
#colnames(Data)

Data <- Data[complete.cases(Data),] #omitted 272579 rows (aprox la mitad de los datos)

Data <- unique(Data) #omitted 271721 rows eliminamos los datos repetidos

Data


#######

Data$album_release_date <- stamp_date(Data$album_release_date)






#Data$album_release_date <- replace(Data$album_release_date, Data$album_release_date_precision == "year", paste(Data$album_release_date, "-01-01", sep="") )
#Data$album_release_date <- replace(Data$album_release_date, Data$album_release_date_precision == "month", paste(Data$album_release_date, "-01", sep="") )

#Data$album_release_date[Data$album_release_date_precision == "year"] <- Data$album_release_date[Data$album_release_date_precision == "year"]

#Data$album_release_date <- replace(Data$album_release_date, Data$album_release_date[Data$album_release_date_precision == "year"], paste(Data$album_release_date[Data$album_release_date_precision == "year"], "-01-01", sep="") )

nueva_album_release_date <- Data$album_release_date

for (i in Data$album_release_date) {
if ( nchar(Data$album_release_date[i]) <= 4) { Data$album_release_date[i] <- paste(toString(Data$album_release_date[i]),toString("-01-01"),sep="") }
}



toString("-01-01")


Data <- Data$album_release_date[Data$album_release_date_precision == "year"]+"-01-01"

Data


zzzz <- Data$album_release_date
zzzz


Data$album_release_date$2015-12-18




Data222  <- Data[Data$height_seca1 == "day"]
Data222



timestampppp <- stamp_date('2019-04-04')
timestampppp





datos_stings = c("artist_name", "artist_id", "album_id", "album_type")

datos_album_cachando = unique(Data[("album_release_date_precision")])

summary(datos_album_cachando)



album_release_date_precision
album_release_date = () # pasar a numero

datos_numeros = c("speechiness","duration","time_signature","acousticness","danceability","energy"
             ,"instrumentalness","liveness","loudness","valence","tempo")



Num_Data <- Data %>%
  select(num_cols)

#Checking Correlation between variables
library(corrplot)
M<-cor(Num_Data)
corrplot(M, method="circle")



