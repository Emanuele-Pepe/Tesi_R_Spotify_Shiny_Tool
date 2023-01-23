# UTILITIES AND PACKAGES ----------------------------------------------------------

# install.packages('spotifyr', depndencies = T)
# devtools::install_github('charlie86/spotifyr')

options(scipen = 9999)

library(spotifyr)
library(data.table)
library(ggplot2)
library(ggpubr)
library(factoextra)
library(highcharter)
library(corrplot)
library(tidyr)
library(janitor)
library(archetypes)
library(cluster)
library(factoextra)
library(NbClust)
library(stats)
library(plotly)
library(purrr)
library(ggthemes)
library(nnls)
library(nnet)


# API AUTHENTICATION AND AUTHORIZATION --------------------------
Sys.setenv(SPOTIFY_CLIENT_ID = '')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '')

access_token <- get_spotify_access_token()


# DATA REQUEST -------------------------------

     ## GET Tracks -----------------------------

offset_track_index <- 120L
tracklist = list()

for (i in 0: offset_track_index){
  
  tracks <- data.frame(get_my_saved_tracks(
    limit = 50,
    offset = (50*i),
    authorization = get_spotify_authorization_code(scope = scopes()),
    include_meta_info = FALSE))
  tracklist[[i +1]] <- tracks
} 

dt_tracks <- rbindlist(tracklist)

  ## GET AudioFeatures ------------------------
tracks_id <- dt_tracks$track.id

tracks_id_list <- as.list(tracks_id)
id_n <- 61L
trackfeature_list <- list()

for (i in 1 : id_n){
  
  audio_features <- get_track_audio_features(
    tracks_id_list[(((i-1)*100)+1) : (((i-1)*100)+100)], 
    authorization = access_token) 
  trackfeature_list[[i]] <- audio_features
  
}

dt_audio_features <- rbindlist(trackfeature_list)
dt_audio_features <- dt_audio_features[complete.cases(dt_audio_features[, ])]
setnames(dt_audio_features, old = "id", new = "track.id")


  ## GET Artists -----------------------------------------
artist_list <- list()

for (i in 1:nrow(dt_tracks)){
  track_id <- dt_tracks[i, .SD, .SDcols = "track.id"]
  artist <- t(unlist(dt_tracks[i, .SD, .SDcols = "track.artists"]))
  track_artist <- cbind(track_id, artist)
  artist_list[[i]] <- track_artist
}

dt_artists <- rbindlist(artist_list, fill = T)

dt_artists[, track.mainArtist.id := fifelse(is.na(track.artists.id), track.artists.id1, track.artists.id )]
dt_artists[, track.mainArtist.name := fifelse(is.na(track.artists.name), track.artists.name1, track.artists.name )]


dt_artists[, featuring := fifelse(is.na(track.artists.href1), 0, 1)]
kc_artist <- c("track.id", "track.mainArtist.id", "track.mainArtist.name", "featuring")

dt_artists <- dt_artists[, .SD, .SDcols = kc_artist]
artists_id <- unique(dt_artists$track.mainArtist.id)


offset_artist_index <- 45L
genre_artist_list= list()


for (i in 0: offset_artist_index){
  
  artist_genre <- get_artists(
    ids = artists_id[(1 + (50*i)) : (50+(i*50))],
    authorization = access_token,
    include_meta_info = FALSE)
  genre_artist_list[[i +1]] <- artist_genre
} 

dt_artist_genre <- rbindlist(genre_artist_list)
# dt_artist_genre[, genres := lapply(dt_artist_genre$genres, unlist)]

dt_artist_genre[lengths(genres) == 0, genres :=  NA]


dt_artist_genre[, main_genre := mapply('[', genres,1,SIMPLIFY = TRUE)]
setnames(dt_artist_genre,old = c("id", "name"), new =c("mainArtist.id", "mainArtist.name"))
kc_genres <- c("mainArtist.id" , "mainArtist.name", "popularity", "followers.total", "main_genre")
dt_artist_genre_final <- dt_artist_genre[, .SD, .SDcols = kc_genres]
dt_artist_genre_final <- dt_artist_genre_final[!is.na(mainArtist.id)]

genre_list <- list(dt_artist_genre_final = dt_artist_genre_final,dt_artists =  dt_artists)

# MERGE  -----------------------


dt_tracks_analysis <- merge(dt_tracks, dt_audio_features, by = "track.id")
dt_tracks_analysis <- merge(dt_tracks_analysis, dt_artists, by = "track.id")
dt_tracks_analysis <- merge(dt_tracks_analysis, dt_artist_genre_final, by.x = "track.mainArtist.id", by.y = "mainArtist.id")
setkey(x = dt_tracks_analysis, ... = track.id)
skimr::skim_without_charts(dt_tracks_analysis)

# CLEANING ------------------------------
dt_tracks_analysis[, featuring := as.factor(featuring)]
dt_tracks_analysis[, release_year := as.factor(as.numeric(substr(track.album.release_date, start = 1, stop = 4)))]
dt_tracks_analysis[, key := as.factor(key)]
dt_tracks_analysis[, mode := as.factor(mode)]


kc_analysis <- c()
# 
# # Genre -----------------------
# dt_tracks_analysis_finale <- dt_tracks_analysis[!is.na(main_genre)]
# skimr::skim_without_charts(dt_tracks_analysis_finale)
# unique(dt_tracks_analysis_finale$main_genre)
# 
# # Hip Hop
# unique(grep(pattern = "hip hop", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# unique(grep(pattern = "lo-fi", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# unique(grep(pattern = "rap", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# # Rock
# unique(grep(pattern = "rock", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# unique(grep(pattern = "metal", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# # afro-american
# unique(grep(pattern = "blues", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# unique(grep(pattern = "jazz", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# unique(grep(pattern = "soul", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# unique(grep(pattern = "r&b", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# unique(grep(pattern = "funk", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# # Pop
# unique(grep(pattern = "pop", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# 
# # Elettronic
# unique(grep(pattern = "tech", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# unique(grep(pattern = "electr", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# unique(grep(pattern = "house", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# unique(grep(pattern = "trance", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# 
# # Reggae
# unique(grep(pattern = "dub", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# unique(grep(pattern = "reggae", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# unique(grep(pattern = "dancehall", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# 
# # Indie
# unique(grep(pattern = "indie", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# # Classical
# unique(grep(pattern = "classical", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))
# 
# 
# 
# toMatch <- c("rap" , "jazz")
# 
# matches <- unique (grep(paste(toMatch,collapse = "|"), 
#                         dt_tracks_analysis_finale$main_genre, value=TRUE))
# 
# notMatch <- c ("hip hop", "lo-fi", "rap", "rock", "metal", "blues", "jazz", "soul", "r&b", "funk", "pop", "tech", 
#               "electr", "house", "trance", "dub", "reggaae", "dancehall")
# 
# prova <- dt_tracks_analysis_finale[!main_genre %chin%
#                            unique(grep(pattern = paste(notMatch,collapse="|"),
#                                        x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T)
#                                   ),
#                          unique(.SD), .SDcols = "main_genre" ]
# 
# write.csv(prova, "/home/emanuele/Desktop/TESI /Tesi_R_Spotify_Shiny_Tool/prova.csv", row.names=FALSE)
# 
# 
# trascodifica <-read.csv("trascodifica_generi_mancanti.csv", header = T, sep = ",")
# dt_trascodifica <-setDT(trascodifica)
# dt_trascodifica <- clean_names(dt_trascodifica)
# dt_tracks_analysis_finale <- merge(dt_tracks_analysis_finale, dt_trascodifica, by.x = "main_genre", by.y = "genre", all.x = T)
# 
# # hip hop
# dt_tracks_analysis_finale <- dt_tracks_analysis_finale[,
#                                                        trascodifica := fifelse(
#                                                          main_genre %chin% unique(grep(pattern = "hip hop", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T)),
#                                                          "hip hop", trascodifica 
#                                                          )
#                                                        ]
# dt_tracks_analysis_finale <- dt_tracks_analysis_finale[,
#                                                        trascodifica := fifelse(
#                                                          main_genre %chin% unique(grep(pattern = "lo-fi", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T)),
#                                                          "hip hop", trascodifica
#                                                          )
#                                                        ]
# dt_tracks_analysis_finale <- dt_tracks_analysis_finale[,
#                                                        trascodifica := fifelse(
#                                                          main_genre %chin% unique(grep(pattern = "rap", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T)),
#                                                          "hip hop", trascodifica
#                                                          )
#                                                        ]
# 
# 
# dt_tracks_analysis_finale[main_genre %chin% unique(grep(pattern = "lo-fi", x = dt_tracks_analysis_finale$main_genre, ignore.case = T, perl = F, value = T))]
# 
# dt_tracks_analysis_finale
# 
# 



# STATISTICHER DESCRITTIVE --------------------------------------

names(dt_tracks_analysis)

# added at;  "release_year"; "featuring",  track.explicit; track.popularity;   
# track.duration_ms; danceability"; "energy"; "key" ; "loudness";"mode";"
# speechiness" ;"valence";"tempo";"main_genre"; 

# INFORMAZIONI GENERALI

  hchart(dt_tracks_analysis_finale$release_year, 
    type = "area", name = "Distribuzione Anno Pubblicazione Brano",
  )
  
  hchart(dt_tracks_analysis_finale$featuring, 
         type = "column", name = "Composizione Collaborazioni"
  )
  
  highchart() %>% 
    hc_add_series(dt_tracks_analysis_finale$featuring, type = "column")
  
# INFORMAZIONI AUDIO 
  
  highchart() %>% 
    hc_add_series(dt_tracks_analysis_finale$track.duration_ms, type = "density")
  
  
  
  
  
  
  
  
  
  
  
  
  
# CLUSTER -----------------------
  
set.seed(123)
  
db_cluster = dt_tracks_analysis[, .( danceability, energy, 
                                    loudness, speechiness, acousticness, 
                                    instrumentalness, valence, tempo )]

corrplot(corr =  cor(db_cluster), method = "square")

  
wss <- function(k) {
  kmeans(as.matrix(db_cluster), k, nstart = 10 )$tot.withinss
}
  
k.values <- 1:15
  
wss_values <- map_dbl(k.values, wss)
  
df_k = data.table(k.values = k.values, wss_values = wss_values)
  
df_k |> 
  
  ggplot(aes(x = k.values, y = wss_values)) +
  geom_line(color = '#A3BFD9', size = 1) +
  geom_point(color = '#164773', size = 3) +
  ggtitle("Within Sum Squares comparison between k-means clustering") +
  xlab('# of centroids') + 
  ylab('Within Sum of Squares')

# k-mean k = 3

set.seed(313)

model_km_3 <- kmeans(db_cluster, centers = 3)


cluster_3 <- model_km_3$cluster

dt_tracks_analysis[, clus_3 := as.factor(cluster_3)]

table(dt_tracks_analysis$clus_3)


fviz_cluster(model_km_3, data = db_cluster,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


ggplot(dt_tracks_analysis, aes(x = clus_3, y = danceability, color = as.character(clus_3))) + 
  scale_color_brewer(type = 'qual', palette = 2) +
  geom_boxplot()



ggplot(dt_tracks_analysis, aes(x = clus_3, y = energy, color = as.character(clus_3))) + 
  scale_color_brewer(type = 'qual', palette = 2) +
  geom_boxplot()

ggplot(dt_tracks_analysis, aes(x = clus_3, y = loudness, color = as.character(clus_3))) + 
  scale_color_brewer(type = 'qual', palette = 2) +
  geom_boxplot()

ggplot(dt_tracks_analysis, aes(x = clus_3, y = speechiness, color = as.character(clus_3))) + 
  scale_color_brewer(type = 'qual', palette = 2) +
  geom_boxplot()

ggplot(dt_tracks_analysis, aes(x = clus_3, y = acousticness, color = as.character(clus_3))) + 
  scale_color_brewer(type = 'qual', palette = 2) +
  geom_boxplot()

ggplot(dt_tracks_analysis, aes(x = clus_3, y = instrumentalness, color = as.character(clus_3))) + 
  scale_color_brewer(type = 'qual', palette = 2) +
  geom_boxplot()




# PCA --------------------

prova_pca  <- prcomp(x = db_cluster, scale = T, center = T)

summary(prova_pca)


fviz_eig(
  prova_pca, choice = "variance", geom = c("bar", "line"), linecolor = "#00203FFF", 
  barcolor = "#ADEFD1FF", barfill = "#ADEFD1FF")+
  labs(
    title = "Quarterly Profit (in million U.S. dollars)",
    subtitle = "A simple bar chart",
    caption = "Source: ImaginaryCo"
  )+
  theme(
    plot.title = element_text(color = "#0099f9", size = 20),
    plot.subtitle = element_text(face = "bold"),
    plot.caption = element_text(face = "italic")
  )


dt_tracks_analysis[, pc1 := prova_pca$x[, 1]]
dt_tracks_analysis[, pc2 := prova_pca$x[, 2]]
dt_tracks_analysis[, pc3 := prova_pca$x[, 3]]

plot_ly(dt_tracks_analysis, x=~pc1, y=~pc2, 
        z=~pc3, color=~clus_3) %>%
  add_markers(size=1.5)

dt_tracks_analysis |>
  ggplot(aes(x = pc1, y = pc2, color = as.character(clus_3)))+
  geom_point(size = 5)+
  ggtitle("kmeans")

dt_tracks_analysis |>
  ggplot(aes(x = pc3, y = pc2, color = as.character(clus_3)))+
  geom_point(size = 5)+
  ggtitle("kmeans")


plot_ly(dt_tracks_analysis, x=~pc1, y=~pc2, 
        z=~pc3, color=~clus_3) %>%
  add_markers(size=1.5)

ggplot(dt_tracks_analysis, aes(x = clus_3, y = pc1, color = as.character(clus_3))) + 
  scale_color_brewer(type = 'qual', palette = 2) +
  geom_boxplot()

ggplot(dt_tracks_analysis, aes(x = clus_3, y = pc2, color = as.character(clus_3))) + 
  scale_color_brewer(type = 'qual', palette = 2) +
  geom_boxplot()


ggplot(dt_tracks_analysis, aes(x = clus_3, y = pc3, color = as.character(clus_3))) + 
  scale_color_brewer(type = 'qual', palette = 2) +
  geom_boxplot()

fviz_pca_ind(prova_pca,
             title = "Cluster K = 3",
             geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = dt_tracks_analysis$clus_3, 
             legend.title = "Clusters",
             palette = c("#1B9E77", "#D95F02", "#7570B3"), 
             addEllipses = F,
             repel = TRUE)




names(dt_tracks_analysis)


# LOGISTIC REGRESSION

mylogit <- glm(clus_3 ~ track.duration_ms + track.popularity + danceability + energy + loudness + 
                 speechiness+ acousticness + instrumentalness + liveness + valence + tempo , 
               data = dt_tracks_analysis, family = "binomial")

summary(mylogit)

multinom <- multinom(clus_3 ~ track.duration_ms + track.popularity + danceability + energy + loudness + 
           speechiness+ acousticness + instrumentalness + liveness + valence + tempo , 
         data = dt_tracks_analysis)

summary(multinom)

z <- summary(multinom)$coefficients/summary(multinom)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p






# DESCRITTORI CLUSTER

table(dt_tracks_analysis[clus_3 == 1]$track.explicit)
table(dt_tracks_analysis[clus_3 == 2]$track.explicit)
table(dt_tracks_analysis[clus_3 == 3]$track.explicit)

table(dt_tracks_analysis[clus_3 == 1]$key)
table(dt_tracks_analysis[clus_3 == 2]$key)
table(dt_tracks_analysis[clus_3 == 3]$key)

table(dt_tracks_analysis[clus_3 == 1]$featuring)
table(dt_tracks_analysis[clus_3 == 2]$featuring)
table(dt_tracks_analysis[clus_3 == 3]$featuring)

table(dt_tracks_analysis[clus_3 == 1]$release_year)
table(dt_tracks_analysis[clus_3 == 2]$release_year)
table(dt_tracks_analysis[clus_3 == 3]$release_year)

nkc <- c("track.artists","track.available_markets" ,"track.href",
         "track.is_local" ,"track.preview_url","track.track_number", "track.type",
         "track.uri","track.album.album_type", "track.album.artists", 
         "track.album.available_markets", "track.album.href",  "track.album.images", "track.album.release_date_precision", 
         "track.album.type", "track.album.uri", "track.album.external_urls.spotify", "track.external_ids.isrc",  
         "track.external_urls.spotify", "type",  "uri", "track_href", "analysis_url", "duration_ms","popularity" )


prova <- dt_tracks_analysis[, .SD, .SDcols = !nkc]

setnames(prova, names(prova), toupper(names(prova)))

skimr::skim_without_charts(prova)
