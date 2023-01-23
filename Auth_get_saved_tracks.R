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

# API AUTHENTICATION AND AUTHORIZATION --------------------------
Sys.setenv(SPOTIFY_CLIENT_ID = 'YOUR CLIENT ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOUR CLIENT SECRET')

access_token <- get_spotify_access_token()


# DATA REQUEST -------------------------------
offset_track_index <- 120L
tracklist = list()
# or pre-allocate for slightly more efficiency
# tracklist = vector("list", length = offset_index)

for (i in 0: offset_track_index){
  
  tracks <- data.frame(get_my_saved_tracks(
    limit = 50,
    offset = (50*i),
    authorization = get_spotify_authorization_code(scope = scopes()),
    include_meta_info = FALSE))
  tracklist[[i +1]] <- tracks
} 

dt_tracks <- rbindlist(tracklist)

full_track_id <- dt_tracks$track.id
# rem_id <- c("0qQBwbbotriOUmAVB8I6Ny","6MbxtmdQYE6eljl9sntLZt", "5oI2y67UPIm1ICN8QKQP2E")
# red_track_id <- full_track_id[!full_track_id %chin% rem_id]

tracks_id <- as.list(full_track_id)
id_n <- 61L
trackfeaturelist <- list()

for (i in 1 : id_n){
  
  audio_features <- get_track_audio_features(
    tracks_id[(((i-1)*100)+1) : (((i-1)*100)+100)], 
    authorization = access_token) 
  trackfeaturelist[[i]] <- audio_features
  
}

# trackfeaturelist <- trackfeaturelist[- length(trackfeaturelist)]
dt_audio_features <- rbindlist(trackfeaturelist)
dt_audio_features <- dt_audio_features[complete.cases(dt_audio_features[, ])]

#skimr::skim_without_charts(dt_audio_features)

dt_audio_features_red <- dt_audio_features[, .SD, .SDcols = c("id", "danceability", "energy", "key", "loudness", "mode", "speechiness", 
                                                              "acousticness", "instrumentalness", "liveness", "valence", "tempo",
                                                              "duration_ms","time_signature" )]
# table(duplicated(dt_audio_features_red$id))
dt_tracks_red <- dt_tracks[, .SD, .SDcols = c( "added_at" , "track.artists", "track.explicit", "track.id",
                                               "track.name", "track.popularity",  "track.album.id", 
                                               "track.album.name",  "track.album.release_date", "track.album.total_tracks" )]
dt_tracks_red[, track.explicit := fifelse(track.id == TRUE, 1, 0)]
dt_tracks_analysis <- merge(x = dt_audio_features_red, y = dt_tracks_red, by.x = "id", by.y = "track.id", all = T)

# num_col <- names(dt_tracks_analysis[,-c("track.name", "id", "track.explicit", "mode", "key", "track.artists" )])


artist_list <- list()

for (i in 1:nrow(dt_tracks_analysis)){
  track_id <- dt_tracks_analysis[i, .SD, .SDcols = "id"]
  artist <- t(unlist(dt_tracks_analysis[i, .SD, .SDcols = "track.artists"]))
  track_artist <- cbind(track_id, artist)
  artist_list[[i]] <- track_artist
}

dt_artists <- rbindlist(artist_list, fill = T)

dt_artists[, track.artists.href := fifelse(is.na(track.artists.href), track.artists.href1, track.artists.href )]
dt_artists[, track.artists.id := fifelse(is.na(track.artists.id), track.artists.id1, track.artists.id )]
dt_artists[, track.artists.name := fifelse(is.na(track.artists.name), track.artists.name1, track.artists.name )]
dt_artists[, track.artists.type := fifelse(is.na(track.artists.type), track.artists.type1, track.artists.type )]
dt_artists[, track.artists.uri := fifelse(is.na(track.artists.uri), track.artists.uri1, track.artists.uri )]
dt_artists[, track.artists.external_urls.spotify := fifelse(is.na(track.artists.external_urls.spotify), track.artists.external_urls.spotify1, track.artists.external_urls.spotify )]

dt_artists[, featuring := fifelse(is.na(track.artists.href1), 0, 1)]
kc_artist <- c("id", "track.artists.href", "track.artists.id","track.artists.name", 
               "track.artists.type", "track.artists.uri", "track.artists.external_urls.spotify", "featuring")

dt_artists <- dt_artists[, .SD, .SDcols = kc_artist]
artists_id <- unique(dt_artists$track.artists.id)
  
offset_artist_index <- 44L
genre_artist_list= list()
# or pre-allocate for slightly more efficiency
# tracklist = vector("list", length = offset_index)

for (i in 0: offset_artist_index){
  
  artist_genre <- get_artists(
    ids = artists_id[(1 + (50*i)) : (50+(i*50))],
    authorization = access_token,
    include_meta_info = FALSE)
  genre_artist_list[[i +1]] <- artist_genre
} 

dt_artist_genre <- rbindlist(genre_artist_list)
dt_artist_genre[, genres := lapply(dt_artist_genre$genres, unlist)]

dt_artist_genre[, main_genre := mapply('[', genres,1,SIMPLIFY = TRUE)] 
kc_genres <- c("id" , "name", "popularity", "followers.total", "main_genre")
dt_artist_genre_final <- dt_artist_genre[, .SD, .SDcols = kc_genres]
dt_artist_genre_final <- dt_artist_genre_final[!is.na(id)]

dt_tracks_analysis <- merge(dt_tracks_analysis, dt_artists, by = "id", all = T)
dt_tracks_analysis <- merge(dt_tracks_analysis, dt_artist_genre_final, by.x = "track.artists.id", by.y = "id", all = T )


setnames(dt_tracks_analysis, "id", "track.id")
dt_tracks_analysis[, name := NULL]

# dt_tracks_analysis <- unnest(dt_tracks_analysis, "main_genre")

setDT(dt_tracks_analysis)


# offset_index_date <- 119L
# 
# track_date_list <- list()
# for (i in 0: offset_index_date){
#   
#   tracks_date <- get_tracks(
#     ids = tracks_id[(1 + (50*i)) : (50+(i*50))],
#     authorization = access_token,
#     include_meta_info = FALSE)
#   track_date_list[[i +1]] <- tracks_date
# } 
# 
# dt_track_date <- rbindlist(track_date_list)

# 
# track_audio_analysis_list <- list()
# 
# for (i in 1 : length(tracks_id)){
#   
#   audio_analysis <- get_track_audio_analysis(tracks_id[[i]], access_token)
#   
#   track_audio_analysis_list[[i]] <- audio_analysis
#   
# }




# PRELIMINARY ANALYSIS: CLEANIN + VIZ  ----------------------------

setkey(x = dt_tracks_analysis, ... = track.id)
skimr::skim_without_charts(dt_tracks_analysis)
dt_tracks_analysis_final <- na.omit(dt_tracks_analysis)

skimr::skim_without_charts(dt_tracks_analysis_final)


contingency_genres <- setDT(as.data.frame( table(dt_tracks_analysis$main_genre)))
setnames(contingency_genres, "Var1", "genre")
contingency_genres <- clean_names(contingency_genres)
contingency_genres[, perc := (freq/sum(freq)*100)]
contingency_genres <- contingency_genres[order(-rank(perc))]
contingency_genres[, cum_perc := cumsum(perc)]

contingency_genres %>%
  hchart('column', hcaes(x = genre, y = perc))




contingency_key <- setDT(as.data.frame( table(dt_tracks_analysis$key)))

setnames(contingency_key, "Var1", "key")
contingency_key <- clean_names(contingency_key)
contingency_key[, perc := (freq/sum(freq)*100)]
contingency_key <- contingency_key[order(-rank(perc))]

contingency_key[, cum_perc := cumsum(perc)]

contingency_key %>%
  hchart('column', hcaes(x = key, y = perc))




hist(dt_tracks_analysis$danceability, col='red')
hchart(
  density(dt_tracks_analysis$danceability), 
  type = "area", color = "#B71C1C", name = "Danceability"
)
hchart(
  dt_tracks_analysis$danceability, 
  color = "#B71C1C", name = "Danceability"
)

hist(dt_tracks_analysis$energy, col='green', )
hist(dt_tracks_analysis$loudness, col='blue')

hchart(
  dt_tracks_analysis$loudness, 
  color = "#B71C1C", name = "Loudness"
)

R <- cor(dt_tracks_analysis[, .SD, .SDcols = num_col])

corrplot(R, method="color")

dt_tracks_analysis %>% 
  hchart('scatter', hcaes(x = energy, y = loudness))

# PROVA PCA -------------------------

prova_pca  <- prcomp(x = dt_tracks_analysis[, c("danceability", "energy", "loudness", "speechiness", "acousticness","instrumentalness",
                                                "liveness", "track.duration_ms" )], scale = T)

prova_pca_2  <- princomp(dt_tracks_analysis[, c("danceability", "energy", "loudness", "speechiness", "acousticness","instrumentalness",
                                                "liveness")], cor = T)


# PROVA CLUSTER -------------------------

wss <- (nrow(dt_tracks_analysis[, .SD, .SDcols = num_col])-1)*sum(apply(dt_tracks_analysis[, .SD, .SDcols = num_col],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dt_tracks_analysis[, .SD, .SDcols = num_col],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

prova_model <- kmeans(x = dt_tracks_analysis[, .SD, .SDcols = num_col],  centers = 2)


fviz_cluster(prova_model, data = dt_tracks_analysis[, .SD, .SDcols = num_col],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)





