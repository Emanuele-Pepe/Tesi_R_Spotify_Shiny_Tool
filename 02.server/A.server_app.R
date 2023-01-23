

### SERVER ###


server_app = function(input, output, session) {
  
  # AUTHORIZATION ------------------------
  
  Sys.setenv(SPOTIFY_CLIENT_ID = '')
  Sys.setenv(SPOTIFY_CLIENT_SECRET = '')
  
  access_token <- get_spotify_access_token()
  
  # CARICAMENTO DATI
  
  ## GET TRACKS --------------------------------------
  
  dt_tracks <-eventReactive(input$getTracks, {
    
    withProgress(message = 'Retrieving Tracks', min = 0, max = 100, {
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
      return(dt_tracks)
      
    })
  })
  
  
  
  output$table <- renderReactable({
    dt_tracks()[, .SD, .SDcols = c("track.id", "track.name", "track.album.id", "track.album.release_date")] |>
      reactable(
        columns = list(
          track.id = colDef(name = "TRACK.ID", minWidth = 100, align = 'left', style = list(fontWeight = "bold")),
          track.name = colDef(name = "TRACK NAME"),
          track.album.id = colDef(name = "TRACK ALBUM ID"),
          track.album.release_date = colDef(name = "TRACK ALBUM RELEASE DATE")),
        bordered = TRUE, highlight = TRUE, sortable = TRUE, fullWidth = TRUE,
        theme = reactableTheme(color = "#486966", tableStyle = list(fontSize = 12),
                               headerStyle = list(height = "3em", background = "#649FA1", borderColor = "#8FBCBB", color = '#fff', cursor = "pointer", fontSize = 14),
                               footerStyle = list(height = "2.25em")),
        
        defaultColDef = colDef(
          format = colFormat(separators = TRUE),
          align = "right",
          minWidth = 100),
        
        
            
        )
    
  })
  
  ## GET AUDIO FEATURES ------------------------------------------------------
  
  dt_audio_features <- eventReactive(input$getAudioFeatures, {
    
    withProgress(message = 'Retrieving Audio Features', min = 0, max = 100, {
    
      tracks_id <- dt_tracks()$track.id
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
      
      
      return(dt_audio_features)
      
    })
    
  })
  
  output$audiofeatures <- renderReactable({
    
    dt_audio_features()[, .SD, .SDcols = c("track.id", "danceability", "energy", "key","loudness","mode","speechiness","acousticness","instrumentalness","liveness","valence","tempo" ,"type", "duration_ms","time_signature")] |>
      reactable(
        columns = list(
          track.id = colDef(name = "TRACK.ID", minWidth = 100, align = 'left', style = list(fontWeight = "bold")),
          danceability = colDef(name = "DANCEABILITY"),
          energy = colDef(name = "ENERGY"),
          key = colDef(name = "KEY"),
          loudness = colDef(name = "LOUDNESS"),
          mode = colDef(name = "MODE"),
          speechiness = colDef(name = "SPEECHINESS"),
          acousticness = colDef(name = "ACOUSTICNESS"),
          instrumentalness = colDef(name = "INSTRUMENTALNESS"),
          liveness = colDef(name = "LIVENESS"),
          valence = colDef(name = "VALENCE"),
          tempo = colDef(name = " TEMPO"),
          type = colDef(name = "TYPE"),
          duration_ms = colDef(name = "DURATION MS"),
          time_signature = colDef(name = "TIME SIGNATURE")),
        bordered = TRUE, highlight = TRUE, sortable = TRUE, fullWidth = TRUE,
        theme = reactableTheme(color = "#486966", tableStyle = list(fontSize = 12),
                               headerStyle = list(height = "3em", background = "#649FA1", borderColor = "#8FBCBB", color = '#fff', cursor = "pointer", fontSize = 14),
                               footerStyle = list(height = "2.25em")),
        
        defaultColDef = colDef(
          format = colFormat(separators = TRUE),
          align = "right",
          minWidth = 100),
        
       
      )
  })

  ## GET ARTISTS -------------------------------------------------------
  
  dt_genre_final <- eventReactive(input$getArtist, {
    withProgress(message = "Retrieving Artists' Details " , min = 0, max = 100, {
    
      artist_list <- list()
      
      for (i in 1:nrow(dt_tracks())){
        track_id <- dt_tracks()[i, .SD, .SDcols = "track.id"]
        artist <- t(unlist(dt_tracks()[i, .SD, .SDcols = "track.artists"]))
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
      
      return(list(dt_artist_genre_final = dt_artist_genre_final, dt_artists = dt_artists)) 
    
    })
    
  })
  
  
  output$genre <- renderReactable({
    dt_genre_final()[["dt_artist_genre_final"]]|>
      reactable(
        columns = list(
          mainArtist.id = colDef(name = "MAIN ARTIST ID", minWidth = 100, align = 'left', style = list(fontWeight = "bold")),
          mainArtist.name = colDef(name = " MAIN ARTIST NAME"),
          popularity = colDef(name = "POPULARITY"),
          followers.total = colDef(name = "FOLLOWERS TOTAL"),
          main_genre = colDef(name = "MAIN GENRE")),
    
    
        bordered = TRUE, highlight = TRUE, sortable = TRUE, fullWidth = TRUE,
        theme = reactableTheme(color = "#486966", tableStyle = list(fontSize = 12),
                               headerStyle = list(height = "3em", background = "#649FA1", borderColor = "#8FBCBB", color = '#fff', cursor = "pointer", fontSize = 14),
                               footerStyle = list(height = "2.25em")),
        
        defaultColDef = colDef(
          format = colFormat(separators = TRUE),
          align = "right",
          minWidth = 100),
        
       
          
      )
  })
  
  ## MERGE --------------------------------------
  
  dt_tracks_analysis <- eventReactive(input$merge, {
    withProgress(message = "Retrieving Artists' Details " , min = 0, max = 100, {
      dt_tracks_analysis <- merge(dt_tracks(), dt_audio_features(), by = "track.id")
      dt_tracks_analysis <- merge(dt_tracks_analysis, dt_genre_final()[["dt_artists"]], by = "track.id")
      dt_tracks_analysis <- merge(dt_tracks_analysis, dt_genre_final()[["dt_artist_genre_final"]], by.x = "track.mainArtist.id", by.y = "mainArtist.id")
      setkey(x = dt_tracks_analysis, ... = track.id)
      return(dt_tracks_analysis)
    })
  })
  
  
  output$merge_table <- renderReactable({
    dt_tracks_analysis()[, .SD, .SDcols = c("track.id","track.name", "track.album.id", "track.album.release_date", 
                                            "danceability", "energy", "key","loudness","mode","speechiness","acousticness","instrumentalness","liveness","valence","tempo" ,"type", "duration_ms","time_signature", 
                                            "track.mainArtist.id",  "mainArtist.name","popularity", "followers.total","main_genre" )] |>
      setnames(old =c("track.id","track.name", "track.album.id", "track.album.release_date", 
                      "danceability", "energy", "key","loudness","mode","speechiness","acousticness","instrumentalness","liveness","valence","tempo" ,"type", "duration_ms","time_signature", 
                      "track.mainArtist.id",  "mainArtist.name","popularity", "followers.total","main_genre" ),
               new = c("TRACK.ID","TRACK NAME", "TRACK ALBUM ID", "TRACK ALBUM RELEASE DATE,", 
                       "DANCEABILITY", "ENERGY", "KEY","LOUDNESS","MODE","SPEECHINESS","ACOUSTICNESS","IMSTRUMENTALNESS","LIVENESS","VALENCE","TEMPO" ,"TYPE", "DURATION MS","TIME SIGNATURE", 
                       "MAIN ARTIST ID",  "MAIN ARTIST NAME","POPULARITY", "FOLLOWERS TOTAL","MAIN GENRE" )) |>
                 reactable(
                   
                   bordered = TRUE, highlight = TRUE, sortable = TRUE, fullWidth = TRUE,
                   theme = reactableTheme(color = "#486966", tableStyle = list(fontSize = 12),
                                          headerStyle = list(height = "3em", background = "#649FA1", borderColor = "#8FBCBB", color = '#fff', cursor = "pointer", fontSize = 14),
                                          footerStyle = list(height = "2.25em")),
                   
                   defaultColDef = colDef(
                     format = colFormat(separators = TRUE),
                     align = "right",
                     minWidth = 100),
                   
                   columns = list(
                     TRACK.ID= colDef(name = "TRACK.ID", minWidth = 100, align = 'left', style = list(fontWeight = "bold")))
                 )
  })
  
  ## CLEAN ------------------------------
  
  dt_tracks_analysis_finale <- eventReactive(input$clean, {
    withProgress(message = "Cleaning Data", min = 0, max = 100, {
      
      nkc <- c("track.artists","track.available_markets" ,"track.href","track.is_local","track.preview_url","track.track_number", 
               "track.type", "track.uri","track.album.album_type", "track.album.artists", 
               "track.album.available_markets", "track.album.href",  "track.album.images", "track.album.release_date_precision", 
               "track.album.type", "track.album.uri", "track.album.external_urls.spotify", "track.external_ids.isrc", 
               "track.external_urls.spotify", "type",  "uri", "track_href", "analysis_url", "duration_ms", "popularity" )
      
      dt_tracks_analysis_finale <- dt_tracks_analysis()[!is.na(main_genre)]
      dt_tracks_analysis_finale <- dt_tracks_analysis_finale[, .SD, .SDcols = !nkc]
      dt_tracks_analysis_finale[, featuring := as.factor(featuring)]
      dt_tracks_analysis_finale[, track.explicit := as.factor(as.numeric(track.explicit))]
      dt_tracks_analysis_finale[, release_year := as.numeric(substr(track.album.release_date, start = 1, stop = 4))]
      #dt_tracks_analysis_finale[, release_year := as.factor(as.numeric(substr(track.album.release_date, start = 1, stop = 4)))]
      dt_tracks_analysis_finale[, key := as.factor(key)]
      dt_tracks_analysis_finale[, mode := as.factor(mode)]
      dt_tracks_analysis_finale[, time_signature := as.factor(time_signature)]
      dt_tracks_analysis_finale[, track.disc_number := as.factor(track.disc_number)]
      
      setnames(x = dt_tracks_analysis_finale, old = names(dt_tracks_analysis_finale), new = toupper(names(dt_tracks_analysis_finale)))

      return(dt_tracks_analysis_finale)
    })
  })
  
  output$clean_table <- renderReactable({
    dt_tracks_analysis_finale()|>
    reactable(bordered = TRUE, highlight = TRUE, sortable = TRUE, fullWidth = TRUE,
              theme = reactableTheme(color = "#486966", tableStyle = list(fontSize = 12),
                                     headerStyle = list(height = "3em", background = "#649FA1", borderColor = "#8FBCBB", color = '#fff', cursor = "pointer", fontSize = 14),
                                     footerStyle = list(height = "2.25em")),
              
              defaultColDef = colDef(
                format = colFormat(separators = TRUE),
                align = "right",
                minWidth = 100)
              )
  })
  
  # DESCRIPTIVE STATISTICS --------------------------------
  
  ## QUANTITATIVE VAR ---------------------------------------
  
  observe({
    
    x = names(dt_tracks_analysis_finale()[, .SD, .SDcols = is.numeric])
    
    updateSelectInput(
      session = session,
      inputId = 'var_selector_quant_descr',
      choices = x,
      selected = NULL
    )     
    
  })
  
  output$distr_var_quant_hist <- renderPlot({
    
    data = as.data.frame( dt_tracks_analysis_finale()[, .SD, .SDcols = input$var_selector_quant_descr])
    data|>
      ggplot(aes(x = .data[[input$var_selector_quant_descr]])) +
      geom_histogram()+
      labs(
        title = "Density estimation plot",
        subtitle = "Quantitative variables' distribution",
        caption = "Source: ImaginaryCo"
      ) +
      theme_bw()+
      theme(
        plot.title = element_text(color = "#0099f9", size = 20),
        plot.subtitle = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")
      )
      
  })
  
  
  observe({
    
    x = names(dt_tracks_analysis_finale()[, .SD, .SDcols = is.numeric])
    
    updateSelectInput(
      session = session,
      inputId = 'var_selector_scatt_x',
      choices = x,
      selected = NULL
    )     
    
  })
  
  observe({
    
    x = names(dt_tracks_analysis_finale()[, .SD, .SDcols = is.numeric])
    
    updateSelectInput(
      session = session,
      inputId = 'var_selector_scatt_y',
      choices = x,
      selected = NULL
    )     
    
  })
  
  output$scatt_descr_plot <- renderPlot({
    
    data = as.data.frame(dt_tracks_analysis_finale())
    data|>
      ggplot(aes(x = .data[[input$var_selector_scatt_x]], y = .data[[input$var_selector_scatt_y]])) +
      geom_point()+
      labs(
        title = "Scatter plot",
        subtitle = "Quantitative variables scatterplot",
        caption = "Source: ImaginaryCo"
      ) +
      theme_bw()+
      theme(
        plot.title = element_text(color = "#0099f9", size = 20),
        plot.subtitle = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")
      )
    
  })
  
  ## CATEGORICAL VAR -----------------------
  
  observe({
    
    x = names(dt_tracks_analysis_finale()[, .SD, .SDcols = is.factor])
    
    updateSelectInput(
      session = session,
      inputId = 'var_selector_cat_descr',
      choices = x,
      selected = NULL
    )     
    
  })
  
  output$cat_bar_plot <- renderPlot({
    
    data = as.data.frame(dt_tracks_analysis_finale())
    data|>
      ggplot(aes(x = .data[[input$var_selector_cat_descr]], fill = .data[[input$var_selector_cat_descr]])) +
      geom_bar()+
      labs(
        title = "Bar chart",
        subtitle = "Categorical/Factor Variables",
        caption = "Source: ImaginaryCo"
      ) +
      theme_bw()+
      theme(
        plot.title = element_text(color = "#0099f9", size = 20),
        plot.subtitle = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")
      )
    
  })
    
  
  
  # DATA ANALYSIS ------------------
  
  ## CLUSTER ANALYSIS ----------------
  observe({
    
    x = names(dt_tracks_analysis_finale()[, .SD, .SDcols = is.numeric])
    
    updateSelectInput(
      session = session,
      inputId = 'var_selector_clus',
      choices = x
    )     
    
  })
 
  db_cluster <- reactive({ 
    db_cluster = dt_tracks_analysis_finale()[, .SD, .SDcols = input$var_selector_clus] 
    return(db_cluster)
  })
  
  output$elbow_cluster <- renderPlot({
    
    validate(
      need(input$var_selector_clus != "", "Please select a variable")
    )
    
    wss <- function(k) {
      kmeans(scale(as.matrix(db_cluster()), center = T, scale = T), k, nstart = 10 )$tot.withinss
    }
    
    k.values <- 1:15
    wss_values <- map_dbl(k.values, wss)
    df_k = data.table(k.values = k.values, wss_values = wss_values)
    
    theme_set(theme_bw())
    
      ggplot(df_k, aes(x = k.values, y = wss_values)) +
      geom_line(color = '#A3BFD9', size = 1) +
      geom_point(color = '#164773', size = 3) +
      ggtitle("Within Sum Squares comparison between k-means clustering") +
      xlab('# of centroids') + 
      ylab('Within Sum of Squares')
    
  })
  
  output$scatter_cluster <- renderPlot({
    
    validate(
      need(input$cluster_num_selector != 0, "Please select the number of clusters")
    )
    validate(
      need(input$var_selector_clus != "", "Please select at least two variables for cluster analysis")
    )
    validate(
      need(length(input$var_selector_clus) >= 2, "Please increase the number of variables")
    )
    set.seed(313)
    model_km <- kmeans(scale(as.matrix(db_cluster()), center = T, scale = T), centers = input$cluster_num_selector)
    fviz_cluster(model_km, data = scale(as.matrix(db_cluster()),center = T, scale = T),
                 palette = c("#2E9FDF", "#00AFBB", "#E7B800","#ffa535", 
                             "#f9ff97", "#ffebad", "#871282", "#2b2d42", 
                             "#8d99ae", "#edf2f4"), 
                 geom = "point",
                 ellipse.type = "convex", 
                 ggtheme = theme_bw()
    )
    
  })
  
  ## PCA ----------------------------------------
  
  observe({
    
    x = names(dt_tracks_analysis_finale()[, .SD, .SDcols = is.numeric])
    
    updateSelectInput(
      session = session,
      inputId = 'var_selector_pca',
      choices = x
    )     
    
  })
  

  db_pca <- reactive ({
    db_pca <- dt_tracks_analysis_finale()[, .SD, .SDcols = input$var_selector_pca]
    return(db_pca) 
    })
  
  output$summary_pca <- renderPrint({
    
    validate(
      need(input$var_selector_pca != "", "Please select a variable")
    )
    pca <- prcomp(x = db_pca(), scale = T, center = T)
    summary(pca)
  })
  
  output$pca_cum_var <- renderPlot({
    validate(
      need(input$var_selector_pca != "", "No variable selected")
    )
    
    pca <- prcomp(x = db_pca(), scale = T, center = T)
    fviz_eig(
      pca, choice = "variance", geom = c("bar", "line"), linecolor = "#00203FFF", 
      barcolor = "#ADEFD1FF", barfill = "#ADEFD1FF")+
      labs(
        title = "Variance scree plot",
        subtitle = "Principal component analysis",
        caption = "Source: ImaginaryCo"
      ) +
      theme(
        plot.title = element_text(color = "#0099f9", size = 20),
        plot.subtitle = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")
      )
    
  })
  
  output$loadings_pca <- renderPrint({
    
    validate(
      need(input$var_selector_pca != "", "")
    )
    pca <- prcomp(x = db_pca(), scale = T, center = T)
    print(pca)
  })
  
  
  
  
  
  
  
  

# X. END =============================================================================
      
      session$onSessionEnded(function() {
          stopApp()
      })      
 
}    





