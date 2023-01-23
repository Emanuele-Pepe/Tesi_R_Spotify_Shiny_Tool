

### UI WRAPPER ###





# : ============================================================================================================================



ui_app <-


suppressWarnings(
    
# Header ================================================================================
    
navbarPage(
  
  title = div(div(class = "topimg_left", img(src = "spotify-logo-png-7059.png", width = '100%', height = '40px')), div(class = "topimg_right", img(src = "spotify-logo-png-7053.png", width = '100%', height = '40px'))),
  windowTitle = 'Dashboard Analitica Spotify',
  theme = default_th,
  header = header_def,
  footer = NULL,
  
  
  
      

    

# 0. Info =============================================================================
     
     tabPanel(

          title = 'Caricamento Dati',

          t_info

     ),

# A. Input =============================================================================

      navbarMenu(title = "Descriptive Statistics",
        tabPanel(title = "Quantitative variables",
                 t_descr_quant
                 ),
        tabPanel(title = "Categorical Variables",
                 t_descr_cat
                 )),

      tabPanel(
        
        title = 'Analisi Dei Dati',
        
        t_audio_analysis 
        
      ),


    setSliderColor(rep('#A1C7E0', 20), 1:20)


))
