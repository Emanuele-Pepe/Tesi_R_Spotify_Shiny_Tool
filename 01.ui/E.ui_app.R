

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

          title = 'Introduzione',

          t_info

     ),

# A. Input =============================================================================


tabPanel(

          title = 'Descizione Utente',

          t_descrizione_user
     
          ),

tabPanel(
  
  title = 'Audio Analysis',
  
  t_audio_analysis 
  
),

tabPanel(
  
  title = 'Insights',
  
  t_insights
  
),

tabPanel(
  
  title = 'Sviluppi Futuri',
  
  t_sviluppi_futuri
  
),


     
#      
# # 2. Model =============================================================================
#      
#      nav(
# 
#           title = 'Profilazione Familiare',
# 
#           t_run
# 
#      ),
# 
# 
# # 3. Simulation =============================================================================
#      
#      nav(
# 
#           title = 'Simulazione e Pianificazione',
# 
#           t_sim
# 
#      ),
# 
# 
# # 3. Simulation =============================================================================
#      
#      nav(
# 
#           title = 'Erogazione',
# 
#           'place'
# 
#      ),

# : ============================================================================================================================

    # footer = tagList(
    # 
    #   tags$head(
    #     
    #     tags$style("
    #     
    #      .fluid_column{ border-style: solid;
    #                     border-color: rgba(0,0,0,0.1);
    #                     border-width: thin; }"
    #                )), 
    #   
    #   tags$head(
    #     
    #     tags$style("
    #     
    #      .fixed_column{ min-height: 350px;
    #                     border-radius: 0.25rem;
    #                     border-style: solid;
    #                     border-color: rgba(0,0,0,0.1);
    #                     border-width: thin; 
    #                     margin-bottom:10px;}"
    #     )),
    #   
    #   
    #    tags$style(".topimg {
    #                         margin-left:5px;
    #                         margin-right: -15px;
    #                         margin-top: -21px;
    #                         margin-bottom: -60px;
    #                       }"),       
    #   
    #    tags$style(".hidden-column-headers .rt-thead {
    #                               position: absolute;
    #                               width: 1px;
    #                               height: 1px;
    #                               padding: 0;
    #                               margin: -1px;
    #                               overflow: hidden;
    #                               clip: rect(0, 0, 0, 0);
    #                               border: 0;
    #                             }")
    # 
    # ),

    setSliderColor(rep('#A1C7E0', 20), 1:20)


))
