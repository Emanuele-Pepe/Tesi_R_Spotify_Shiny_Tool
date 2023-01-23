

### TAB PANELS ###




# : ---------



# 0. Info =============================================================================

t_info =
    
    fluidPage(
      
      fluidRow(
        column(width = 12,
          fluidRow(
            column(12,
              h4(HTML("<b>Manuale Istruzioni</b>"), style = 'color: #4e7584;'), hr(style = "height:2px;border-width:0;color:white;background-color:#617A72"))
          ),
          fluidRow(
            column(12,
                   "Placeholder Link Istruzioni")),
        )
      ),
      
      br(),
      br(),
      
      fluidRow(
      
        column(width = 2,
           fluidRow(
             
             column(width = 12,
                    
                    h4(HTML("<b>Console API</b>"), style = 'color: #4e7584;'), hr(style = "height:2px;border-width:0;color:white;background-color:#617A72")),
             
           ),
           
           fluidRow(
             
             column(width = 12,
                    
            
               actionButton("getTracks", "Get Tracks"),
               
               br(),
               br(),
               
               actionButton("getAudioFeatures", "Get Audio Features"),
               
               br(),
               br(),
               
               actionButton("getArtist", "Get Artist"),
               
               br(),
               br(),
               
           fluidRow(
             
             column(width = 12,
                    
                    h4(HTML("<b>Organizzazione Dati</b>"), style = 'color: #4e7584;'), hr(style = "height:2px;border-width:0;color:white;background-color:#617A72")),
             
           ),
               
               actionButton("merge", "Merge Data"),
               
               br(),
               br(),
               
               actionButton("clean", "Clean Data")
             )
            )
               
               
        ),
        
        column( width = 10,
                
          fluidRow(
            
            column(width = 12,
                   
                   h4(HTML("<b>Test Download Dati</b>"), style = 'color: #4e7584;'), hr(style = "height:2px;border-width:0;color:white;background-color:#617A72")),
            
          ),
          
          fluidRow(
            
            column(width = 12,
                   
                   h5(HTML("<b>Tabella Get Tracks</b>"), style = 'color: #617A72;'), hr(style = "height:2px;border-width:0;color:white;background-color:#617A72")),
            
          ),
        
          fluidRow(
            column( width = 12, reactableOutput("table"))
            ),
          
          fluidRow(
            
            column(width = 12,
                   
                   h5(HTML("<b>Tabella Get Audio Features</b>"), style = 'color: #617A72;'), hr(style = "height:2px;border-width:0;color:white;background-color:#617A72")),
            
          ),
          
          fluidRow(
            column( width = 12, reactableOutput("audiofeatures"))
          ),
          
          fluidRow(
            
            column(width = 12,
                   
                   h5(HTML("<b>Tabella Get Artist</b>"), style = 'color: #617A72;'), hr(style = "height:2px;border-width:0;color:white;background-color:#617A72")),
            
          ),
          
          fluidRow(
            column( width = 12, reactableOutput("genre"))
          ),
          
          
          fluidRow(
            
            column(width = 12,
                   
                   h5(HTML("<b>Tabella Merge Dati</b>"), style = 'color: #617A72;'), hr(style = "height:2px;border-width:0;color:white;background-color:#617A72")),
            
          ),
          
          fluidRow(
            column( width = 12, reactableOutput("merge_table"))
          ),
          
          fluidRow(
            
            column(width = 12,
                   
                   h5(HTML("<b>Tabella Pulizia Dati</b>"), style = 'color: #617A72;'), hr(style = "height:2px;border-width:0;color:white;background-color:#617A72")),
            
          ),
          
          fluidRow(
            column( width = 12, reactableOutput("clean_table"))
          ),
        
        
        
      
      )
    
    )
        
    )


# DESCRIPTIVE STATISTICS =============================================================================

t_descr_quant = 
  
  fluidPage(
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          var_selector_quant_descr
        ),
        mainPanel(
          
          plotOutput("distr_var_quant_hist")
          
        )
        
      )
    ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          var_selector_scatt_x,
          var_selector_scatt_y
        ),
        mainPanel(
          
          plotOutput("scatt_descr_plot")
          
        )
        
      )
    )
    
    
  )
  
  
  

t_descr_cat = fluidPage(
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        var_selector_cat_descr
      ),
      mainPanel(
        
        plotOutput("cat_bar_plot")
        
      )
    )
  )
)
 



# DATA ANALYSIS ============================================

t_audio_analysis = 
  
  fluidPage(
    fluidRow(
      column(width = 4,    
        var_selector_clus,     
        plotOutput("elbow_cluster"),
        
  
        ),
      column(width = 8,
        cluster_num_selector,
        plotOutput("scatter_cluster", height = "100%")
        )
      ),
    
    br(),
    
    fluidRow(
      column(width = 4,
        var_selector_pca,
        verbatimTextOutput(outputId = "summary_pca"),
        br(),
        verbatimTextOutput(outputId = "loadings_pca")  
             ),
      column(width = 8,
             plotOutput("pca_cum_var")
             )
    )
  )








