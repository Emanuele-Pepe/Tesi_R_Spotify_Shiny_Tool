

### TAB PANELS ###




# : ---------



# 0. Info =============================================================================

t_info =
    
    fluidPage(
        
        br(), br(), br(), br(),
        
        uiOutput(outputId = 'Introduzione')
        
    )


# A. Input =============================================================================


t_descrizione_user= 
      
  fluidPage(style = "padding: 0.0rem",
    
    tabsetPanel(
      
      tabPanel(
        title = "Genere",
        fluidRow()
      ),
      tabPanel(
        title = "Data Inserimento Libreria",
        fluidRow()
      ),
      tabPanel(
        title = "Periodo Storico",
        fluidRow()
      ),
      tabPanel(
        title = "Popolarità",
        fluidRow()
      )
    )
  )
 

t_audio_analysis = 
  
  fluidPage()

t_insights = 
  
  fluidPage()

t_sviluppi_futuri = 
  
  fluidPage()


