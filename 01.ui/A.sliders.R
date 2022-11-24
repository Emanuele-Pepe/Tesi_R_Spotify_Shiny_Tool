



### SLIDERS ###


side_bar_width = '100%'


# : ---------




# A. Input =============================================================================


s_input_selector_uni =
  
  selectInput(
    
    inputId = 's_input_selector_uni',
    label = 'Univariate Variable to Plot',
    choices = NULL,
    width = side_bar_width
    
  )


s_input_selector_uni_bg =
  
  selectInput(
    
    inputId = 's_input_selector_uni_bg',
    label = 'Select Variable',
    choices = NULL,
    width = side_bar_width
    
  )

s_input_selector_uni_bg_g =
  
  selectInput(
    
    inputId = 's_input_selector_uni_bg_g',
    label = 'Grouping Variable',
    choices = NULL,
    width = side_bar_width
    
  )



# B. Run =============================================================================

s_run_reload =
  
  actionButton(
    
    inputId = 's_run_launchazienda',
    label = 'Launch Prediction',
    icon = shiny::icon('person-running'),
    width = side_bar_width,
    class = "btn-success"
    
  )




# C. Sim =============================================================================

## Family --------------------

s_sim_fam_w =
  
  selectInput(
    
    inputId = 's_sim_fam_w',
    label = 'Seleziona Tipologia familiare',
    choices = list_fam,
    multiple = TRUE,
    selected = '05. All',
    width = side_bar_width
    
  )

s_sim_fam_mw =
  
  selectInput(
    
    inputId = 's_sim_fam_mw',
    label = 'Seleziona Tipologia familiare',
    choices = list_fam,
    multiple = TRUE,
    selected = c('02. Monoreddito', '04. Famiglia'),
    width = side_bar_width
    
  )

s_sim_fam_m =
  
  selectInput(
    
    inputId = 's_sim_fam_m',
    label = 'Seleziona Tipologia familiare',
    choices = list_fam,
    multiple = TRUE,
    selected = c('04. Famiglia'),
    width = side_bar_width
    
  )

s_sim_fam_a =
  
  selectInput(
    
    inputId = 's_sim_fam_a',
    label = 'Seleziona Tipologia familiare',
    choices = list_fam,
    multiple = TRUE,
    selected = c('00. None'),
    width = side_bar_width
    
  )


## Figli --------------------

s_sim_figli_w =
  
  selectInput(
    
    inputId = 's_sim_figli_w',
    label = 'Figli Minori di 3 Anni',
    choices = list_si_no,
    multiple = TRUE,
    selected = c('1. No', '2. Si'),
    width = side_bar_width
    
  )

s_sim_figli_mw =
  
  selectInput(
    
    inputId = 's_sim_figli_mw',
    label = 'Figli Minori di 3 Anni',
    choices = list_si_no,
    multiple = TRUE,
    selected = c('2. Si'),
    width = side_bar_width
    
  )

s_sim_figli_m =
  
  selectInput(
    
    inputId = 's_sim_figli_m',
    label = 'Figli Minori di 3 Anni',
    choices = list_si_no,
    multiple = TRUE,
    selected = c('0. None'),
    width = side_bar_width
    
  )

s_sim_figli_a =
  
  selectInput(
    
    inputId = 's_sim_figli_a',
    label = 'Figli Minori di 3 Anni',
    choices = list_si_no,
    multiple = TRUE,
    selected = c('0. None'),
    width = side_bar_width
    
  )


## Anziani --------------------

s_sim_anziani_w =
  
  selectInput(
    
    inputId = 's_sim_anziani_w',
    label = 'Spese per Anziani o Altri familiari a carico',
    choices = list_si_no,
    multiple = TRUE,
    selected = c('1. No', '2. Si'),
    width = side_bar_width
    
  )

s_sim_anziani_mw =
  
  selectInput(
    
    inputId = 's_sim_anziani_mw',
    label = 'Spese per Anziani o Altri familiari a carico',
    choices = list_si_no,
    multiple = TRUE,
    selected = c('2. Si'),
    width = side_bar_width
    
  )

s_sim_anziani_m =
  
  selectInput(
    
    inputId = 's_sim_anziani_m',
    label = 'Spese per Anziani o Altri familiari a carico',
    choices = list_si_no,
    multiple = TRUE,
    selected = c('2. Si'),
    width = side_bar_width
    
  )

s_sim_anziani_a =
  
  selectInput(
    
    inputId = 's_sim_anziani_a',
    label = 'Spese per Anziani o Altri familiari a carico',
    choices = list_si_no,
    multiple = TRUE,
    selected = c('0. None'),
    width = side_bar_width
    
  )


## Auto --------------------

s_sim_auto_w =
  
  selectInput(
    
    inputId = 's_sim_auto_w',
    label = "Utilizzo dell'Auto + Distanze percose elevate",
    choices = list_si_no,
    multiple = TRUE,
    selected = c('1. No', '2. Si'),
    width = side_bar_width
    
  )

s_sim_auto_mw =
  
  selectInput(
    
    inputId = 's_sim_auto_mw',
    label = "Utilizzo dell'Auto + Distanze percose elevate",
    choices = list_si_no,
    multiple = TRUE,
    selected = c('2. Si'),
    width = side_bar_width
    
  )

s_sim_auto_m =
  
  selectInput(
    
    inputId = 's_sim_auto_m',
    label = "Utilizzo dell'Auto + Distanze percose elevate",
    choices = list_si_no,
    multiple = TRUE,
    selected = c('0. None'),
    width = side_bar_width
    
  )

s_sim_auto_a =
  
  selectInput(
    
    inputId = 's_sim_auto_a',
    label = "Utilizzo dell'Auto + Distanze percose elevate",
    choices = list_si_no,
    multiple = TRUE,
    selected = c('0. None'),
    width = side_bar_width
    
  )


## Launch Sim -------------------------

s_sim_composizione_itavsazienda =
  
  selectInput(
    
    inputId = 's_sim_composizione_itavsazienda',
    label = 'Seleziona Distribuzione',
    choices = c('Italia', "Azienda"),
    selected = 'Italia',
    width = side_bar_width
    
  )


s_sim_composizione_standard =
  
  selectInput(
    
    inputId = 's_sim_composizione_standard',
    label = 'Seleziona Fascia di Rischio',
    choices = c('Ridotta', 'Media', 'Ampia'),
    selected = 'Media',
    width = side_bar_width
    
  )


s_sim_scenario_ipotesi =
  
  selectInput(
    
    inputId = 's_sim_scenario_ipotesi',
    label = 'Seleziona Scenario Macroeconomico',
    choices = c('Base', 'Medio', 'Stress'),
    selected = 'Medio',
    width = side_bar_width
    
  )

# D. Ipotesi Investimento ----------

s_sim_ipotesi_base =
  
  sliderInput(
    
    inputId = 's_sim_ipotesi_base',
    label = h6('Base: Senza Fattori Aggravanti', style = 'text-align: center;'),
    min = 0,
    max = 1,
    value = 0.5,
    step = 0.1,
    width = side_bar_width
    
  )

s_sim_ipotesi_figli =
  
  sliderInput(
    
    inputId = 's_sim_ipotesi_figli',
    label = h6('Figli minori 3 anni', style = 'text-align: center;'),
    min = 0,
    max = 1,
    value = 1,
    step = 0.1,
    width = side_bar_width
    
  )

s_sim_ipotesi_anziani =
  
  sliderInput(
    
    inputId = 's_sim_ipotesi_anziani',
    label = h6('Spese per Anziani', style = 'text-align: center;'),
    min = 0,
    max = 1,
    value = 0.5,
    step = 0.1,
    width = side_bar_width
    
  )

s_sim_ipotesi_auto =
  
  sliderInput(
    
    inputId = 's_sim_ipotesi_auto',
    label = h6('Utilizzo Auto per Lunghe Distanze', style = 'text-align: center;'),
    min = 0,
    max = 1,
    value = 1,
    step = 0.1,
    width = side_bar_width
    
  )

s_sim_ipotesi_singolo =
  
  sliderInput(
    
    inputId = 's_sim_ipotesi_singolo',
    label = h6('Singolo', style = 'text-align: center;'),
    min = 0,
    max = 1,
    value = 1,
    step = 0.1,
    width = side_bar_width
    
  )


s_sim_ipotesi_mono =
  
  sliderInput(
    
    inputId = 's_sim_ipotesi_mono',
    label = h6('Monoreddito con familiari a carico', style = 'text-align: center;'),
    min = 0,
    max = 1,
    value = 1,
    step = 0.1,
    width = side_bar_width
    
  )


s_sim_ipotesi_coppia =
  
  sliderInput(
    
    inputId = 's_sim_ipotesi_coppia',
    label = h6('Coppia senza familiari a carico', style = 'text-align: center;'),
    min = 0,
    max = 1,
    value = 1,
    step = 0.1,
    width = side_bar_width
    
  )


s_sim_ipotesi_famiglia =
  
  sliderInput(
    
    inputId = 's_sim_ipotesi_famiglia',
    label = h6('Coppia con familiari a carico', style = 'text-align: center;'),
    min = 0,
    max = 1,
    value = 1,
    step = 0.1,
    width = side_bar_width
    
  )


s_sim_ipotesi_now =
  
  sliderInput(
    
    inputId = 's_sim_ipotesi_now',
    label = h6('Erogazione 2022', style = 'text-align: center;'),
    min = 0,
    max = 1,
    value = 0.5,
    step = 0.1,
    width = side_bar_width
    
  )


s_sim_ipotesi_tomorrow =
  
  sliderInput(
    
    inputId = 's_sim_ipotesi_tomorrow',
    label = h6('Erogazione 2023', style = 'text-align: center;'),
    min = 0,
    max = 1,
    value = 0.25,
    step = 0.1,
    width = side_bar_width
    
  )


s_sim_launch =
  
  actionButton(
    
    inputId = 's_sim_launch',
    label = 'Calcolare Fabbisogno',
    icon = shiny::icon('person-running'),
    width = side_bar_width,
    class = "btn-info",
    style = 'height:60px'
    
  )



s_sim_scenario_ipotesi_download =

  downloadButton(

    outputId = 's_sim_scenario_ipotesi_download',
    label = 'Download',
    icon = shiny::icon("download"),
    style = "width:100%;",
    class = "btn-danger",
    style = 'height:60px;padding:17px'

  )




# E. FAI DA TE ------------------------------------------------

s_sim_composizione_itavsazienda_faidate =
  
  selectInput(
    
    inputId = 's_sim_composizione_itavsazienda_faidate',
    label = 'Seleziona Distribuzione',
    choices = c('Italia', "Azienda"),
    selected = 'Italia',
    width = side_bar_width
    
  )

s_sim_scenario_ipotesi_faidate =
    
    selectInput(
        
        inputId = 's_sim_scenario_ipotesi_faidate',
        label = 'Seleziona Scenario Macroeconomico',
        choices = c('Base', 'Medio', 'Stress'),
        selected = 'Medio',
        width = side_bar_width
        
    )


s_sim_ipotesi_base_faidate =
    
    sliderInput(
        
        inputId = 's_sim_ipotesi_base_faidate',
        label = h6('Base: Senza Fattori Aggravanti', style = 'text-align: center;'),
        min = 0,
        max = 1,
        value = 0.75,
        step = 0.1,
        width = side_bar_width
        
    )

s_sim_ipotesi_figli_faidate =
    
    sliderInput(
        
        inputId = 's_sim_ipotesi_figli_faidate',
        label = h6('Figli minori 3 anni', style = 'text-align: center;'),
        min = 0,
        max = 1,
        value = 1,
        step = 0.1,
        width = side_bar_width
        
    )

s_sim_ipotesi_anziani_faidate =
    
    sliderInput(
        
        inputId = 's_sim_ipotesi_anziani_faidate',
        label = h6('Spese per Anziani', style = 'text-align: center;'),
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.1,
        width = side_bar_width
        
    )

s_sim_ipotesi_auto_faidate =
    
    sliderInput(
        
        inputId = 's_sim_ipotesi_auto_faidate',
        label = h6('Utilizzo Auto per Lunghe Distanze', style = 'text-align: center;'),
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.1,
        width = side_bar_width
        
    )

s_sim_ipotesi_singolo_faidate =
    
    sliderInput(
        
        inputId = 's_sim_ipotesi_singolo_faidate',
        label = h6('Singolo', style = 'text-align: center;'),
        min = 0,
        max = 1,
        value = 1,
        step = 0.1,
        width = side_bar_width
        
    )


s_sim_ipotesi_mono_faidate =
    
    sliderInput(
        
        inputId = 's_sim_ipotesi_mono_faidate',
        label = h6('Monoreddito con familiari a carico', style = 'text-align: center;'),
        min = 0,
        max = 1,
        value = 1,
        step = 0.1,
        width = side_bar_width
        
    )


s_sim_ipotesi_coppia_faidate =
    
    sliderInput(
        
        inputId = 's_sim_ipotesi_coppia_faidate',
        label = h6('Coppia senza familiari a carico', style = 'text-align: center;'),
        min = 0,
        max = 1,
        value = 1,
        step = 0.1,
        width = side_bar_width
        
    )


s_sim_ipotesi_famiglia_faidate =
    
    sliderInput(
        
        inputId = 's_sim_ipotesi_famiglia_faidate',
        label = h6('Coppia con familiari a carico', style = 'text-align: center;'),
        min = 0,
        max = 1,
        value = 1,
        step = 0.1,
        width = side_bar_width
        
    )


s_sim_ipotesi_now_faidate =
    
    sliderInput(
        
        inputId = 's_sim_ipotesi_now_faidate',
        label = h6('Erogazione 2022', style = 'text-align: center;'),
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.1,
        width = side_bar_width
        
    )


s_sim_ipotesi_tomorrow_faidate =
    
    sliderInput(
        
        inputId = 's_sim_ipotesi_tomorrow_faidate',
        label = h6('Erogazione 2023', style = 'text-align: center;'),
        min = 0,
        max = 1,
        value = 0.25,
        step = 0.1,
        width = side_bar_width
        
    )



s_sim_scenario_ipotesi_faidate_download =

  downloadButton(

    outputId = 's_sim_scenario_ipotesi_faidate_download',
    label = 'Download',
    icon = shiny::icon("download"),
    style = "width:100%;",
    class = "btn-danger",
    style = 'height:60px;padding:17px'

  )





