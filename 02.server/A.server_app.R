

### SERVER ###


server_app = function(input, output, session) {


     
    ## 0. Info ===========================================================================================================================================================

    output$fileViewer <- renderUI({
      
      src_file = paste0('Social Targeting - README.pdf')
      
      tags$iframe(style = 'height:900px; width:100%; scrolling=yes; border:1px solid white;>', 
                  src = as.character(src_file))
      
    })     
     
      
# X. END =============================================================================
      
      session$onSessionEnded(function() {
          stopApp()
      })      
 
}    





