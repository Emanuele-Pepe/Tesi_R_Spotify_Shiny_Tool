
default_th <-

    bs_theme(

      version = 5,

        bootswatch = 'zephyr',

            primary = "#015958",
            secondary = '#8FBCBB',

            info = '#00ABBD',
            success = '#A1C7E0',
            warning = '#81A1C1',
            danger = '#FFCB9A',

      font_scale = 1

        )



### reactable global theme

header_def = 
  
  tagList(
    
    tags$head(
      
      tags$style(".topimg_left {
                            margin-left: 0px;
                            margin-right: 20px;
                            margin-top: -17px;
                            margin-bottom: -60px;
                          }"),
      
      tags$style(".topimg_right {
                            margin-left: 0px;
                            margin-right: 0px;
                            margin-top: 0px;
                            margin-bottom: 0px;
                            position: fixed;
                            right: 20px;
                            top: 15px;
                          }"),
      
      tags$style(" nav { box-shadow: 0 1px 2px 0 rgba(0,0,0,.2);}"),
      
      tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                         padding-top:12px !important; 
                         padding-bottom:0 !important;
                         height: 45px;
                         color: #225E64;
                         font-size: 15px;
                         font-weight: bold;
       }
                         
                         .navbar.navbar-default {
                         color: #225E64;
                         }
                         
                        .navbar {
                        min-height:40px !important;
                        }')),
      
      tags$head(
        tags$style(type="text/css", ".inline label{ display: table-cell; text-align: right; vertical-align: bottom; }
                                   .inline .form-group { display: table-row;}")
      )
    ) 
  )



header_tables = colDef(headerStyle = list(background = "#015958", borderColor = "#015958", color = '#fff', cursor = "pointer"),
                       format = colFormat(separator = TRUE, digits = 0),
                       align = "center")

non_header_tables = colDef(format = colFormat(separator = TRUE, digits = 0), align = "center")
