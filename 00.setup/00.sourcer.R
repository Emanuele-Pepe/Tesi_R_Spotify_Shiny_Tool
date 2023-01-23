

# : ========================================================================================================================================================



## Sourcer ----------------------------------------------------------------
set.seed(123)

options(scipen = 9999)
# Set up ===============================================================

source(file.path('00.setup', "A.packages.R"))
source(file.path('00.setup', "B.API_auth.R"))
source(file.path('00.setup', "C.theme.R"))
source(file.path('00.setup', "D.preparation.R"))



# UI ===============================================================
source(file.path('01.ui', "A.sliders.R"))
source(file.path('01.ui', "B.buttons.R"))
source(file.path('01.ui', "C.tabs.R"))
# source(file.path('01.ui', "D.modules.R"))
source(file.path('01.ui', "E.ui_app.R"))


# Source SERVER ===============================================================
source(file.path('02.server', "A.server_app.R"))





# Body ===============================================================




