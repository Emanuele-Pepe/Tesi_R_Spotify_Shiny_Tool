


# : ========



# Install R Packages ===============================================

#install.packages('renv')
install.packages('shiny')
install.packages('bslib')
install.packages('shinyWidgets')
install.packages('shinyalert')
install.packages('bsutils')
install.packages('data.table')
install.packages('openxlsx')
install.packages('ggplot2')
install.packages('highcharter')
install.packages('mlr3')
install.packages('mlr3verse')
install.packages('kknn')
install.packages('Cairo')
install.packages('RColorBrewer')
install.packages('reactable')
install.packages('devtools')
install.packages('shinyBS')

# Install Git R Packages ===============================================

devtools::install_github('aba-innovationteam/innteamUtils')
devtools::install_github("mlr-org/mlr3extralearners")
devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.0.6/catboost-R-Windows-1.0.6.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))
mlr3extralearners::install_learners('classif.catboost')

