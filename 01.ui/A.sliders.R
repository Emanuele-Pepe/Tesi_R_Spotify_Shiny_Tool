### SLIDERS ###


side_bar_width = '100%'

# DESCRIPTIVE STAT -----------------------
## QUANTITATIVE VAR -------------------

var_selector_quant_descr <- selectInput("var_selector_quant_descr", label = "Quantitative Variables", choices = NULL, selected = NULL, multiple = F,
                                       selectize = TRUE, width = side_bar_width, size = NULL)

var_selector_scatt_x <- selectInput("var_selector_scatt_x", label = "Quantitative Variables", choices = NULL, selected = NULL, multiple = F,
                                    selectize = TRUE, width = side_bar_width, size = NULL)
var_selector_scatt_y<- selectInput("var_selector_scatt_y", label = "Quantitative Variables", choices = NULL, selected = NULL, multiple = F,
                                   selectize = TRUE, width = side_bar_width, size = NULL)

## CATEGORICAL VAR -----------------------------------------------------

var_selector_cat_descr <- selectInput("var_selector_cat_descr", label = "Categorical Variables", choices = NULL, selected = NULL, multiple = F,
                                        selectize = TRUE, width = side_bar_width, size = NULL)


# DATA ANALYSIS -------------------

var_selector_clus <- selectInput("var_selector_clus", label = "Variables to perform k-means clustering", choices = NULL, selected = NULL, multiple = T,
            selectize = TRUE, width = side_bar_width, size = NULL)

cluster_num_selector <- numericInput(inputId = "cluster_num_selector", label = "Select number of clusters", 
                                     value = NULL, min = 1, max = 10, width = side_bar_width)

var_selector_pca <- selectInput("var_selector_pca", label = "Variables to perform PCA", choices = NULL, selected = NULL, multiple = T,
                                selectize = TRUE, width = side_bar_width, size = NULL)