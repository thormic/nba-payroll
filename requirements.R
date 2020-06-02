requiredPackages = c('shiny',
                     'shinydashboard',
                     'DT',
                     'gbm',
                     'readr',
                     'DALEX',
                     'ingredients',
                     'iBreakDown',
                     'ggplot2',
                     'scales',
                     'randomForest',
                     'imputeTS',
                     'dplyr',
                     'dashboardthemes',
                     'fields',
                     'ggthemes')

  for(p in requiredPackages){
    if(!require(p,character.only = TRUE)) install.packages(p)
  }


requiredPackages = c('shiny','shinydashboard','DT','gbm','readr','DALEX','ingredients','iBreakDown','ggplot2','scales','randomForest','imputeTS','dplyr', 'dashboardthemes','fields','ggthemes')