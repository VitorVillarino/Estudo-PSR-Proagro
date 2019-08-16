source("./Scripts/01_Load_Data.R", encoding = "UTF-8")
source("./Scripts/02_Transformação.R", encoding = "UTF-8")
source("./Scripts/Function_Gerais.R", encoding = "UTF-8")
library(ggplot2)
library(dplyr)
library(summarytools)


estat_PSR <- data_PSR %>% filter(Produto_Padronizado == "SOJA", VL_LIMITE_GARANTIA <= 300000) 
estat_PSR$Premio_Area <- estat_PSR$VL_PREMIO_LIQUIDO/estat_PSR$NR_AREA_TOTAL

estat_PSR <- remove_outliers(estat_PSR)


#Ver package outliers
https://cran.r-project.org/web/packages/outliers/outliers.pdf

estat_PSR[estat_PSR$Premio_Area == max(estat_PSR$Premio_Area),]
