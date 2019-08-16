source("./Scripts/01_Load_Data.R", encoding = "UTF-8")
source("./Scripts/02_Transformação.R", encoding = "UTF-8")
library(ggplot2)
library(dplyr)
library(summarytools)


estat_PSR <- data_PSR %>% filter(Produto_Padronizado == "SOJA", VL_LIMITE_GARANTIA <= 300000) 
estat_PSR$Premio_Area <- estat_PSR$VL_PREMIO_LIQUIDO/estat_PSR$NR_AREA_TOTAL
estat_PSR <- estat_PSR[estat_PSR$Premio_Area < 400,]
estat_PSR <- estat_PSR[estat_PSR$Premio_Area > 7,]
estat_PSR <- estat_PSR[estat_PSR$NR_AREA_TOTAL < 750,]

descr(estat_PSR$NR_AREA_TOTAL)
openxlsx::write.xlsx(estat_PSR, "estat_PSR.xlsx")



esquisse::esquisser()
