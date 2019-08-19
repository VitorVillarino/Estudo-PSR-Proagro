library(ggplot2)
library(dplyr)
library(summarytools)

#Removendo outliers - PSR

estat_PSR <- data_PSR %>% filter(Produto_Padronizado == "SOJA", VL_LIMITE_GARANTIA <= 300000) 
estat_PSR <- estat_PSR[estat_PSR$Premio_Area < 300,]
estat_PSR <- estat_PSR[estat_PSR$Premio_Area > 7,]
estat_PSR <- estat_PSR[estat_PSR$NR_AREA_TOTAL < 750,]



esquisse::esquisser()
descr(estat_PSR$Premio_Area)
openxlsx::write.xlsx(estat_PSR, "estat_PSR.xlsx")



estat_Proagro <- data_Proagro %>% filter(Produto_Padronizado == "SOJA")
estat_Proagro$VL_TOT_Medio <- estat_Proagro$VL_TOT/estat_Proagro$QT_ENQ
estat_Proagro$AREA_Media <- estat_Proagro$AREA/estat_Proagro$QT_ENQ
estat_Proagro$Premio_Medio <- estat_Proagro$VL_ADIC/estat_Proagro$AREA

estat_Proagro <- estat_Proagro[estat_Proagro$QT_ENQ >= 20,]
descr(estat_Proagro$QT_ENQ)
estat_Proagro$QT_ENQ



openxlsx::write.xlsx(estat_Proagro, "estat_Prograo.xlsx")
descr(estat_Proagro$AREA_Media)

esquisse::esquisser()
