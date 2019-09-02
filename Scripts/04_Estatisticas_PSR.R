source("./Scripts/01_Load_Data.R", encoding = "UTF-8")
source("./Scripts/02_Transformacoes_Necessarias.R", encoding = "UTF-8")
library(ggplot2)
library(summarytools)



estat_PSR <- data_PSR %>% filter(Produto_Padronizado == "SOJA", VL_LIMITE_GARANTIA <= 300000, SAFRA != "2019/2020") 
estat_PSR$Premio_Area <- estat_PSR$VL_PREMIO_LIQUIDO/estat_PSR$NR_AREA_TOTAL
estat_PSR$Premio_Area_deflacionado <- estat_PSR$VL_PREMIO_LIQUIDO_deflacionado/estat_PSR$NR_AREA_TOTAL


#Ver com Iran
estat_PSR <- estat_PSR[estat_PSR$Premio_Area < 750,]
estat_PSR <- estat_PSR[estat_PSR$Premio_Area > 7,]




descr(estat_PSR$Premio_Area_deflacionado)

stby(data = estat_PSR$Premio_Area_deflacionado , 
     INDICES = estat_PSR$SAFRA, 
     FUN = descr, stats = c("mean", "sd", "min", "med", "max"))


descr(estat_PSR$NR_AREA_TOTAL)
openxlsx::write.xlsx(estat_PSR, "estat_PSR.xlsx")


esquisse::esquisser()


#Fazer joins e mostrar estatísticas


