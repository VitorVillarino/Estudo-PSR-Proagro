library(readxl)
library(dplyr)
PSR_2013_2018_com_sinistros <- read_excel("Dados/Raw/PSR_2013_2018 com sinistros.xlsx", 
                                sheet = "2013", col_types = c("text", 
                                                              "date", "date", "date", "text", "text", 
                                                              "text", "text", "text", "text", 
                                                              "text", "text", "text", 
                                                              "text", "text", "text", 
                                                              "text"))
PSR_2013_2018_com_sinistros <- union_all(PSR_2013_2018_com_sinistros,
                                         read_excel("Dados/Raw/PSR_2013_2018 com sinistros.xlsx", 
                                                    sheet = "2014", col_types = c("text", 
                                                                                  "date", "date", "date", "text", "text", 
                                                                                  "text", "text", "text", "text", 
                                                                                  "text", "text", "text", 
                                                                                  "text", "text", "text", 
                                                                                  "text")))
PSR_2013_2018_com_sinistros <- union_all(PSR_2013_2018_com_sinistros,
                                         read_excel("Dados/Raw/PSR_2013_2018 com sinistros.xlsx", 
                                                    sheet = "2015", col_types = c("text", 
                                                                                  "date", "date", "date", "text", "text", 
                                                                                  "text", "text", "text", "text", 
                                                                                  "text", "text", "text", 
                                                                                  "text", "text", "text", 
                                                                                  "text")))                             
PSR_2013_2018_com_sinistros <- union_all(PSR_2013_2018_com_sinistros,
                                         read_excel("Dados/Raw/PSR_2013_2018 com sinistros.xlsx", 
                                                    sheet = "2016", col_types = c("text", 
                                                                                  "date", "date", "date", "text", "text", 
                                                                                  "text", "text", "text", "text", 
                                                                                  "text", "text", "text", 
                                                                                  "text", "text", "text", 
                                                                                  "text")))                             
PSR_2013_2018_com_sinistros <- union_all(PSR_2013_2018_com_sinistros,
                                         read_excel("Dados/Raw/PSR_2013_2018 com sinistros.xlsx", 
                                                    sheet = "2017", col_types = c("text", 
                                                                                  "date", "date", "date", "text", "text", 
                                                                                  "text", "text", "text", "text", 
                                                                                  "text", "text", "text", 
                                                                                  "text", "text", "text", 
                                                                                  "text")))     
PSR_2013_2018_com_sinistros <- union_all(PSR_2013_2018_com_sinistros,
                                         read_excel("Dados/Raw/PSR_2013_2018 com sinistros.xlsx", 
                                                    sheet = "2018", col_types = c("text", 
                                                                                  "date", "date", "date", "text", "text", 
                                                                                  "text", "text", "text", "text", 
                                                                                  "text", "text", "text", 
                                                                                  "text", "text", "text", 
                                                                                  "text")))  
PSR_2013_2018_com_sinistros <- union_all(PSR_2013_2018_com_sinistros,
                                         read_excel("Dados/Raw/PSR_2013_2018 com sinistros.xlsx", 
                                                    sheet = "2019", col_types = c("text", 
                                                                                  "date", "date", "date", "text", "text", 
                                                                                  "text", "text", "text", "text", 
                                                                                  "text", "text", "text", 
                                                                                  "text", "text", "text", 
                                                                                  "text")))  

PSR_2013_2018_com_sinistros <- data.frame(lapply(PSR_2013_2018_com_sinistros, function(x) {
                                                                                  gsub(",", ".", x, fixed = T)
                                                                              }))
PSR_2013_2018_com_sinistros[c(9:16)] <- sapply(sapply(PSR_2013_2018_com_sinistros[c(9:16)], as.character),as.numeric)

gc()


PSR_2013_2018_com_sinistros <-  PSR_2013_2018_com_sinistros %>% 
                                group_by(ID_PROPOSTA, DT_PROPOSTA, DT_INICIO_VIGENCIA, DT_FIM_VIGENCIA, NR_DOCUMENTO_SEGURADO, NM_MUNICIPIO_PROPRIEDADE, SG_UF_PROPRIEDADE, NM_CULTURA_GLOBAL, ANO, AN_SAFRA) %>% 
                                summarise(
                                  NR_AREA_TOTAL = mean(NR_AREA_TOTAL),
                                  NR_PRODUTIVIDADE_ESTIMADA	= mean(NR_PRODUTIVIDADE_ESTIMADA),
                                  NR_PRODUTIVIDADE_SEGURADA	= mean(NR_PRODUTIVIDADE_SEGURADA),
                                  VL_LIMITE_GARANTIA	= mean(VL_LIMITE_GARANTIA),
                                  VL_PREMIO_LIQUIDO = mean(VL_PREMIO_LIQUIDO),
                                  VL_SUBVENCAO_FEDERAL = mean(VL_SUBVENCAO_FEDERAL),
                                  PE_TAXA = mean(PE_TAXA)
                                )
gc()
library(writexl)
write_xlsx(PSR_2013_2018_com_sinistros, "PSR_2013_2018_com_sinistros_Agrupado.xlsx")





