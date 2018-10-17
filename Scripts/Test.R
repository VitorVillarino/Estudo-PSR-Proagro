library(tidyverse)
library(readxl)

data_PSR <- read_xlsx("./Dados/Raw/PSR - 2016.xlsx",
                      col_types = c(
                        "text", #NM_RAZAO_SOCIAL
                        "text", #ID_PROPOSTA
                        "date", #DT_PROPOSTA
                        "date", #DT_INICIO_VIGENCIA
                        "date", #DT_FIM_VIGENCIA
                        "text", #NR_APOLICE
                        "text", #DT_APOLICE
                        "text", #NR_DOCUMENTO_SEGURADO
                        "text", #NM_SEGURADO
                        "text", #NM_CULTURA_GLOBAL
                        "text", #SG_UF_PROPRIEDADE
                        "text", #NM_MUNICIPIO_PROPRIEDADE
                        "numeric", #NR_AREA_TOTAL
                        "numeric", #VL_LIMITE_GARANTIA
                        "numeric", #VL_PREMIO_LIQUIDO
                        "numeric", #VL_SUBVENCAO_FEDERAL
                        "numeric", #NR_PRODUTIVIDADE_ESTIMADA
                        "numeric", #NR_PRODUTIVIDADE_SEGURADA
                        "numeric", #PE_NIVEL_COBERTURA
                        "numeric", #NR_GRAU_LAT
                        "numeric", #NR_MIN_LAT
                        "numeric", #NR_SEG_LAT
                        "text", #LATITUDE
                        "numeric", #NR_GRAU_LONG
                        "numeric", #NR_MIN_LONG
                        "numeric", #NR_SEG_LONG
                        "text", #LONGITUDE
                        "numeric"  #CD_PROCESSO_SUSEP
                    ))

data_PSR$NM_MUNICIPIO_PROPRIEDADE <- toupper(data_PSR$NM_MUNICIPIO_PROPRIEDADE)

municipios <- read_xlsx("./Dados/Auxiliares/Municipios.xlsx",
                      col_types = c(
                        "text", #UF
                        "text", #Sigla UF
                        "numeric", #CodUF
                        "text", #NomeMunic
                        "numeric", #Codmun
                        "numeric"  #Codmundv
                      ))
