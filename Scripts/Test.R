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

data_PSR <- data_PSR %>% mutate(NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO = 
                                  case_when( 
                                    NM_MUNICIPIO_PROPRIEDADE == "BAIRRO LIMOEIRO" && SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "FOZ DO IGUACU" && SG_UF_PROPRIEDADE == "PR" ~ "FOZ DO IGUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "CANDEIA" && SG_UF_PROPRIEDADE == "PR" ~ "MARIPÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAMBE" && SG_UF_PROPRIEDADE == "PR" ~ "CAMBÉ",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITABERA" && SG_UF_PROPRIEDADE == "SP" ~ "ITABERÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "INDÁPOLIS" && SG_UF_PROPRIEDADE == "MS" ~ "DOURADOS",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAPAO BONITO" && SG_UF_PROPRIEDADE == "SP" ~ "CAPÃO BONITO",
                                    NM_MUNICIPIO_PROPRIEDADE == "MAUA DA SERRA" && SG_UF_PROPRIEDADE == "PR" ~ "MAUÁ DA SERRA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SENGES" && SG_UF_PROPRIEDADE == "PR" ~ "SENGÉS",
                                    NM_MUNICIPIO_PROPRIEDADE == "JAGUARIAIVA" && SG_UF_PROPRIEDADE == "PR" ~ "JAGUARIAÍVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITARARE" && SG_UF_PROPRIEDADE == "SP" ~ "ITARARÉ",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOSSA SENHORA DA CANDELÁRIA" && SG_UF_PROPRIEDADE == "PR" ~ "BANDEIRANTES",
                                    NM_MUNICIPIO_PROPRIEDADE == "SEDE ALVORADA" && SG_UF_PROPRIEDADE == "PR" ~ "CASCAVEL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOÃO D'OESTE" && SG_UF_PROPRIEDADE == "PR" ~ "CASCAVEL",
                                    NM_MUNICIPIO_PROPRIEDADE == "QUEDAS DO IGUACU" && SG_UF_PROPRIEDADE == "PR" ~ "QUEDAS DO IGUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "OURO PRETO" && SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "TAQUARUNA" && SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO CLEMENTE" && SG_UF_PROPRIEDADE == "PR" ~ "SANTA HELENA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO LUIZ" && SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PIRAPÓ" && SG_UF_PROPRIEDADE == "PR" ~ "APUCARANA",
                                    NM_MUNICIPIO_PROPRIEDADE == "LERROVILLE" && SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "DEZ DE MAIO" && SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAETANO MENDES" && SG_UF_PROPRIEDADE == "PR" ~ "TIBAGI",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA RITA DO OESTE" && SG_UF_PROPRIEDADE == "PR" ~ "SANTA RITA D'OESTE",
                                    NM_MUNICIPIO_PROPRIEDADE == "MALU" && SG_UF_PROPRIEDADE == "PR" ~ "TERRA BOA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO PEDRO" && SG_UF_PROPRIEDADE == "PR" ~ "APUCARANA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SELVA" && SG_UF_PROPRIEDADE == "PR" ~ "OLÍMPIO LOPES",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOA VISTA" && SG_UF_PROPRIEDADE == "PR" ~ "CAMPO LARGO",
                                    NM_MUNICIPIO_PROPRIEDADE == "ARAUCARIA" && SG_UF_PROPRIEDADE == "PR" ~ "ARAUCÁRIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAPAO ALTO" && SG_UF_PROPRIEDADE == "SC" ~ "CAPÃO ALTO",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOM JESUS" && SG_UF_PROPRIEDADE == "GO" ~ "BOM JESUS DE GOIÁS",
                                    NM_MUNICIPIO_PROPRIEDADE == "UBERLANDIA" && SG_UF_PROPRIEDADE == "MG" ~ "UBERLÂNDIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "INDIANOPOLIS" && SG_UF_PROPRIEDADE == "MG" ~ "INDIANÓPOLIS",
                                    NM_MUNICIPIO_PROPRIEDADE == "BRAGANCA PAULISTA" && SG_UF_PROPRIEDADE == "SP" ~ "BRAGANÇA PAULISTA",
                                    NM_MUNICIPIO_PROPRIEDADE == "MÁGDA" && SG_UF_PROPRIEDADE == "SP" ~ "MAGDA",
                                    NM_MUNICIPIO_PROPRIEDADE == "BIRIGÜI" && SG_UF_PROPRIEDADE == "SP" ~ "BIRIGUI",
                                    NM_MUNICIPIO_PROPRIEDADE == "ARACOIABA DA SERRA" && SG_UF_PROPRIEDADE == "SP" ~ "ARAÇOIABA DA SERRA",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITABOA" && SG_UF_PROPRIEDADE == "SP" ~ "RIBEIRÃO BRANCO",
                                    NM_MUNICIPIO_PROPRIEDADE == "MOGI GUACU" && SG_UF_PROPRIEDADE == "SP" ~ "MOGI GUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "APIAÍ-MIRIM" && SG_UF_PROPRIEDADE == "SP" ~ "CAPÃO BONITO",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA OLIVA" && SG_UF_PROPRIEDADE == "RS" ~ "CAXIAS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA LÚCIA DO PIAÍ" && SG_UF_PROPRIEDADE == "RS" ~ "CAXIAS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SILVEIRA" && SG_UF_PROPRIEDADE == "RS" ~ "SÃO JOSÉ DOS AUSENTES",
                                    NM_MUNICIPIO_PROPRIEDADE == "FAZENDA SOUZA" && SG_UF_PROPRIEDADE == "RS" ~ "CAXIAS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "PAIQUERÊ" && SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOSSA SENHORA DA APARECIDA" && SG_UF_PROPRIEDADE == "PR" ~ "ROLÂNDIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA SÃO JOÃO" && SG_UF_PROPRIEDADE == "RS" ~ "CRUZ ALTA",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA IPIRANGA" && SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "COMANDAI" && SG_UF_PROPRIEDADE == "RS" ~ "COMANDAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "BELA VISTA" && SG_UF_PROPRIEDADE == "RS" ~ "PASSO FUNDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "FLUVIÓPOLIS" && SG_UF_PROPRIEDADE == "PR" ~ "SÃO MATEUS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "VARZEA DA PALMA" && SG_UF_PROPRIEDADE == "MG" ~ "VÁRZEA DA PALMA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVA VICOSA" && SG_UF_PROPRIEDADE == "BA" ~ "NOVA VIÇOSA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CACADOR" && SG_UF_PROPRIEDADE == "SC" ~ "CAÇADOR",
                                    TRUE ~ NM_MUNICIPIO_PROPRIEDADE)
)





# municipios <- read_xlsx("./Dados/Auxiliares/Municipios.xlsx",
#                       col_types = c(
#                         "text", #UF
#                         "text", #Sigla UF
#                         "numeric", #CodUF
#                         "text", #NomeMunic
#                         "numeric", #Codmun
#                         "numeric"  #Codmundv
#                       ))
# 

 municipios <- read_xls("./Dados/Auxiliares/DTB_BRASIL_MUNICIPIO.xls",
                       col_types = c(
                         "text", #UF
                         "text", #Nome_UF
                         "text", #Sigla_UF
                         "text", #Mesorregião Geográfica
                         "text", #Nome_Mesorregião
                         "text", #Microrregião Geográfica
                         "text", #Nome_Microrregião
                         "text", #Município
                         "text", #Código Município Completo
                         "text", #Nome_Município
                         "text" #Nome_Município_Maiuscula 
                       ))

 distritos <- read_xls("./Dados/Auxiliares/DTB_BRASIL_DISTRITO.xls",
                        col_types = c(
                          "text", #UF
                          "text", #Nome_UF
                          "text", #Sigla_UF
                          "text", #Mesorregião Geográfica
                          "text", #Nome_Mesorregião
                          "text", #Microrregião Geográfica
                          "text", #Nome_Microrregião
                          "text", #Município
                          "text", #Código Município Completo
                          "text", #Nome_Município
                          "text", #Distrito
                          "text", #Código de Distrito Completo
                          "text", #Nome_Distrito
                          "text" #Nome_Distrito_Maiuscula
                        ))




municipios_nao_encontrados <- anti_join(data_PSR, municipios, by = c("SG_UF_PROPRIEDADE" = "Sigla_UF", "NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO" = "Nome_Município_Maiuscula"))  %>% 
                              left_join(distritos, by =c("SG_UF_PROPRIEDADE" = "Sigla_UF", "NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO" = "Nome_Distrito_Maiuscula")) %>% 
                              distinct(NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO, SG_UF_PROPRIEDADE, Município, Nome_Município, Nome_Distrito) 




