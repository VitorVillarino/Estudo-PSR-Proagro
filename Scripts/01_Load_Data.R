library(readxl)
library(dplyr)


#################################################### Inflação #################################################### 
inflacao <- data.frame(jul2014 = 6.5, jul2015 = 9.56, jul2016 = 8.74, jul2017=2.71, jul2018=4.48, jul2019=3.22)






################################################### Qtd Mínima ################################################### 
qtd_min_PSR <- 20
qtd_min_proagro <- 20



#################################################### BASES IBGE ####################################################

#Base de Municípiops do IBGE. Pegaremos daqui o código do Município para fazer algo mais simples e eficiente do que 
#comparar strings.
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
names(municipios)[names(municipios)=="Código Município Completo"] <- "Cod_Municipio"
names(municipios)[names(municipios)=="Microrregião Geográfica"] <- "Cod_Microrregiao"



#################################################### PRODUTOS ####################################################


#Base de Produtos - Padronizados
produtos_padronizados <- read_xlsx("./Dados/Auxiliares/Padronização de Produtos.xlsx",
                                   col_types = c(
                                     "numeric", #idProduto
                                     "text", #Proagro
                                     "text", #PSR
                                     "text", #Tabela 949 - 2006
                                     "text", #Tabela 822 - 2006
                                     "text", #Tabela 6615 - 2017
                                     "text", #Tabela 6616 - 2017
                                     "text", #Tabela 6618
                                     "text", #Tabela 6619
                                     "text"  #Produto Padronizado
                                   ))



#################################################### PSR ####################################################

data_PSR_tmp <- read_excel("Dados/Raw/PSR_2013_2018_com_sinistros_Agrupado_pt1_novo.xlsm", 
                             col_types = c("text", #ID_PROPOSTA
                                           "text", #DT_PROPOSTA
                                           "text", #DT_INICIO_VIGENCIA
                                           "text", #DT_FIM_VIGENCIA
                                           "text", #NR_DOCUMENTO_SEGURADO
                                           "text", #NM_MUNICIPIO_PROPRIEDADE
                                           "text", #SG_UF_PROPRIEDADE
                                           "text", #NM_CULTURA_GLOBAL
                                           "numeric", #ANO
                                           "text", #AN_SAFRA
                                           "numeric", #NR_AREA_TOTAL
                                           "numeric", #NR_PRODUTIVIDADE_ESTIMADA
                                           "numeric", #NR_PRODUTIVIDADE_SEGURADA
                                           "numeric", #VL_LIMITE_GARANTIA
                                           "numeric", #VL_PREMIO_LIQUIDO
                                           "numeric",  #VL_SUBVENCAO_FEDERAL
                                           "text",  #CEP_String
                                           "text",  #Cidade_CEP
                                           "text",  #Estado_CEP
                                           "text"  #Cod_Municipio_CEP
                                       ))

data_PSR_tmp <- union_all(data_PSR_tmp, read_excel("Dados/Raw/PSR_2013_2018_com_sinistros_Agrupado_pt2_novo.xlsm", 
                            col_types = c("text", #ID_PROPOSTA
                                          "text", #DT_PROPOSTA
                                          "text", #DT_INICIO_VIGENCIA
                                          "text", #DT_FIM_VIGENCIA
                                          "text", #NR_DOCUMENTO_SEGURADO
                                          "text", #NM_MUNICIPIO_PROPRIEDADE
                                          "text", #SG_UF_PROPRIEDADE
                                          "text", #NM_CULTURA_GLOBAL
                                          "numeric", #ANO
                                          "text", #AN_SAFRA
                                          "numeric", #NR_AREA_TOTAL
                                          "numeric", #NR_PRODUTIVIDADE_ESTIMADA
                                          "numeric", #NR_PRODUTIVIDADE_SEGURADA
                                          "numeric", #VL_LIMITE_GARANTIA
                                          "numeric", #VL_PREMIO_LIQUIDO
                                          "numeric",  #VL_SUBVENCAO_FEDERAL
                                          "text",  #CEP_String
                                          "text",  #Cidade_CEP
                                          "text",  #Estado_CEP
                                          "text"  #Cod_Municipio_CEP
                            )))



#Arrumando datas
data_PSR_tmp$DT_PROPOSTA <- as.Date(data_PSR_tmp$DT_PROPOSTA , origin = "1899-12-30")
data_PSR_tmp$DT_INICIO_VIGENCIA <- as.Date(data_PSR_tmp$DT_INICIO_VIGENCIA , origin = "1899-12-30")
data_PSR_tmp$DT_FIM_VIGENCIA <- as.Date(data_PSR_tmp$DT_FIM_VIGENCIA , origin = "1899-12-30")

#Premio Pag
data_PSR_tmp$VL_PREMIO_PAGO <- data_PSR_tmp$VL_PREMIO_LIQUIDO - data_PSR_tmp$VL_SUBVENCAO_FEDERAL 

#Base PSR Indenizações
PSR_Indenizacoes <- read_excel("Dados/Raw/PSR_Indenizacoes.xlsx")
data_PSR_tmp <- data_PSR_tmp %>% 
            left_join(
                PSR_Indenizacoes,
                by =c("ID_PROPOSTA" = "ID_PROPOSTA")
            )


#Nome do Município maiúscula - Normalizando para cruzar com a base do IBGE 
data_PSR_tmp$NM_MUNICIPIO_PROPRIEDADE <- toupper(data_PSR_tmp$NM_MUNICIPIO_PROPRIEDADE)


#Separando em municipios que consegui geocodificar o CEP e outros que não. 
data_PSR_Geocodificado <- data_PSR_tmp[!is.na(data_PSR_tmp$Cod_Municipio_CEP),]
data_PSR_Nao_Geocodificado <- data_PSR_tmp[is.na(data_PSR_tmp$Cod_Municipio_CEP),]
data_PSR_tmp <- NULL



#Se consegui geocodificar, só fazer o join pelo ID do municipio, caso contrário checa municipio e UF
data_PSR_Geocodificado <- data_PSR_Geocodificado %>% 
  left_join(
    municipios %>% select(Sigla_UF,Nome_Município_Maiuscula,Cod_Microrregiao,Cod_Municipio),
    by =c("Cod_Municipio_CEP" = "Cod_Municipio")
  )
data_PSR_Geocodificado$Cod_Municipio <- data_PSR_Geocodificado$Cod_Municipio_CEP


data_PSR_Nao_Geocodificado <- data_PSR_Nao_Geocodificado %>% mutate(MUNICIPIO_CORRIGIDO =
                                    case_when(
                                    NM_MUNICIPIO_PROPRIEDADE == "BIRIGÜI" & SG_UF_PROPRIEDADE == "SP" ~ "BIRIGUI",
                                    NM_MUNICIPIO_PROPRIEDADE == "JANSEN" & SG_UF_PROPRIEDADE == "RS" ~ "FARROUPILHA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTANA DO LIVRAMENTO" & SG_UF_PROPRIEDADE == "RS" ~ "SANT'ANA DO LIVRAMENTO",
                                    TRUE ~ NM_MUNICIPIO_PROPRIEDADE)
                                )
data_PSR_Nao_Geocodificado <- data_PSR_Nao_Geocodificado %>% 
  left_join(
    municipios %>% select(Sigla_UF,Nome_Município_Maiuscula,Cod_Microrregiao,Cod_Municipio),
    by =c("SG_UF_PROPRIEDADE" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Município_Maiuscula")
  )

data_PSR_Nao_Geocodificado$Sigla_UF <- data_PSR_Nao_Geocodificado$SG_UF_PROPRIEDADE
data_PSR_Nao_Geocodificado$Nome_Município_Maiuscula <- data_PSR_Nao_Geocodificado$MUNICIPIO_CORRIGIDO
data_PSR_Nao_Geocodificado$MUNICIPIO_CORRIGIDO <- NULL


data_PSR <- union_all(data_PSR_Geocodificado,data_PSR_Nao_Geocodificado)
rm(data_PSR_Geocodificado)
rm(data_PSR_Nao_Geocodificado)


#Padronizando Produtos
data_PSR <- data_PSR %>% 
  left_join(
    produtos_padronizados %>% select(PSR,Produto_Padronizado), 
    by = c("NM_CULTURA_GLOBAL" = "PSR")
  )

#Dropando Colunas não utilizadas de MUNICIPIO
data_PSR$NM_MUNICIPIO_PROPRIEDADE <- NULL
data_PSR$NM_CULTURA_GLOBAL <- NULL
data_PSR <- data_PSR %>% rename(SAFRA = AN_SAFRA)


#Adicionando  Prêmio por área
data_PSR$Premio_Area <- data_PSR$VL_PREMIO_LIQUIDO/data_PSR$NR_AREA_TOTAL



#################################################### Proagro ####################################################


data_Proagro <- read_excel("Dados/Raw/CONTRATACAO PROAGRO 20190801.xlsx")
data_Proagro <- data_Proagro[!data_Proagro$MUNICIPIO=="Não informado",]
data_Proagro <- data_Proagro %>% mutate(MUNICIPIO_CORRIGIDO = 
                                  case_when( 
                                    MUNICIPIO == "BIRITIBA-MIRIM" & UF == "SP" ~ "BIRITIBA MIRIM",
                                    MUNICIPIO == "MOJI MIRIM" & UF == "SP" ~ "MOGI MIRIM",
                                    MUNICIPIO == "RESTINGA SECA" & UF == "RS" ~ "SANTO ÂNGELO",
                                    MUNICIPIO == "FLORÍNIA" & UF == "SP" ~ "FLORÍNEA",
                                    MUNICIPIO == "LAURO MULLER" & UF == "SC" ~ "LAURO MÜLLER",
                                    MUNICIPIO == "SÃO CRISTOVÃO DO SUL" & UF == "SC" ~ "SÃO CRISTÓVÃO DO SUL",
                                    MUNICIPIO == "VESPASIANO CORREA" & UF == "RS" ~ "VESPASIANO CORRÊA",
                                    MUNICIPIO == "ATILIO VIVACQUA" & UF == "ES" ~ "ATÍLIO VIVACQUA",
                                    MUNICIPIO == "SÃO VICENTE FERRER" & UF == "PE" ~ "SÃO VICENTE FÉRRER",
                                    MUNICIPIO == "WESTFALIA" & UF == "RS" ~ "WESTFÁLIA",
                                    MUNICIPIO == "SÃO LUÍZ DO NORTE" & UF == "GO" ~ "SÃO LUIZ DO NORTE",
                                    MUNICIPIO == "POXORÉO" & UF == "MT" ~ "POXORÉU",
                                    MUNICIPIO == "BRASILIA (BRAZLANDIA)" & UF == "DF" ~ "BRASÍLIA",
                                    MUNICIPIO == "BRASILIA (PLANALTINA)" & UF == "DF" ~ "BRASÍLIA",
                                    MUNICIPIO == "BRASILIA (PARANOA)" & UF == "DF" ~ "BRASÍLIA",
                                    TRUE ~ MUNICIPIO))


#Adicionando código Município e Microrregião
data_Proagro <- data_Proagro %>% 
  left_join(
    municipios %>% select(Sigla_UF,Nome_Município_Maiuscula,Cod_Microrregiao,Cod_Municipio),
    by =c("UF" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Município_Maiuscula")
  )

#Padronizando Produtos
data_Proagro <- data_Proagro %>% 
  left_join(
    produtos_padronizados %>% select(Proagro,Produto_Padronizado), 
    by =c("PRODUTO" = "Proagro")
  )


#Dropando Colunas não utilizadas de MUNICIPIO
data_Proagro$MUNICIPIO <- NULL
data_Proagro$PRODUTO <- NULL





#################################################### CENSO RURAL #################################################### 

data_Censo_Rural <- NULL

# Tabela 6615 - Número de estabelecimentos por produtos da lavoura temporária - resultados preliminares 2017
data_Censo_6615 <- read.csv("./Dados/Raw/Censo - 2017/tabela6615.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)
data_Censo_6615[data_Censo_6615=="-"]<-0
data_Censo_6615[data_Censo_6615 == "X"] <- NA
data_Censo_6615 <- data_Censo_6615 %>% 
    left_join(
      produtos_padronizados %>%  select ("Tabela 6615 - 2017", "Produto_Padronizado"), 
      by = c("Produtos" = "Tabela 6615 - 2017")
    )


# # Tabela 6616 - Número de estabelecimentos agropecuários e Número de pés existentes, por produtos da lavoura permanente - resultados preliminares 2017
# data_Censo_6616 <- read.csv("./Dados/Raw/Censo - 2017/tabela6616.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)
# data_Censo_6616[data_Censo_6616=="-"]<-0
# data_Censo_6616[data_Censo_6616=="X"]<-NA
# 
# 
# # Tabela 6618 - Número de estabelecimentos agropecuários e Quantidade produzida, por produtos da agroindústria rural - resultados preliminares 2017
# data_Censo_6618 <- read.csv("./Dados/Raw/Censo - 2017/tabela6618.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)
# data_Censo_6618[data_Censo_6618=="-"]<-0
# data_Censo_6618[data_Censo_6618=="X"]<-NA
# 
# 
# # Tabela 6619 - Número de estabelecimentos agropecuários e Quantidade produzida, por produtos da horticultura - resultados preliminares 2017
# data_Censo_6619 <- read.csv("./Dados/Raw/Censo - 2017/tabela6619.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)
# data_Censo_6619[data_Censo_6619=="-"]<-0
# data_Censo_6619[data_Censo_6619=="X"]<-NA


data_Censo_Rural <- data_Censo_6615 
rm(data_Censo_6615)



data_Censo_Rural


######################### Dados Censo Geral ##################################

data_Censo_Geral <- NULL

#Tabela 6635 - Número de estabelecimentos agropecuários, Área dos estabelecimentos agropecuários, Área territorial total e Condição legal das terras - resultados preliminares 2017																					
data_Censo_6635 <- read_excel("Dados/Raw/Censo - 2017/tabela6635.xlsx", col_types = "text")
data_Censo_6635[data_Censo_6635=="-"]<-0
data_Censo_6635[data_Censo_6635=="X"]<-NA
data_Censo_6635[, c(3:30)] <- sapply(data_Censo_6635[, c(3:30)], as.numeric)

data_Censo_Geral <- data_Censo_6635[, -c(2,11,13,15,17,19,21,23,25,27,29)]
rm(data_Censo_6635)

#Tabela 6639 - Número de estabelecimentos agropecuários e Número de unidades armazenadoras e capacidade, por tipo de unidade armazenadora - resultados preliminares 2017														
data_Censo_6639 <- read_excel("Dados/Raw/Censo - 2017/tabela6639.xlsx")
data_Censo_6639[data_Censo_6639=="-"]<-0
data_Censo_6639[data_Censo_6639=="X"]<-NA
data_Censo_6639[, c(3:26)] <- sapply(data_Censo_6639[, c(3:26)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6639[, -c(2,3,5,7,9,11,13,15,17,19,21,23,25)] , by = "Cód.")
rm(data_Censo_6639)

#Tabela 6640 - Número de estabelecimentos agropecuários, Sistema de preparo do solo e Área com plantio direto na palha - resultados preliminares 2017					
data_Censo_6640 <- read_excel("Dados/Raw/Censo - 2017/tabela6640.xlsx")
data_Censo_6640[data_Censo_6640=="-"]<-0
data_Censo_6640[data_Censo_6640=="X"]<-NA
data_Censo_6640[, c(3:12)] <- sapply(data_Censo_6640[, c(3:12)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6640[, -c(2,3,5,7,9,11)], by = "Cód.")
rm(data_Censo_6640)


#Tabela 6641 - Número de estabelecimentos agropecuários e Número de tratores, implementos e máquinas existentes nos estabelecimentos agropecuários - resultados preliminares 2017
data_Censo_6641 <- read_excel("Dados/Raw/Censo - 2017/tabela6641.xlsx")
data_Censo_6641[data_Censo_6641=="-"]<-0
data_Censo_6641[data_Censo_6641=="X"]<-NA
data_Censo_6641[, c(3:18)] <- sapply(data_Censo_6641[, c(3:18)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6641[, -c(2,3,5,7,9,11,13,15,17)], by = "Cód.")
rm(data_Censo_6641)


#Tabela 6642 - Número de estabelecimentos agropecuários e Número de veículos existentes nos estabelecimentos agropecuários - resultados preliminares 2017												
data_Censo_6642 <- read_excel("Dados/Raw/Censo - 2017/tabela6642.xlsx")
data_Censo_6642[data_Censo_6642=="-"]<-0
data_Censo_6642[data_Censo_6642=="X"]<-NA
data_Censo_6642[, c(3:22)] <- sapply(data_Censo_6642[, c(3:22)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6642[, -c(2,3,5,7,9,11,13,15,17,19,21)], by = "Cód.")
rm(data_Censo_6642)


#Tabela 6643 - Número de estabelecimentos agropecuários por telefone, e-mail e internet - resultados preliminares 2017											
data_Censo_6643 <- read_excel("Dados/Raw/Censo - 2017/tabela6643.xlsx", col_types = "text")
data_Censo_6643[data_Censo_6643=="-"]<-0
data_Censo_6643[data_Censo_6643=="X"]<-NA
data_Censo_6643[, c(3:14)] <- sapply(data_Censo_6643[, c(3:14)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6643[, -c(2,3,5,7,9,11,13)], by = "Cód.")
rm(data_Censo_6643)


#Tabela 6647 - Número de estabelecimentos agropecuários por sexo, alfabetização, idade e cor ou raça do produtor - resultados preliminares 2017														
data_Censo_6647 <- read_excel("Dados/Raw/Censo - 2017/tabela6647.xlsx", col_types = "text")
data_Censo_6647[data_Censo_6647=="-"]<-0
data_Censo_6647[data_Censo_6647=="X"]<-NA
data_Censo_6647[data_Censo_6647==".."]<-NA
data_Censo_6647[data_Censo_6647=="..."]<-NA
data_Censo_6647[, c(3:27)] <- sapply(data_Censo_6647[, c(3:27)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6647[, -c(2,3,4,6,8,10,12,14,16,18,20,22,24,26)], by = "Cód.")
rm(data_Censo_6647)


#Tabela 6649 - Número de estabelecimentos agropecuários por residência, finalidade da produção e DAP - resultados preliminares 2017																
data_Censo_6649 <- read_excel("Dados/Raw/Censo - 2017/tabela6649.xlsx", col_types = "text")
data_Censo_6649[data_Censo_6649=="-"]<-0
data_Censo_6649[data_Censo_6649=="X"]<-NA
data_Censo_6649[, c(3:28)] <- sapply(data_Censo_6649[, c(3:28)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6649[, -c(2,3,5,7,9,11,13,15,17,19,21,23,25,27)], by = "Cód.")
rm(data_Censo_6649)


#Tabela 6650 - Número de estabelecimentos agropecuários por forma de obtenção das terras - resultados preliminares 2017													
data_Censo_6650 <- read_excel("Dados/Raw/Censo - 2017/tabela6650.xlsx")
data_Censo_6650[data_Censo_6650=="-"]<-0
data_Censo_6650[data_Censo_6650=="X"]<-NA
data_Censo_6650[, c(3:24)] <- sapply(data_Censo_6650[, c(3:24)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6650[, -c(2,3,5,7,9,11,13,15,17,19,21,23)], by = "Cód.")
rm(data_Censo_6650)


#Tabela 6651 - Número de estabelecimentos agropecuários por associação, uso de energia elétrica, orientação técnica e obtenção de informação - resultados preliminares 2017																												
data_Censo_6651 <- read_excel("Dados/Raw/Censo - 2017/tabela6651.xlsx", col_types = "text")
data_Censo_6651[data_Censo_6651=="-"]<-0
data_Censo_6651[data_Censo_6651=="X"]<-NA
data_Censo_6651[, c(3:48)] <- sapply(data_Censo_6651[, c(3:48)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6651[, -c(2,seq(3,48,2))], by = "Cód.")
rm(data_Censo_6651)


#Tabela 6652 - Número de estabelecimentos agropecuários por uso de agricultura orgânica - resultados preliminares 2017							
data_Censo_6652 <- read_excel("Dados/Raw/Censo - 2017/tabela6652.xlsx")
data_Censo_6652[data_Censo_6652=="-"]<-0
data_Censo_6652[data_Censo_6652=="X"]<-NA
data_Censo_6652[, c(3:12)] <- sapply(data_Censo_6652[, c(3:12)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6652[, -c(2,seq(3,12,2))], by = "Cód.")
rm(data_Censo_6652)


#Tabela 6655 - Número de estabelecimentos agropecuários por nascentes, rios/riachos, poços e cisternas - resultados preliminares 2017													
data_Censo_6655 <- read_excel("Dados/Raw/Censo - 2017/tabela6655.xlsx", col_types = "text")
data_Censo_6655[data_Censo_6655=="-"]<-0
data_Censo_6655[data_Censo_6655=="X"]<-NA
data_Censo_6655[, c(3:24)] <- sapply(data_Censo_6655[, c(3:24)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6655[, -c(2,seq(3,24,2))], by = "Cód.")
rm(data_Censo_6655)


#Tabela 6658 - Número de estabelecimentos agropecuários por financiamentos/empréstimos - resultados preliminares 2017																
data_Censo_6658 <- read_excel("Dados/Raw/Censo - 2017/tabela6658.xlsx", col_types = "text")
data_Censo_6658[data_Censo_6658=="-"]<-0
data_Censo_6658[data_Censo_6658=="X"]<-NA
data_Censo_6658[, c(3:30)] <- sapply(data_Censo_6658[, c(3:30)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6658[, -c(2,seq(3,30,2))], by = "Cód.")
rm(data_Censo_6658)


#Tabela 6707 - Número de estabelecimentos agropecuários, por associação do produtor à cooperativa e/ou à entidade de classe, sexo do produtor, escolaridade do produtor, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017																
#Alfabetização
data_Censo_6707_2 <- read_excel("Dados/Raw/Censo - 2017/tabela6707 (1).xlsx", col_types = "text")
data_Censo_6707_2[data_Censo_6707_2=="-"]<-0
data_Censo_6707_2[data_Censo_6707_2=="X"]<-NA
data_Censo_6707_2[, c(3:30)] <- sapply(data_Censo_6707_2[, c(3:30)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6707_2[, -c(2,seq(3,30,2))], by = "Cód.")
rm(data_Censo_6707_2)


#Tabela 6707 - Número de estabelecimentos agropecuários, por associação do produtor à cooperativa e/ou à entidade de classe, sexo do produtor, escolaridade do produtor, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017										
#Condição legal do produtor
data_Censo_6707 <- read_excel("Dados/Raw/Censo - 2017/tabela6707.xlsx", col_types = "text")
data_Censo_6707[data_Censo_6707=="-"]<-0
data_Censo_6707[data_Censo_6707=="X"]<-NA
data_Censo_6707[, c(3:18)] <- sapply(data_Censo_6707[, c(3:18)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6707[, -c(2,seq(3,18,2))], by = "Cód.")
rm(data_Censo_6707)


#Tabela 6709 - Número de estabelecimentos agropecuários, por existência de energia elétrica, sexo do produtor, escolaridade do produtor, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017				
data_Censo_6709 <- read_excel("Dados/Raw/Censo - 2017/tabela6709.xlsx", col_types = "text")
data_Censo_6709[data_Censo_6709=="-"]<-0
data_Censo_6709[data_Censo_6709=="X"]<-NA
data_Censo_6709[, c(3:6)] <- sapply(data_Censo_6709[, c(3:6)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6709[, -c(2,seq(3,6,2))], by = "Cód.")
rm(data_Censo_6709)

#Tabela 6710 - Número de estabelecimentos agropecuários, Área dos estabelecimentos agropecuários, por condição legal das terras, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017																										
data_Censo_6710 <- read_excel("Dados/Raw/Censo - 2017/tabela6710.xlsx", col_types = "text")
data_Censo_6710[data_Censo_6710=="-"]<-0
data_Censo_6710[data_Censo_6710=="X"]<-NA
data_Censo_6710[data_Censo_6710==".."]<-NA
data_Censo_6710[data_Censo_6710=="..."]<-NA
data_Censo_6710[, c(3:14)] <- sapply(data_Censo_6710[, c(3:14)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6710[, -c(2,seq(3,14,2))], by = "Cód.")
rm(data_Censo_6710)


#Tabela 6790 - Número de estabelecimentos agropecuários por classes de idade do produtor - resultados preliminares 2017																		
data_Censo_6790 <- read_excel("Dados/Raw/Censo - 2017/tabela6790.xlsx", col_types = "text")
data_Censo_6790[data_Censo_6790=="-"]<-0
data_Censo_6790[data_Censo_6790=="X"]<-NA
data_Censo_6790[, c(3:8)] <- sapply(data_Censo_6790[, c(3:8)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6790[, -c(2)], by = "Cód.")
rm(data_Censo_6790)





