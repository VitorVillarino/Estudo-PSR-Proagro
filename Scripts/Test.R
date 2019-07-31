library(tidyverse)
library(readxl)
library(cluster)
library(factoextra)
library(fpc)
library(NbClust)
library(esquisse)
library(forcats)
library(summarytools)


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
                                     "text"  #Padronizado
                                   ))


#Base PSR 2016
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

#Nome do Município maiúscula - Normalizando para cruzar com a base do IBGE 
data_PSR$NM_MUNICIPIO_PROPRIEDADE <- toupper(data_PSR$NM_MUNICIPIO_PROPRIEDADE)

#Agora arrumando os municípios que estão errados (grafia, acento, etc.) ou que na verdade são nomes de distritos. Neste último caso,
#o nome foi trocado para o do município.
data_PSR <- data_PSR %>% mutate(NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO = 
                                  case_when( 
                                    NM_MUNICIPIO_PROPRIEDADE == "FOZ DO IGUACU" & SG_UF_PROPRIEDADE == "PR" ~ "FOZ DO IGUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "CANDEIA" & SG_UF_PROPRIEDADE == "PR" ~ "MARIPÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAMBE" & SG_UF_PROPRIEDADE == "PR" ~ "CAMBÉ",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITABERA" & SG_UF_PROPRIEDADE == "SP" ~ "ITABERÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAPAO BONITO" & SG_UF_PROPRIEDADE == "SP" ~ "CAPÃO BONITO",
                                    NM_MUNICIPIO_PROPRIEDADE == "MAUA DA SERRA" & SG_UF_PROPRIEDADE == "PR" ~ "MAUÁ DA SERRA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SENGES" & SG_UF_PROPRIEDADE == "PR" ~ "SENGÉS",
                                    NM_MUNICIPIO_PROPRIEDADE == "JAGUARIAIVA" & SG_UF_PROPRIEDADE == "PR" ~ "JAGUARIAÍVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITARARE" & SG_UF_PROPRIEDADE == "SP" ~ "ITARARÉ",
                                    NM_MUNICIPIO_PROPRIEDADE == "QUEDAS DO IGUACU" & SG_UF_PROPRIEDADE == "PR" ~ "QUEDAS DO IGUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "ARAUCARIA" & SG_UF_PROPRIEDADE == "PR" ~ "ARAUCÁRIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAPAO ALTO" & SG_UF_PROPRIEDADE == "SC" ~ "CAPÃO ALTO",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOM JESUS" & SG_UF_PROPRIEDADE == "GO" ~ "BOM JESUS DE GOIÁS",
                                    NM_MUNICIPIO_PROPRIEDADE == "UBERLANDIA" & SG_UF_PROPRIEDADE == "MG" ~ "UBERLÂNDIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "INDIANOPOLIS" & SG_UF_PROPRIEDADE == "MG" ~ "INDIANÓPOLIS",
                                    NM_MUNICIPIO_PROPRIEDADE == "BRAGANCA PAULISTA" & SG_UF_PROPRIEDADE == "SP" ~ "BRAGANÇA PAULISTA",
                                    NM_MUNICIPIO_PROPRIEDADE == "MÁGDA" & SG_UF_PROPRIEDADE == "SP" ~ "MAGDA",
                                    NM_MUNICIPIO_PROPRIEDADE == "BIRIGÜI" & SG_UF_PROPRIEDADE == "SP" ~ "BIRIGUI",
                                    NM_MUNICIPIO_PROPRIEDADE == "ARACOIABA DA SERRA" & SG_UF_PROPRIEDADE == "SP" ~ "ARAÇOIABA DA SERRA",
                                    NM_MUNICIPIO_PROPRIEDADE == "MOGI GUACU" & SG_UF_PROPRIEDADE == "SP" ~ "MOGI GUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "VARZEA DA PALMA" & SG_UF_PROPRIEDADE == "MG" ~ "VÁRZEA DA PALMA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVA VICOSA" & SG_UF_PROPRIEDADE == "BA" ~ "NOVA VIÇOSA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CACADOR" & SG_UF_PROPRIEDADE == "SC" ~ "CAÇADOR",
                                    NM_MUNICIPIO_PROPRIEDADE == "BAIRRO LIMOEIRO" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "AMAMBAÍ" & SG_UF_PROPRIEDADE == "MS" ~ "AMAMBAI",
                                    NM_MUNICIPIO_PROPRIEDADE == "INDÁPOLIS" & SG_UF_PROPRIEDADE == "MS" ~ "DOURADOS",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOSSA SENHORA DA CANDELÁRIA" & SG_UF_PROPRIEDADE == "PR" ~ "BANDEIRANTES",
                                    NM_MUNICIPIO_PROPRIEDADE == "SEDE ALVORADA" & SG_UF_PROPRIEDADE == "PR" ~ "CASCAVEL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOÃO D'OESTE" & SG_UF_PROPRIEDADE == "PR" ~ "CASCAVEL",
                                    NM_MUNICIPIO_PROPRIEDADE == "OURO PRETO" & SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "TAQUARUNA" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO CLEMENTE" & SG_UF_PROPRIEDADE == "PR" ~ "SANTA HELENA",
                                    NM_MUNICIPIO_PROPRIEDADE == "FLORÍNIA" & SG_UF_PROPRIEDADE == "SP" ~ "FLORÍNEA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PIRAPÓ" & SG_UF_PROPRIEDADE == "PR" ~ "APUCARANA",
                                    NM_MUNICIPIO_PROPRIEDADE == "LERROVILLE" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "DEZ DE MAIO" & SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAETANO MENDES" & SG_UF_PROPRIEDADE == "PR" ~ "TIBAGI",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA RITA DOESTE" & SG_UF_PROPRIEDADE == "PR" ~ "TERRA ROXA",
                                    NM_MUNICIPIO_PROPRIEDADE == "MALU" & SG_UF_PROPRIEDADE == "PR" ~ "TERRA BOA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SELVA" & SG_UF_PROPRIEDADE == "PR" ~ "OLÍMPIO LOPES",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOA VISTA" & SG_UF_PROPRIEDADE == "PR" ~ "PRESIDENTE CASTELO BRANCO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO LUIZ DO PARAITINGA" & SG_UF_PROPRIEDADE == "SP" ~ "SÃO LUÍS DO PARAITINGA",
                                    NM_MUNICIPIO_PROPRIEDADE == "RESTINGA SECA" & SG_UF_PROPRIEDADE == "RS" ~ "SANTO ÂNGELO",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITAÓCA" & SG_UF_PROPRIEDADE == "SP" ~ "ITAOCA",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITABOA" & SG_UF_PROPRIEDADE == "SP" ~ "RIBEIRÃO BRANCO",
                                    NM_MUNICIPIO_PROPRIEDADE == "APIAÍ-MIRIM" & SG_UF_PROPRIEDADE == "SP" ~ "CAPÃO BONITO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA LÚCIA DO PIAÍ" & SG_UF_PROPRIEDADE == "RS" ~ "CAXIAS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SILVEIRA" & SG_UF_PROPRIEDADE == "RS" ~ "SÃO JOSÉ DOS AUSENTES",
                                    NM_MUNICIPIO_PROPRIEDADE == "FAZENDA SOUZA" & SG_UF_PROPRIEDADE == "RS" ~ "CAXIAS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "PAIQUERÊ" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOSSA SENHORA DA APARECIDA" & SG_UF_PROPRIEDADE == "PR" ~ "ROLÂNDIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA SÃO JOÃO" & SG_UF_PROPRIEDADE == "RS" ~ "CRUZ ALTA",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA IPIRANGA" & SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "COMANDAI" & SG_UF_PROPRIEDADE == "RS" ~ "SANTO ÂNGELO",
                                    NM_MUNICIPIO_PROPRIEDADE == "FLUVIÓPOLIS" & SG_UF_PROPRIEDADE == "PR" ~ "SÃO MATEUS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "HOLAMBRA II" & SG_UF_PROPRIEDADE == "SP" ~ "HOLAMBRA",
                                    NM_MUNICIPIO_PROPRIEDADE == "FELPUDO" & SG_UF_PROPRIEDADE == "PR" ~ "CAMPO LARGO",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAMPINA DE FORA" & SG_UF_PROPRIEDADE == "SP" ~ "RIBEIRÃO BRANCO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTANA DO LIVRAMENTO" & SG_UF_PROPRIEDADE == "RS" ~ "SANT'ANA DO LIVRAMENTO",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITAHUM" & SG_UF_PROPRIEDADE == "MS" ~ "DOURADOS",
                                    NM_MUNICIPIO_PROPRIEDADE == "LUIS EDUARDO MAGALHAES" & SG_UF_PROPRIEDADE == "BA" ~ "LUIS EDUARDO MAGALHÃES",
                                    NM_MUNICIPIO_PROPRIEDADE == "CHAPADAO DO SUL" & SG_UF_PROPRIEDADE == "MS" ~ "CHAPADÃO DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "GOIANAPOLIS" & SG_UF_PROPRIEDADE == "GO" ~ "GOIANÁPOLIS",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAMAQUA" & SG_UF_PROPRIEDADE == "RS" ~ "CAMAQUÃ",
                                    NM_MUNICIPIO_PROPRIEDADE == "UNIAO DA VITORIA" & SG_UF_PROPRIEDADE == "PR" ~ "UNIÃO DA VITÓRIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVO HORIZONTE" & SG_UF_PROPRIEDADE == "PR" ~ "MARECHAL CÂNDIDO RONDON",
                                    NM_MUNICIPIO_PROPRIEDADE == "MARGARIDA" & SG_UF_PROPRIEDADE == "PR" ~ "MARECHAL CÂNDIDO RONDON",
                                    NM_MUNICIPIO_PROPRIEDADE == "IGUIPORÃ" & SG_UF_PROPRIEDADE == "PR" ~ "MARECHAL CÂNDIDO RONDON",
                                    NM_MUNICIPIO_PROPRIEDADE == "JUTÍ" & SG_UF_PROPRIEDADE == "MS" ~ "JUTI",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUARAVERA" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "AMANDINA" & SG_UF_PROPRIEDADE == "MS" ~ "IVINHEMA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PÉROLA INDEPENDENTE" & SG_UF_PROPRIEDADE == "PR" ~ "MARIPÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "ENTRE RIOS" & SG_UF_PROPRIEDADE == "PR" ~ "GUARAPUAVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO LOURENÇO" & SG_UF_PROPRIEDADE == "PR" ~ "CIANORTE",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVA LOURDES" & SG_UF_PROPRIEDADE == "PR" ~ "SÃO JOÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA SOCORRO" & SG_UF_PROPRIEDADE == "PR" ~ "GUARAPUAVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTO ANTÔNIO DO RIO VERDE" & SG_UF_PROPRIEDADE == "GO" ~ "CATALÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOSÉ DAS LARANJEIRAS" & SG_UF_PROPRIEDADE == "SP" ~ "MARACAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUAIRACÁ" & SG_UF_PROPRIEDADE == "PR" ~ "GUARAPUAVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVO SARANDI" & SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "TELEMACO BORBA" & SG_UF_PROPRIEDADE == "PR" ~ "TELÊMACO BORBA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SAO JOSE DOS PINHAIS" & SG_UF_PROPRIEDADE == "PR" ~ "SÃO JOSÉ DOS PINHAIS",
                                    NM_MUNICIPIO_PROPRIEDADE == "CRIÚVA" & SG_UF_PROPRIEDADE == "RS" ~ "CAXIAS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA CECILIA" & SG_UF_PROPRIEDADE == "SC" ~ "SANTA CECÍLIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA VITÓRIA" & SG_UF_PROPRIEDADE == "PR" ~ "GUARAPUAVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PALMEIRINHA" & SG_UF_PROPRIEDADE == "PR" ~ "GUARAPUAVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAPANÉ" & SG_UF_PROPRIEDADE == "RS" ~ "CACHOEIRA DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "FERREIRA" & SG_UF_PROPRIEDADE == "RS" ~ "CACHOEIRA DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "UVAIA" & SG_UF_PROPRIEDADE == "PR" ~ "PONTA GROSSA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOSÉ DA RESERVA" & SG_UF_PROPRIEDADE == "RS" ~ "SANTA CRUZ DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "ALTO DO AMPARO" & SG_UF_PROPRIEDADE == "PR" ~ "TIBAGI",
                                    NM_MUNICIPIO_PROPRIEDADE == "JANSEN" & SG_UF_PROPRIEDADE == "RS" ~ "FARROUPILHA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVA SARDENHA" & SG_UF_PROPRIEDADE == "RS" ~ "FARROUPILHA",
                                    NM_MUNICIPIO_PROPRIEDADE == "GRAMADINHO" & SG_UF_PROPRIEDADE == "SP" ~ "ITAPETININGA",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUAIPORÃ" & SG_UF_PROPRIEDADE == "PR" ~ "CAFEZAL DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "LAGEADO DE ARAÇAÍBA" & SG_UF_PROPRIEDADE == "SP" ~ "APIAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "ÁGUAS CLARAS" & SG_UF_PROPRIEDADE == "RS" ~ "VIAMÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "MONTE ALVERNE" & SG_UF_PROPRIEDADE == "RS" ~ "SANTA CRUZ DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "ENGENHEIRO MAIA" & SG_UF_PROPRIEDADE == "SP" ~ "ITABERÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "CERRO DO ROQUE" & SG_UF_PROPRIEDADE == "RS" ~ "BUTIÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUARAGI" & SG_UF_PROPRIEDADE == "PR" ~ "PONTA GROSSA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÍTIO GRANDE" & SG_UF_PROPRIEDADE == "BA" ~ "SÃO DESIDÉRIO",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUAJUVIRA" & SG_UF_PROPRIEDADE == "PR" ~ "ARAUCÁRIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVO SOBRADINHO" & SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "WESTFALIA" & SG_UF_PROPRIEDADE == "RS" ~ "WESTFÁLIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "OURILÂNDIA" & SG_UF_PROPRIEDADE == "PR" ~ "BARBOSA FERRAZ",
                                    NM_MUNICIPIO_PROPRIEDADE == "BATEIAS" & SG_UF_PROPRIEDADE == "PR" ~ "CAMPO LARGO",
                                    NM_MUNICIPIO_PROPRIEDADE == "PERICÓ" & SG_UF_PROPRIEDADE == "SC" ~ "SÃO JOAQUIM",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA IZABEL" & SG_UF_PROPRIEDADE == "SC" ~ "SÃO JOAQUIM",
                                    NM_MUNICIPIO_PROPRIEDADE == "TAPINAS" & SG_UF_PROPRIEDADE == "SP" ~ "ITÁPOLIS",
                                    NM_MUNICIPIO_PROPRIEDADE == "DORIZON" & SG_UF_PROPRIEDADE == "PR" ~ "MALLET",
                                    NM_MUNICIPIO_PROPRIEDADE == "VALE DOS VINHEDOS" & SG_UF_PROPRIEDADE == "RS" ~ "BENTO GONÇALVES",
                                    NM_MUNICIPIO_PROPRIEDADE == "FARIA LEMOS" & SG_UF_PROPRIEDADE == "RS" ~ "BENTO GONÇALVES",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVA MILANO" & SG_UF_PROPRIEDADE == "RS" ~ "FARROUPILHA",
                                    NM_MUNICIPIO_PROPRIEDADE == "VICENTINÓPOLIS" & SG_UF_PROPRIEDADE == "SP" ~ "SANTO ANTÔNIO DO ARACANGUÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "IPOMÉIA" & SG_UF_PROPRIEDADE == "SC" ~ "RIO DAS ANTAS",
                                    NM_MUNICIPIO_PROPRIEDADE == "PALMITÓPOLIS" & SG_UF_PROPRIEDADE == "PR" ~ "NOVA AURORA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PASSO DO VERDE" & SG_UF_PROPRIEDADE == "RS" ~ "SANTA MARIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "LUIS EDUARDO MAGALHÃES" & SG_UF_PROPRIEDADE == "BA" ~ "LUÍS EDUARDO MAGALHÃES",
                                    NM_MUNICIPIO_PROPRIEDADE == "JORDÃO" & SG_UF_PROPRIEDADE == "PR" ~ "FOZ DO JORDÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA SECA" & SG_UF_PROPRIEDADE == "RS" ~ "CAXIAS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "ARARANGUA" & SG_UF_PROPRIEDADE == "SC" ~ "ARARANGUÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "VENANCIO AIRES" & SG_UF_PROPRIEDADE == "RS" ~ "VENÂNCIO AIRES",
                                    NM_MUNICIPIO_PROPRIEDADE == "RIBEIRAO PRETO" & SG_UF_PROPRIEDADE == "SP" ~ "RIBEIRÃO PRETO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SERTAOZINHO" & SG_UF_PROPRIEDADE == "SP" ~ "SERTÃOZINHO",
                                    NM_MUNICIPIO_PROPRIEDADE == "JACAREI" & SG_UF_PROPRIEDADE == "SP" ~ "JACAREÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "CRISPIM JAQUES" & SG_UF_PROPRIEDADE == "MG" ~ "TEÓFILO OTONI",
                                    NM_MUNICIPIO_PROPRIEDADE == "VESPASIANO CORREA" & SG_UF_PROPRIEDADE == "RS" ~ "VESPASIANO CORRÊA",
                                    NM_MUNICIPIO_PROPRIEDADE == "COUTO DE MAGALHÃES" & SG_UF_PROPRIEDADE == "TO" ~ "COUTO MAGALHÃES",
                                    NM_MUNICIPIO_PROPRIEDADE == "PEDRO LUSTOSA" & SG_UF_PROPRIEDADE == "PR" ~ "RESERVA DO IGUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "ARAÇAÍBA" & SG_UF_PROPRIEDADE == "SP" ~ "APIAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "PRIMAVERA" & SG_UF_PROPRIEDADE == "SP" ~ "ROSANA",
                                    NM_MUNICIPIO_PROPRIEDADE == "TUBARAO" & SG_UF_PROPRIEDADE == "SC" ~ "TUBARÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "ICARA" & SG_UF_PROPRIEDADE == "SC" ~ "IÇARA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PÓLO PETROQUÍMICO DE TRIUNFO" & SG_UF_PROPRIEDADE == "RS" ~ "TRIUNFO",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOCAINA" & SG_UF_PROPRIEDADE == "PR" ~ "PONTA GROSSA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO VALÉRIO DA NATIVIDADE" & SG_UF_PROPRIEDADE == "TO" ~ "SÃO VALÉRIO",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOCAÍNA DO SUL" & SG_UF_PROPRIEDADE == "SC" ~ "BOCAINA DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "BIRITIBA-MIRIM" & SG_UF_PROPRIEDADE == "SP" ~ "BIRITIBA MIRIM",
                                    NM_MUNICIPIO_PROPRIEDADE == "ESPIGÃO DO OESTE" & SG_UF_PROPRIEDADE == "RO" ~ "ESPIGÃO D'OESTE",
                                    NM_MUNICIPIO_PROPRIEDADE == "CARIPARE" & SG_UF_PROPRIEDADE == "BA" ~ "CARIPARÉ",
                                    NM_MUNICIPIO_PROPRIEDADE == "DUERÊ" & SG_UF_PROPRIEDADE == "TO" ~ "DUERÉ",
                                    NM_MUNICIPIO_PROPRIEDADE == "PARANAITÁ" & SG_UF_PROPRIEDADE == "MT" ~ "PARANAÍTA",
                                    TRUE ~ NM_MUNICIPIO_PROPRIEDADE)
                                )

data_Proagro <- read_xlsx("./Dados/Raw/PLANILHA_CONTRATACAO_PROAGRO 2016-2017 final até JUNHO 17.xlsx",
                      col_types = c(
                        "text", #MÊS EMISSAO
                        "text", #SAFRA 
                        "text", #FINALIDADE
                        "text", #PROGRAMA
                        "text", #SEGURO
                        "text", #UF
                        "text", #MUNICIPIO
                        "text", #PRODUTO
                        "text", #FAIXA VALOR
                        "numeric", #QUANTIDADE
                        "numeric", #AREA
                        "numeric", # VALOR_FINANCIADO 
                        "numeric", # VALOR_INVESTIMENTO 
                        "numeric", # RECURSOS_PROPRIOS 
                        "numeric", # RENDA_MINIMA 
                        "numeric", # VALOR_TOTAL 
                        "numeric", # VALOR_ADICIONAL 
                        "numeric" # RBE 
                      ))

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
                                    TRUE ~ MUNICIPIO)
                                  )

# Tabela 6615 - Número de estab2
# por produtos da lavoura temporária - resultados preliminares 2017
# data_Censo_6615 <- read.csv("./Dados/Raw/Censo - 2017/tabela6615.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM')


# Tabela 6616 - Número de estabelecimentos agropecuários e Número de pés existentes, por produtos da 
# lavoura permanente - resultados preliminares 2017
# data_Censo_6616 <- read.csv("./Dados/Raw/Censo - 2017/tabela6616.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM')


# Tabela 6618 - Número de estabelecimentos agropecuários e Quantidade produzida, por produtos 
# da agroindústria rural - resultados preliminares 2017
# data_Censo_6618 <- read.csv("./Dados/Raw/Censo - 2017/tabela6618.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM')


# Tabela 6619 - Número de estabelecimentos agropecuários e Quantidade produzida, por produtos 
# da horticultura - resultados preliminares 2017
# data_Censo_6619 <- read.csv("./Dados/Raw/Censo - 2017/tabela6619.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM')




# municipios_nao_encontrados_PSR <- anti_join(data_PSR, municipios, by = c("SG_UF_PROPRIEDADE" = "Sigla_UF", "NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO" = "Nome_Município_Maiuscula"))  %>%
#   left_join(distritos, by =c("SG_UF_PROPRIEDADE" = "Sigla_UF", "NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO" = "Nome_Distrito_Maiuscula")) %>%
#   distinct(NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO, SG_UF_PROPRIEDADE, Município, Nome_Município, Nome_Distrito)

# municipios_nao_encontrados_Proagro <- anti_join(data_Proagro, municipios, by = c("UF" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Município_Maiuscula"))  %>%
#   left_join(distritos, by =c("UF" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Distrito_Maiuscula")) %>%
#   distinct(MUNICIPIO, UF, Município, Nome_Município, Nome_Distrito)
# write.xlsx(municipios_nao_encontrados_Proagro, file = 'municipios_nao_encontrados.xlsx')




data_PSR <- data_PSR %>% left_join(produtos_padronizados, by =c("NM_CULTURA_GLOBAL" = "PSR")) 
data_Proagro <- data_Proagro %>% left_join(produtos_padronizados, by =c("PRODUTO" = "Proagro"))



#Cultura

#PSR
ggplot(data_PSR) +
  aes(x = fct_rev(fct_infreq(Padronizado))) +
  geom_bar(fill = "#0c4c8a") +
  coord_flip() +
  theme_minimal()

#Proagro
ggplot(data_Proagro) +
  aes(x = fct_rev(fct_infreq(Padronizado))) +
  geom_bar(fill = "#0c4c8a") +
  coord_flip() +
  theme_minimal()




freq(data_PSR$Padronizado, headings = FALSE, plain.ascii = FALSE, style = "rmarkdown",  order = "freq")
with(data_PSR, 
     print(ctable(Padronizado, SG_UF_PROPRIEDADE, prop = 'n')))


## PSR
drivers_PSR <- 
  data_PSR %>% 
  select(
    Padronizado,
    SG_UF_PROPRIEDADE,
    NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO,
    NR_AREA_TOTAL,
    VL_PREMIO_LIQUIDO) %>%
  filter(VL_PREMIO_LIQUIDO <= 300000)

driver_PSR_Soja <- drivers_PSR %>% filter(Padronizado == "SOJA")



corr_M <- driver_PSR_Soja %>% 
        group_by(SG_UF_PROPRIEDADE,NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO) %>% 
        summarise(cor = cor(NR_AREA_TOTAL,VL_PREMIO_LIQUIDO),n = n())
corr_M <- corr_M %>% filter(n >= 10)
dfSummary(corr_M$cor)

ggplot(corr_M) +
 aes(x = cor) +
 geom_histogram(bins = 50L, fill = "#0c4c8a") +
 theme_minimal()


ggplot(driver_PSR_Soja) +
aes(x = NR_AREA_TOTAL, y = VL_PREMIO_LIQUIDO) +
geom_point(size = 1L, colour = "#0c4c8a") +
theme_minimal()



dados_soja_PSR <- driver_PSR_Soja %>% 
  group_by(Padronizado, SG_UF_PROPRIEDADE,NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO) %>% 
  summarise(media = mean(VL_PREMIO_LIQUIDO), 
            desv_pd = sd(VL_PREMIO_LIQUIDO) ,
            n = n())
names(dados_soja_PSR)[names(dados_soja_PSR) == "SG_UF_PROPRIEDADE"] <- "UF"
names(dados_soja_PSR)[names(dados_soja_PSR) == "NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO"] <- "MUNICIPIO"


## Proagro

drivers_proagro <- data_Proagro %>% 
  select(
    Padronizado,
    UF,
    MUNICIPIO_CORRIGIDO,
    QUANTIDADE,
    AREA,
    VALOR_TOTAL)

drivers_proagro_Soja <- drivers_proagro %>% 
                filter(Padronizado == "SOJA") %>% 
                group_by(Padronizado, UF, MUNICIPIO_CORRIGIDO) %>% 
                summarise(QUANTIDADE = sum(QUANTIDADE), 
                          AREA = sum(AREA) ,
                          VALOR_TOTAL = sum(VALOR_TOTAL))
                
drivers_proagro_Soja$AREA_MEDIA <- drivers_proagro_Soja$AREA/drivers_proagro_Soja$QUANTIDADE
drivers_proagro_Soja$VALOR_MEDIA <- drivers_proagro_Soja$VALOR_TOTAL/drivers_proagro_Soja$QUANTIDADE

names(drivers_proagro_Soja)[names(drivers_proagro_Soja) == "MUNICIPIO_CORRIGIDO"] <- "MUNICIPIO"






ggplot(drivers_proagro_Soja) +
  aes(x = AREA_MEDIA, y = VALOR_MEDIA) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  theme_minimal()





## Juntado
juntado <- dados_soja_PSR %>% left_join(drivers_proagro_Soja, by = c("Padronizado","UF","MUNICIPIO"))
juntado$dif_media <- (juntado$media -juntado$VALOR_MEDIA)/juntado$media

ggplot(juntado) +
  aes(x = media, y = VALOR_MEDIA) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
















# Test Cluster - Sem bons resultados - Gower pesa muito variável qualitativa (Municipio e UF)

  
drivers_PSR$Padronizado <- as.factor(drivers_PSR$Padronizado)
drivers_PSR$NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO <- as.factor(drivers_PSR$NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO)
drivers_PSR$SG_UF_PROPRIEDADE <- as.factor(drivers_PSR$SG_UF_PROPRIEDADE)


gower_dist <- daisy(driver_PSR_Soja)

PSR_pam <- pamk(gower_dist, krange = 2:20, diss = T, critout = T )























max_sil <- -99
max_k <-  0
for (k in 2:100){
  clara2 <- clara(drivers_PSR, k, metric = "euclidean", stand = FALSE, samples = 1000,
                  rngR = FALSE, pamLike = TRUE)
  cat(k, " - ", clara2$ silinfo $ avg.width,"\n")
  if(max_sil< clara2$ silinfo $ avg.width) {
    max_sil <-  clara2$ silinfo $ avg.width
    max_k <- k
  }
} 




media_gowler <- daisy(drivers_PSR)
hcluster <- hclust(media_gowler, method = "ward.D2")
fviz_dend(hcluster, main = "cluster")


