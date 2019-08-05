library(readxl)
library(dplyr)

#Base de Municípiops do IBGE. Pegaremos daqui o código do Município para fazer algo mais simples e eficiente do que 
#comparar strings.
municipios <- read_xls("./Dados/Auxiliares/DTB_BRASIL_MUNICIPIO.xls",
                       col_types = c(
                         "text", #UF
                         "text", #Nome_UF
                         "text", #Sigla_UF
                         "text", #Mesorregião Geográfica
                         "text", #Nome_Mesorregião
                         "numeric", #Microrregião Geográfica
                         "text", #Nome_Microrregião
                         "text", #Município
                         "numeric", #Código Município Completo
                         "text", #Nome_Município
                         "text" #Nome_Município_Maiuscula 
                       )) 
names(municipios)[names(municipios)=="Código Município Completo"] <- "Cod_Municipio"
names(municipios)[names(municipios)=="Microrregião Geográfica"] <- "Cod_Microrregiao"


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


#Base PSR 2016
data_PSR <- read_xlsx("./Dados/Raw/PSR - 2016.xlsx",
                      col_types = c(
                        "text", #NM_RAZAO_SOCIAL
                        "skip", #ID_PROPOSTA
                        "date", #DT_PROPOSTA
                        "date", #DT_INICIO_VIGENCIA
                        "date", #DT_FIM_VIGENCIA
                        "skip", #NR_APOLICE
                        "date", #DT_APOLICE
                        "skip", #NR_DOCUMENTO_SEGURADO
                        "skip", #NM_SEGURADO
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
                        "skip", #NR_GRAU_LAT
                        "skip", #NR_MIN_LAT
                        "skip", #NR_SEG_LAT
                        "skip", #LATITUDE
                        "skip", #NR_GRAU_LONG
                        "skip", #NR_MIN_LONG
                        "skip", #NR_SEG_LONG
                        "skip", #LONGITUDE
                        "skip"  #CD_PROCESSO_SUSEP
                    ))


#Arrumando as Datas
data_PSR$DT_PROPOSTA <- as.Date(data_PSR$DT_PROPOSTA , origin = "1899-12-30")
data_PSR$DT_INICIO_VIGENCIA <- as.Date(data_PSR$DT_INICIO_VIGENCIA , origin = "1899-12-30")
data_PSR$DT_FIM_VIGENCIA <- as.Date(data_PSR$DT_FIM_VIGENCIA , origin = "1899-12-30")
data_PSR$DT_APOLICE <- as.Date(data_PSR$DT_APOLICE , origin = "1899-12-30")

#Nome do Município maiúscula - Normalizando para cruzar com a base do IBGE 
data_PSR$NM_MUNICIPIO_PROPRIEDADE <- toupper(data_PSR$NM_MUNICIPIO_PROPRIEDADE)


#Agora arrumando os municípios que estão errados (grafia, acento, etc.) ou que na verdade são nomes de distritos. Neste último caso,
#o nome foi trocado para o do município.
data_PSR <- data_PSR %>% mutate(MUNICIPIO_CORRIGIDO = 
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
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA RITA DO OESTE" & SG_UF_PROPRIEDADE == "PR" ~ "TERRA ROXA",
                                    NM_MUNICIPIO_PROPRIEDADE == "MALU" & SG_UF_PROPRIEDADE == "PR" ~ "TERRA BOA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SELVA" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
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
                                    NM_MUNICIPIO_PROPRIEDADE == "LUIS EDUARDO MAGALHAES" & SG_UF_PROPRIEDADE == "BA" ~ "LUÍS EDUARDO MAGALHÃES",
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
                                    NM_MUNICIPIO_PROPRIEDADE == "DUERÊ" & SG_UF_PROPRIEDADE == "TO" ~ "DUERÉ",
                                    NM_MUNICIPIO_PROPRIEDADE == "PARANAITÁ" & SG_UF_PROPRIEDADE == "MT" ~ "PARANAÍTA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO LUIZ" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA OLIVA" & SG_UF_PROPRIEDADE == "RS" ~ "SÃO FRANCISCO DE PAULA",
                                    NM_MUNICIPIO_PROPRIEDADE == "BARRO VERMELHO" & SG_UF_PROPRIEDADE == "RS" ~ "CACHOEIRA DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO PEDRO" & SG_UF_PROPRIEDADE == "PR" ~ "SÃO PEDRO DO PARANÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO PEDRO" & SG_UF_PROPRIEDADE == "RS" ~ "SÃO PEDRO DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "TUIUTI" & SG_UF_PROPRIEDADE == "RS" ~ "BENTO GONÇALVES",
                                    NM_MUNICIPIO_PROPRIEDADE == "RETIRO GRANDE" & SG_UF_PROPRIEDADE == "PR" ~ "CAMPO LARGO",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA NOVA" & SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "GLÓRIA" & SG_UF_PROPRIEDADE == "RS" ~ "PORTO ALEGRE",
                                    NM_MUNICIPIO_PROPRIEDADE == "CARIPARE" & SG_UF_PROPRIEDADE == "BA" ~ "RIACHÃO DAS NEVES",
                                    NM_MUNICIPIO_PROPRIEDADE == "BELA VISTA" & SG_UF_PROPRIEDADE == "RS" ~ "PORTO ALEGRE",
                                    NM_MUNICIPIO_PROPRIEDADE == "MARAVILHA" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "TAIM" & SG_UF_PROPRIEDADE == "RS" ~ "RIO GRANDE",
                                    TRUE ~ NM_MUNICIPIO_PROPRIEDADE)
                                )

data_PSR[data_PSR$NM_MUNICIPIO_PROPRIEDADE == "SELVA",]


data_Proagro <- read_xlsx("./Dados/Raw/PLANILHA_CONTRATACAO_PROAGRO 2016-2017 final até JUNHO 17.xlsx",
                      col_types = c(
                        "text", #MÊS EMISSAO
                        "skip", #SAFRA 
                        "skip", #FINALIDADE
                        "text", #PROGRAMA
                        "text", #SEGURO
                        "text", #UF
                        "text", #MUNICIPIO
                        "text", #PRODUTO
                        "skip", #FAIXA VALOR
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
                                    TRUE ~ MUNICIPIO))



#Padronizando Produtos
data_PSR <- data_PSR %>% 
            left_join(
              produtos_padronizados %>% select(PSR,Produto_Padronizado), 
              by = c("NM_CULTURA_GLOBAL" = "PSR")
            ) 
data_Proagro <- data_Proagro %>% 
            left_join(
              produtos_padronizados %>% select(Proagro,Produto_Padronizado), 
              by =c("PRODUTO" = "Proagro")
            )

#Adicionando código Município e Microrregião
data_PSR <- data_PSR %>% 
            left_join(
              municipios %>% select(Sigla_UF,Nome_Município_Maiuscula,Cod_Microrregiao,Cod_Municipio),
              by =c("SG_UF_PROPRIEDADE" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Município_Maiuscula")
            )


data_Proagro <- data_Proagro %>% 
  left_join(
    municipios %>% select(Sigla_UF,Nome_Município_Maiuscula,Cod_Microrregiao,Cod_Municipio),
    by =c("UF" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Município_Maiuscula")
  )




