data_proagro <- read.csv("./Dados/Raw/CONTRATACAO PROAGRO 20190801.csv", header = TRUE, sep = ',', fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

write_xlsx(data_proagro, "test.xlsx")


library(summarytools)
library(lubridate)
data_PSR$dias_proposta <- as.numeric(as.Date(data_PSR$DT_FIM_VIGENCIA) - as.Date(data_PSR$DT_INICIO_VIGENCIA))
summarytools::descr(data_PSR$dias_proposta)
write_xlsx(data_PSR, "test.xlsx")
cor(data_PSR$dias_proposta,data_PSR$VL_PREMIO_LIQUIDO)
esquisse::esquisser()
data_PSR <- data_PSR[data_PSR$dias_proposta < 1000,]
