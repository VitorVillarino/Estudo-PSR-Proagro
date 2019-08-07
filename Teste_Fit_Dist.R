library(fitdistrplus)


test_fit <- data_PSR  %>%  filter(Produto_Padronizado == "SOJA", MUNICIPIO_CORRIGIDO == "TOLEDO")
test_fit$test <- test_fit$VL_PREMIO_LIQUIDO/test_fit$NR_AREA_TOTAL
test_fit <- test_fit[!is.na(test_fit$test),]
test_fit <- test_fit[!is.infinite(test_fit$test),]

test_qto <- test_fit %>% group_by(SG_UF_PROPRIEDADE,MUNICIPIO_CORRIGIDO) %>% summarise(n())
test_qto
View(test_qto)









range01 <- function(x){(x-min(x)+0.001)/(max(x)-min(x)+0.002)}

test_fit$test <- range01(test_fit$test)

plotdist(test_fit$test, histo = TRUE, demp = TRUE,)
x <- descdist(test_fit$test, boot = 1000)
x

View(test_fit)

FIT <- fitdist(test_fit$test,"gamma")
?fitdist
FIT
plot(FIT)
summary(FIT)

