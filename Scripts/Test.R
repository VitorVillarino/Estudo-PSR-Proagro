


library(ggplot2)

x <- ggplot(Proagro_soja_mun) +
  aes(x = Premio_Area_Defl) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

ggplot_build(x)$data[[1]]
layer_data(x, 1)







library(ggplot2)

x2 <- ggplot(Proagro_soja_mun) +
 aes(x = Premio_Area, fill = TP_SEGURO) +
 geom_histogram(bins = 30L) +
 scale_fill_hue() +
 theme_minimal() +
 facet_wrap(vars(PROGRAMA))


ggplot_build(x2)$data[[1]]
