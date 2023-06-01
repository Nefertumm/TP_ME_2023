rm(list = ls())
gc()

# Librerías utilizadas.
library("data.table")
library("dplyr")
library("fastDummies")
library("pROC")
library("ggplot2")
library("ggpubr")

datos <- fread("./data/cardio_train.csv", stringsAsFactors = T, sep = ';')

str(datos)

# Convertimos age a años, ya que en días es demasiado engorroso.
datos_modificada <- dplyr::mutate(datos, age = floor(age / 365))
# Quitamos id, ya que nos es irrelevante para el estudio
datos_modificada_2 <- dplyr::select(datos_modificada, -id)

head(datos_modificada_2)

# Revisión de NA
sum(is.na(datos_modificada_2))
# 0 datos faltantes

# Nos aseguramos que nuestro dataset no contenga duplicados
datos_modificada_2 <- datos_modificada_2 %>% distinct()
# Por la diferencia de observaciones, teniamos 3208 datos duplicados.

# Hacemos un summary para ver los rangos de cada una de las variables cuantitativas
variables <- c("age", "height", "weight", "ap_hi", "ap_lo")

sum_data <- datos_modificada_2 %>% select(all_of(variables)) %>% summary()
print(sum_data)

# Vemos un par de irregularidades, como por ejemplo que para la presión sistólica y diastólica observamos
# valores negativos o valores que no tienen sentido como el máximo que presenta.
# Haremos una pequeña limpieza, quitaremos el 2.5% de los cuartiles de cada lado.
limpieza_dataset <- datos_modificada_2 %>% dplyr::filter(between(ap_hi, quantile(ap_hi, 0.025), quantile(ap_hi, 0.975)) &
                                                         between(ap_lo, quantile(ap_lo, 0.025), quantile(ap_lo, 0.975)))

# También observamos que height y weight tienen un mínimo de 55 y de 10 respectivamente...
# No representan valores muy reales, por lo que podemos hacer lo mismo que hicimos anteriormente con ap_hi y ap_lo

limpieza_dataset_2 <- limpieza_dataset %>% dplyr::filter(between(height, quantile(height, 0.025), quantile(height, 0.975)) &
                                                           between(weight, quantile(weight, 0.025), quantile(weight, 0.975)))

# Volvemos a observar los datos
sum_data_limpieza <- limpieza_dataset_2 %>% select(all_of(variables)) %>% summary()
print(sum_data_limpieza)
# Ahora parece ser que los datos están "limpios", procederemos a graficar para verificar

# Por pregunta a un médico, sabemos que la presión diástolica NUNCA puede ser más grande que la presión sistólica.
# Por lo que vamos a remover estos datos.

dataset_limpio <- limpieza_dataset_2 %>% dplyr::filter(ap_lo < ap_hi)

# Boxplots de exploración para cada una de nuestras variables
plot_list <- list()
for (variable in variables)
{
  plot <- ggplot() +
          geom_boxplot(data = dataset_limpio, aes_string(x = "factor(cardio)", y = variable, fill = "factor(cardio)")) +
          labs(title = variable)
  plot_list[[length(plot_list) + 1]] <- plot
}

plot_combinado <- ggarrange(plotlist=plot_list, ncol = 2, nrow = 3, common.legend = TRUE, legend="bottom")
# Usamos cowplot para convertir de ggarrange a ggplot <.<
ggexport(filename="./output/boxplots.png", plot_combinado, device="png", width = 800, height = 800)

