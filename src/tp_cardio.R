rm(list = ls())
gc()

# Librerías utilizadas.
library("data.table")
library("dplyr")
library("fastDummies")
library("pROC")
library("caret")
library("e1071")
library("naivebayes")
library("ggplot2")
library("ggpubr")

datos <- fread("./data/cardio_train.csv", stringsAsFactors = T, sep = ';')

str(datos)
head(datos)

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

datos_modificada_2$cholesterol <- as.factor(datos_modificada_2$cholesterol)
datos_modificada_2$gender <- as.factor(datos_modificada_2$gender)
datos_modificada_2$gluc <- as.factor(datos_modificada_2$gluc)
datos_modificada_2$smoke <- as.factor(datos_modificada_2$smoke)
datos_modificada_2$alco <- as.factor(datos_modificada_2$alco)
datos_modificada_2$active <- as.factor(datos_modificada_2$active)
datos_modificada_2$cardio <- as.factor(datos_modificada_2$cardio)

# Luego de hacer factor las variables, hacemos que los valores sean más intuitivos
# Para genero, convertimos 1 en F (femenino) y 2 en M (masculino)
datos_modificada_3 <- datos_modificada_2 %>% dplyr::mutate(gender = case_when(
  gender == "1" ~ "F",
  gender == "2" ~ "M",
  TRUE ~ "ERROR"
))

# Para gluc (glucosa en sangre), convertimos 1 = N (normal), 2 = E (elevado), 3 = ME (muy elevado)
datos_modificada_4 <- datos_modificada_3 %>% dplyr::mutate(gluc = case_when(
  gluc == "1" ~ "N",
  gluc == "2" ~ "E",
  gluc == "3" ~ "ME",
  TRUE ~ "ERROR"
))

# Para cholesterol (colesterol en sangre), convertimos 1 = N (normal), 2 = E (elevado), 3 = ME (muy elevado)
datos_modificada_5 <- datos_modificada_4 %>% dplyr::mutate(cholesterol = case_when(
  cholesterol == "1" ~ "N",
  cholesterol == "2" ~ "E",
  cholesterol == "3" ~ "ME",
  TRUE ~ "ERROR"
))

# Después de las modificaciones a las columnas, procedemos a eliminar cualquier dato erroneo.
datos_modificada_5 <- datos_modificada_5 %>% dplyr::filter(cholesterol != "ERROR" & gender != "ERROR" & gluc != "ERROR")

# Hacemos un summary para ver los rangos de cada una de las variables cuantitativas
variables <- c("age", "height", "weight", "ap_hi", "ap_lo")

sum_data <- datos_modificada_5 %>% select(all_of(variables)) %>% summary()
print(sum_data)

# Vemos un par de irregularidades, como por ejemplo que para la presión sistólica y diastólica observamos
# valores negativos o valores que no tienen sentido como el máximo que presenta.
# Haremos una pequeña limpieza, quitaremos el 2.5% de los datos de cada lado.
limpieza_dataset <- datos_modificada_5 %>% dplyr::filter(between(ap_hi, quantile(ap_hi, 0.025), quantile(ap_hi, 0.975)) &
                                                         between(ap_lo, quantile(ap_lo, 0.025), quantile(ap_lo, 0.975)))

# También observamos que height y weight tienen un mínimo de 55 y de 10 respectivamente...
# No representan valores muy reales, por lo que podemos hacer lo mismo que hicimos anteriormente con ap_hi y ap_lo

limpieza_dataset_2 <- limpieza_dataset %>% dplyr::filter(between(height, quantile(height, 0.025), quantile(height, 0.975)) &
                                                           between(weight, quantile(weight, 0.025), quantile(weight, 0.975)))

# Por pregunta a un médico, sabemos que la presión diástolica NUNCA puede ser más grande que la presión sistólica.
# Por lo que vamos a remover estos datos.
dataset_limpio <- limpieza_dataset_2 %>% dplyr::filter(ap_lo < ap_hi)

# Volvemos a observar los datos
sum_data_limpieza <- limpieza_dataset_2 %>% select(all_of(variables)) %>% summary()
print(sum_data_limpieza)
# Ahora parece ser que los datos están "limpios", procederemos a graficar para verificar

# Gráficos de barra para cada una de nuestras variables cualitativas
plot_list <- list()
variables_cuali <- c("gender", "cholesterol", "gluc", "smoke", "alco", "active")

for (variable in variables_cuali)
{
    plot_cuali <- ggplot(dataset_limpio, aes_string(x = variable, fill = "cardio")) +
                  geom_bar(position = "dodge") +
                  labs(x = variable, y = "Cantidad", fill = "Cardio") +
                  scale_fill_manual(values = c("green4", "red"), labels = c("No", "Si"))
    plot_list[[length(plot_list) + 1]] <- plot_cuali
}

barras_combinado <- ggarrange(plotlist = plot_list, ncol = 2, nrow = 3, common.legend = TRUE, legend="bottom")
ggexport(filename="./output/barplots.png", barras_combinado, device = "png", width = 800, height = 800)

# Boxplots de exploración para cada una de nuestras variables cuantitativas con respecto a cardio
plot_list <- list()
for (variable in variables)
{
  plot <- ggplot() +
          geom_boxplot(data = dataset_limpio, aes_string(x = "cardio", y = variable, fill = "cardio")) +
          labs(title = variable)
  plot_list[[length(plot_list) + 1]] <- plot
}

plot_combinado <- ggarrange(plotlist=plot_list, ncol = 2, nrow = 3, common.legend = TRUE, legend="bottom")
ggexport(filename="./output/boxplots.png", plot_combinado, device="png", width = 800, height = 800)

# Hipótesis/preguntas:
# ¿Qué variables de la base de datos son buenos indicadores independientes de la presencia de enfermedad cardiovascular?.
# ¿Hay variables que puedan ser excluidas del modelo predictivo?

# Para hallar las respuestas a estas hipótesis, implementaremos 2 modelos, Regresión Logística y Naive-Bayes

# Antes de nada, realizaremos las variables dummies pertinentes para cada una de nuestras variables categoricas

dataset.dummies <- fastDummies::dummy_cols(dataset_limpio, 
                              select_columns = variables_cuali,
                              remove_selected_columns = T)

# Control para k-fold cv
# trControl <- caret::trainControl(method = "cv",
#                           number = 10)
# # Modelo de Naive-Bayes
# modelo.nb <- caret::train(cardio ~ .,
#                           method    = "naive_bayes",
#                           trControl = trControl,
#                           metric    = "Accuracy",
#                           data      = dataset.dummies)
# 
# modelo.nb.pred <- predict(modelo.nb, newdata = dataset.dummies,
#                           type = "prob")
# 
# modelo.nb.pred.corregido <- ifelse(modelo.nb.pred > 0.5, 1, 0)
# 
# auc.nb <- auc(dataset.dummies$cardio, modelo.nb.pred.corregido[,2])

#conf.mat.nb <- caret::confusionMatrix(modelo.nb.pred.corregido, dataset_limpio$cardio)

# Modelo de Regresión Logística
# modelo.glm <- caret::train(cardio ~ .,
#                            method    = "glm",
#                            trControl = trControl,
#                            metric    = "Accuracy",
#                            data      = dataset.dummies)
# 
# modelo.glm.pred <- predict(modelo.glm,
#                            type = "prob")
# 
# # Esto se tiene que hacer siempre que sean probabilidades?
# modelo.glm.pred.corregido <- ifelse(modelo.glm.pred > 0.5, 1, 0)
# 
# auc.glm <- auc(dataset.dummies$cardio, modelo.glm.pred.corregido[,2])

## conf.mat.nb <- caret::confusionMatrix(modelo.glm.pred, as.factor(dataset.dummies$cardio))

# caret::confusionMatrix(data=factor(modelo.nb.pred.corregido), reference= factor(dataset.dummies$cardio))


## USAMOS EL PAQUETE E1071

# Control 10-fold cv (e1071)
# trControl <- e1071::tune.control(sampling = "cross",
#                                  cross = 10)
# # Modelo de Naive-Bayes
# modelo.nb <- e1071::tune(METHOD = "naiveBayes",
#                          train.x = cardio ~ .,
#                          data = dataset.dummies,
#                          tunecontrol = trControl)
# 
# modelo.nb.pred <- predict(modelo.nb$best.model, newdata = dataset.dummies)

# Control 10-fold cv (caret)
trControl <- caret::trainControl(method = "cv", 
                          number = 10)

# CARET
modelo.nb <- caret::train(cardio ~ .,
                           method = "naive_bayes",
                           trControl = trControl,
                           metric = "Accuracy",
                           data = dataset.dummies)

modelo.nb.pred <- predict(modelo.nb, newdata = dataset.dummies)

# Modelo de Regresión Logística
modelo.glm <- caret::train(cardio ~ .,
                         method = "glm",
                         family = "binomial",
                         trControl = trControl,
                         metric = "Accuracy",
                         data = dataset.dummies)

modelo.glm.pred <- predict(modelo.glm, newdata = dataset.dummies)

# Curvas ROC, AUC y matriz de confusión para cada modelo

mat.conf.nb <- caret::confusionMatrix(modelo.nb.pred, dataset.dummies$cardio)
mat.conf.glm <- caret::confusionMatrix(modelo.glm.pred, dataset.dummies$cardio)

roc.nb <- roc(response = dataset.dummies$cardio, predictor=as.numeric(modelo.nb.pred))
roc.glm <- roc(response = dataset.dummies$cardio, predictor=as.numeric(modelo.glm.pred))

auc.nb <- auc(roc.nb)
auc.glm <- auc(roc.glm)

plot_roc <- ggroc(list("Modelo Naive-Bayes"=roc.nb, "Modelo Regresion Logística"=roc.glm)) +
  labs(title = "Curvas ROC", x = "1 - Specificidad", y = "Sensibilidad") +
  scale_color_manual(values = c("green3", "red3")) +
  guides(color = guide_legend(title = "Modelos")) +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 6))

ggsave("./output/curvas_roc.png", plot = plot_roc, width = 6, height = 4, dpi = 600)

cat("Valores AUC:\n", "Modelo Naive-Bayes: ", round(auc.nb, 4), " \n",
                               "Modelo Regresión Logística: ", round(auc.glm, 4))


