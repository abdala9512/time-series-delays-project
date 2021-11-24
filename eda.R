
library(readxl)
library(dplyr)
library(ggplot2)
library(psych)
library(magrittr)
library(tidyr)
library(viridis)
library(lubridate)
library(caret)
library(rpart)
library(rpart.plot)
library(fastDummies)
library(party)


source("ggplot_custom_theme.R")

train_data <- read_excel("data/citaschallenge.xlsx", sheet = "train2013")
test_data <- read_excel("data/citaschallenge.xlsx", sheet = "test2013")


calculateChallengeMetric <- function(obj){
  cancelacion = if_else(is.na(obj[1]),0, obj[1] * 0.3)
  incumplimiento = obj[3] * 0.5
  cumplimiento   = obj[2] * 0.2

  weighted_metric <- cancelacion + incumplimiento + cumplimiento
  message("F ponderado: ")
  return(weighted_metric %>%  as.numeric())
}

# Preparacion de datos ----------------------------------------------------

train_data$GENERO <- as.factor(train_data$GENERO)
train_data$Hora <- as.factor(train_data$Hora)
train_data$ESTAFINAL <- as.factor(train_data$ESTAFINAL)


estadisticas <-  lapply(X = train_data[,c(-6,-5,-2)], FUN = table)
estadisticas

estadisticas2 <- lapply(X = estadisticas, FUN = prop.table)
estadisticas2

top <-as.data.frame(prop.table(table(train_data$ESPECIALIDAD)))
top_n(top,3)

top <-as.data.frame(prop.table(table(train_data$Hora)))
top_n(top,5)

train_data %<>%
  mutate(
    Estado_Final =
      case_when(
        ESTAFINAL== 1 ~ "Cancelada",
        ESTAFINAL == 2 ~ "Cumplida",
        ESTAFINAL == 3 ~ "Incumplida"
      ),
    Hora = hour(FECHA_CITA),
    dia_semana = weekdays(FECHA_CITA),
    especialidad_ajustada =
      case_when(
        ESPECIALIDAD %in% c("ORTODONCIA", "ODONTOLOGIA") ~ "SD",
        ESPECIALIDAD %in% c("MEDICINA GENERAL") ~ "MG",
        TRUE ~ "ESP"
      )


  )


  # revision Preliminar de los datos ----------------------------------------

glimpse(train_data)
describe(train_data)


train_data %>%
  ggplot(aes(ESTAFINAL)) +
  geom_bar(stat = "count", fill = "#678983")  +
  custom_style() +
  labs(
    title = "Estado final del total de citas"
  )


#
train_data %>%
  ggplot(aes(GENERO)) +
  geom_bar(stat = "count", fill = "#396EB0") +
  facet_grid(~Estado_Final) +
  custom_style() +
  labs(
    title = "Estados finales por género"
  )

# eSPECILIDAD AJSUTADA

train_data %>%
ggplot(aes(especialidad_ajustada)) +
  geom_bar(stat = "count", fill = "#396EB0") +
  facet_grid(~Estado_Final) +
  custom_style() +
  labs(
    title = "Estados finales por especilidad"
  )


 #

train_data %>%
  group_by(Estado_Final, TIPO_AFILIACION) %>%
  count() %>%
  ggplot(aes(x=TIPO_AFILIACION, fill = Estado_Final, y = n)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_viridis(discrete = T) +
  #facet_grid(~Estado_Final) +
  custom_style() +
  labs(
    title = "Porcentaje de estados por Tipo de afiliación"
  )


 train_data %>%
  ggplot(aes(EDAD)) +
  geom_histogram(color = "#ffffff", fill = "#B85252") +
  facet_grid( ~Estado_Final) +
  custom_style() +
  labs(
    title =  "Distribución Edades por estado de cita"
  )


 train_data %>%
   mutate(Hora = as.factor(Hora)) %>%
   group_by(Estado_Final, Hora) %>%
   count() %>%
   ggplot(aes(x=Hora, fill = Estado_Final, y = n)) +
   geom_bar(position = "fill", stat = "identity") +
   scale_fill_viridis(discrete = T) +
   #facet_grid(~Estado_Final) +
   custom_style() +
   scale_fill_manual(values= c("#544179", "#32C1CD", "#C85C5C")) +
   labs(
     title = "Porcentaje de estados por Hora de cita"
   )

 train_data %>%
   mutate(dia_semana = as.factor(dia_semana)) %>%
   group_by(Estado_Final, dia_semana) %>%
   count() %>%
   ggplot(aes(x=dia_semana, fill = Estado_Final, y = n)) +
   geom_bar(position = "fill", stat = "identity") +
   scale_fill_viridis(discrete = T) +
   #facet_grid(~Estado_Final) +
   custom_style() +
   scale_fill_manual(values= c("#77E4D4", "#96C7C1", "#678983")) +
   labs(
     title = "Porcentaje de estados por dia de la semana"
   )


# Citas ´por especialidad

train_data %>%
  ggplot(aes(x = forcats::fct_rev(forcats::fct_infreq(ESPECIALIDAD)))) +
  geom_bar(stat = "count",color = "#ffffff", fill = "#17D7A0") +
  custom_style() +
  labs(
    title =  "Citas por especialidad"
  ) +
  coord_flip() +
  xlab("Especialidad")


# Porcentaje de cancelaciones, incumplimiento y cumplimiento por especialidad


calcPercentages <- function(col){

  names_ <-
    c(
      paste("pct_cancelacion", col, sep = "_"),
     # paste("pct_cumplimiento", col, sep = "_"),
      paste("pct_incumplimiento", col, sep = "_")
    )

  percentages <- train_data %>%
    filter(FECHA_CITA < ymd("2013-11-01")) %>%
    group_by(!!as.name(col)) %>%
    mutate(total_citas = n()) %>%
    group_by(!!as.name(col), Estado_Final) %>%
    summarise(
      pct = (  n() / total_citas ) * 100

    ) %>%
    distinct() %>%
    ungroup() %>%
    spread(key = Estado_Final,value = pct) %>%
    rename(
      pct_cancelacion = Cancelada,
      pct_cumplimiento = Cumplida,
      pct_incumplimiento = Incumplida
    ) %>%
    mutate(
      pct_cumplimiento = if_else(is.na(pct_cumplimiento), 0 , pct_cumplimiento),
      pct_incumplimiento = if_else(is.na(pct_incumplimiento), 0 , pct_incumplimiento),
      pct_cancelacion = if_else(is.na(pct_cancelacion), 0 , pct_cancelacion)
    ) %>%
    select(-pct_cumplimiento) %>%
    rename_with(
      ~names_ , c("pct_cancelacion",
                  #"pct_cumplimiento",
                  "pct_incumplimiento")
    )


  return(percentages)
}

specialization_percentages <- calcPercentages("ESPECIALIDAD")
gender_percentages         <- calcPercentages("GENERO")
affiliaton_percentages     <- calcPercentages("TIPO_AFILIACION")
weekday_percentages        <- calcPercentages("dia_semana")
hour_percentages           <- calcPercentages("Hora")
ajd__esp_percentage        <- calcPercentages("especialidad_ajustada")

specialization_percentages %>%
  gather(key = Tipo, value = Porcentaje, -ESPECIALIDAD) %>%
  mutate(Porcentaje = round(Porcentaje, 2)) %>%
  ggplot(aes(x = Tipo, y = ESPECIALIDAD, fill = Porcentaje)) +
  geom_tile(color = "white",
            lwd = 0.5,
            linetype = 1) +
  geom_text(aes(label = Porcentaje), color = "white", size = 3) +
  scale_fill_gradient(low = "#66806A", high = "red") +
  custom_style() +
  labs(
    title = "Porcentajes de Cumplimiento/Incumplimiento/Cancelación"
  )


# Algunas exploracio por series de tiempo

train_data %>%
  mutate(Fecha = date(FECHA_CITA)) %>%
  group_by(Fecha) %>%
  count() %>%
  rename(citas = n) %>%
  ggplot(aes(x = Fecha, y = citas )) +
  geom_line() +
  custom_style()


#  Comportamiento de citas canceladas
train_data %>%
  mutate(Fecha = date(FECHA_CITA)) %>%
  group_by(Fecha, Estado_Final) %>%
  count() %>%
  rename(citas = n) %>%
  filter(Estado_Final == "Cancelada") %>%
  ggplot(aes(x = Fecha, y = citas )) +
  geom_line() +
  custom_style()


train_data %>%
  mutate(Fecha = date(FECHA_CITA)) %>%
  group_by(Fecha, Estado_Final) %>%
  count() %>%
  rename(citas = n) %>%
  filter(Estado_Final == "Cumplida") %>%
  ggplot(aes(x = Fecha, y = citas )) +
  geom_line() +
  custom_style()


train_data %>%
  mutate(Fecha = date(FECHA_CITA)) %>%
  group_by(Fecha, Estado_Final) %>%
  count() %>%
  rename(citas = n) %>%
  filter(Estado_Final == "Incumplida") %>%
  ggplot(aes(x = Fecha, y = citas )) +
  geom_line() +
  custom_style()



# PREPROCESAMIENTO FINAL --------------------------------------------------

cat_features <- c("TIPO_AFILIACION", "dia_semana","GENERO")

normalized_data <-
  train_data %>%
  left_join(specialization_percentages, by = "ESPECIALIDAD") %>%
  #left_join(gender_percentages, by = "GENERO") %>%
  left_join(affiliaton_percentages, by = "TIPO_AFILIACION") %>%
  left_join(weekday_percentages, by = "dia_semana") %>%
  left_join(hour_percentages, by = "Hora") %>%
  #left_join(ajd__esp_percentage, by = "especialidad_ajustada") %>%
  # select(
  #   TIPO_AFILIACION,
  #   Estado_Final,
  #   EDAD,
  #   Hora,
  #   dia_semana,
  #   GENERO
  # ) %>%
 # mutate(Hora = as.character(Hora)) %>%
  # dummy_cols(cat_features) %>%
  select(
    -c(
      TIPO_AFILIACION,
      dia_semana,
      GENERO,
      Hora,
      ESPECIALIDAD,
      id,
      ESTAFINAL,
      especialidad_ajustada

    )
  ) %>%
  filter(
    Estado_Final != "Cumplido"
  ) %>%
  mutate (
    Estado_Final = factor(Estado_Final)
  )



set.seed(1500)

trainIndex <- createDataPartition(normalized_data$Estado_Final, p = .85, list = FALSE, times = 1)


#train <- normalized_data[ trainIndex,]
train <- normalized_data %>% filter(FECHA_CITA < ymd("2013-11-01")) %>% select(-FECHA_CITA)
#test  <- normalized_data[-trainIndex,]
test  <- normalized_data %>% filter(FECHA_CITA >= ymd("2013-11-01")) %>% select(-FECHA_CITA)


# MODELAMIENTO ------------------------------------------------------------


decisionTree <- rpart(
  formula = Estado_Final ~ .,
  data    = train,
  method  = "class",
  control = list(minbucket=20, cp = 0.00001, minsplit = 5, maxdepth = 4, xval = 10)
)

rpart.plot(decisionTree)

table(decisionTree$where)


plotcp(decisionTree)

decisionTree$cptable

dt_predictions <- predict(decisionTree,test, type="class")

conf_matrix <- confusionMatrix(dt_predictions,test$Estado_Final)
f1_metrics <- conf_matrix$byClass[,'F1']
f1_metrics
conf_matrix$byClass[,'Precision']
conf_matrix$byClass[,'Recall']


# Calculo metrica Challenge
calculateChallengeMetric(f1_metrics)


# Multiples modelos -------------------------------------------------------

dataset_incumplidos <- rbind(
  normalized_data %>%
    filter(Estado_Final == 'Incumplida'),
  normalized_data %>%
    filter(Estado_Final == "Cumplida") %>%
    sample_frac(0.3)
) %>%
  mutate(Estado_Final = factor(Estado_Final))

trainIndex <- createDataPartition(dataset_incumplidos$Estado_Final, p = .85, list = FALSE, times = 1)


train_incumplidos <- dataset_incumplidos[ trainIndex,]
test_incumplidos  <- dataset_incumplidos[-trainIndex,]

train_incumplidos <- dataset_incumplidos %>% filter(FECHA_CITA < ymd("2013-11-01")) %>% select(-FECHA_CITA)
test_incumplidos  <- dataset_incumplidos %>% filter(FECHA_CITA >= ymd("2013-11-01")) %>% select(-FECHA_CITA)


decisionTreeUnfulfilled <- rpart(
  formula = Estado_Final ~ .,
  data    = train_incumplidos,
  method  = "class",
  control = list(minbucket=20, cp = 0.00001, minsplit = 5, maxdepth = 4, xval = 10)
)

rpart.plot(decisionTreeUnfulfilled)

dt_predictions_incumplidos <- predict(decisionTreeUnfulfilled,test_incumplidos, type="class")
conf_matrix <- confusionMatrix(dt_predictions_incumplidos,test_incumplidos$Estado_Final)
conf_matrix$byClass


# Cancelados --------------------------------------------------------------

dataset_cancelados <- rbind(
  normalized_data %>%
    filter(Estado_Final == 'Cancelada'),
  normalized_data %>%
    filter(Estado_Final == "Cumplida") %>%
    sample_frac(0.3)
) %>%
  mutate(Estado_Final = factor(Estado_Final))

trainIndex <- createDataPartition(dataset_cancelados$Estado_Final, p = .85, list = FALSE, times = 1)


train_cancelados <- dataset_cancelados[ trainIndex,]
test_cancelados  <- dataset_cancelados[-trainIndex,]

train_cancelados <- dataset_cancelados %>% filter(FECHA_CITA < ymd("2013-11-01")) %>% select(-FECHA_CITA)
test_cancelados  <- dataset_cancelados %>% filter(FECHA_CITA >= ymd("2013-11-01")) %>% select(-FECHA_CITA)


decisionTreeCancelled <- rpart(
  formula = Estado_Final ~ .,
  data    = train_cancelados,
  method  = "class",
  control = list(minbucket=20, cp = 0.00001, minsplit = 5, maxdepth = 4, xval = 10)
)

rpart.plot(decisionTreeCancelled)

dt_predictions_cancelled <- predict(decisionTreeCancelled,test_cancelados, type="class")
conf_matrix <- confusionMatrix(dt_predictions_cancelled,test_cancelados$Estado_Final)
f1_metrics <- conf_matrix$byClass


# Model mixing ------------------------------------------------------------


preds_complete_model <- predict(decisionTree,test)
preds_cancelled_model <- predict(decisionTreeCancelled,test)
preds_unfulfilled_model <- predict(decisionTreeUnfulfilled,test)


x<-rbind(
  preds_complete_model %>%  as.data.frame() %>%  mutate(id = row_number()) %>% gather(key = class, value = prob, -id),
  preds_cancelled_model %>%  as.data.frame()  %>%  mutate(id = row_number()) %>% gather(key = class, value = prob, -id),
  preds_unfulfilled_model %>%  as.data.frame() %>%  mutate(id = row_number()) %>% gather(key = class, value = prob, -id)
) %>%
  group_by(id, class) %>%
  summarise(prob = mean(prob)) %>%
  group_by(id) %>%
  top_n(1) %>%
  pull(class) %>%  as.factor()


conf_matrix <- confusionMatrix(x,test$Estado_Final)
mixedModelScore <- conf_matrix$byClass[,'F1']

calculateChallengeMetric(mixedModelScore)


# Optimizaccion de hiperparametros ----------------------------------------


# Creamos un dataframe con las posibles combinaciones de hiperparametros
hyper_grid <- expand.grid(
  minsplit = seq(15, 30, 1), # poblaciones entre 5 y 20
  maxdepth = seq(5, 15, 1) #profundidad del arbol entre 8 y 15
)


models <- vector(mode = "list", nrow(hyper_grid))

for (i in 1:nrow(hyper_grid)) {

  # Obtenemos los hiperparametros de la estimación i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]

  # Entrenamos el modelo
  models[[i]] <- rpart(
    formula = Estado_Final ~ .,
    data    = train,
    method  = "class",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}


# Creamos una función para extraer el cost complexity de la lis de modelos
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"]
}

# Función para extraer el modelo con el error minimo
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}


# Miramos el top 5 de valore minimos
hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)
# Partir por especialidades medicas, medicina general y odontologia




# Create submission file --------------------------------------------------


test_normalized_data <-
  test_data %>%
  mutate(
    Hora = hour(FECHA_CITA),
    dia_semana = weekdays(FECHA_CITA)
  ) %>%
  left_join(specialization_percentages, by = "ESPECIALIDAD") %>%
  left_join(affiliaton_percentages, by = "TIPO_AFILIACION") %>%
  left_join(weekday_percentages, by = "dia_semana") %>%
  left_join(hour_percentages, by = "Hora") %>%
  select(
    -c(
      TIPO_AFILIACION,
      dia_semana,
      GENERO,
      Hora,
      ESPECIALIDAD,
      id,
      FECHA_CITA

    )
  )


TEST_preds_complete_model <- predict(decisionTree,test_normalized_data)
TEST_preds_cancelled_model <- predict(decisionTreeCancelled,test_normalized_data)
TEST_preds_unfulfilled_model <- predict(decisionTreeUnfulfilled,test_normalized_data)

submission_df <- test_data %>%
  select(id)

test_predictions<-rbind(
  cbind(submission_df, TEST_preds_complete_model %>%  as.data.frame()) %>% gather(key = class, value = prob, -id),
  cbind(submission_df, TEST_preds_cancelled_model %>%  as.data.frame()) %>% gather(key = class, value = prob, -id),
  cbind(submission_df, TEST_preds_unfulfilled_model %>%  as.data.frame()) %>% gather(key = class, value = prob, -id)
) %>%
  group_by(id, class) %>%
  summarise(prob = mean(prob)) %>%
  group_by(id) %>%
  top_n(1) %>%
  mutate(
    ESTAFINAL = case_when(
      class == "Cancelada" ~ 1,
      class == "Cumplida" ~ 2,
      class == "Incumplida"~ 3
    )
  ) %>%
  pull(ESTAFINAL)


submission_df$ESTAFINAL = test_predictions

writexl::write_xlsx(submission_df, "data/predicciones.xlsx")

