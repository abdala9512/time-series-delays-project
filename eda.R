
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
  cancelacion = obj[1] * 0.3
  incumplimiento = obj[3] * 0.5
  cumplimiento   = obj[2] * 0.2

  weighted_metric <- cancelacion + incumplimiento + cumplimiento
  message("F ponderado: ")
  return(weighted_metric %>%  as.numeric())
}

# Preparacion de datos ----------------------------------------------------

train_data %<>%
  mutate(
    Estado_Final =
      case_when(
        ESTAFINAL== 1 ~ "Cancelada",
        ESTAFINAL == 2 ~ "Cumplida",
        ESTAFINAL == 3 ~ "Incumplida"
      ),
    Hora = hour(FECHA_CITA),
    dia_semana = weekdays(FECHA_CITA)

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
  left_join(gender_percentages, by = "GENERO") %>%
  left_join(affiliaton_percentages, by = "TIPO_AFILIACION") %>%
  left_join(weekday_percentages, by = "dia_semana") %>%
  left_join(hour_percentages, by = "Hora") %>%
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
      FECHA_CITA,
      id,
      ESTAFINAL
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


train <- normalized_data[ trainIndex,]
test  <- normalized_data[-trainIndex,]


# MODELAMIENTO ------------------------------------------------------------


decisionTree <- rpart(
  formula = Estado_Final ~ .,
  data    = train,
  method  = "class",
  control = list(minbucket=20, cp = 0.00001, minsplit = 5, maxdepth = 6, xval = 10)
)

rpart.plot(decisionTree)

table(decisionTree$where)


plotcp(decisionTree)

decisionTree$cptable

dt_predictions <- predict(decisionTree,test, type="class")

conf_matrix <- confusionMatrix(dt_predictions,test$Estado_Final)
conf_matrix$byClass[,'F1']
conf_matrix$byClass[,'Precision']
conf_matrix$byClass[,'Recall']



# Estimacion por ARboles tipo CHAID ---------------------------------------

chaid_decisionTree <-ctree(Estado_Final~., data=train)
plot(chaid_decisionTree)

predictionsCHAID <- predict(chaid_decisionTree, test, type = 'class')



# Optimizaccion de hiperparametros ----------------------------------------

# Partir por especialidades medicas, medicina general y odontologia

