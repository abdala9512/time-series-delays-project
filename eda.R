
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

specialization_percentages <- train_data %>%
  group_by(ESPECIALIDAD) %>%
  mutate(total_citas_especialidad = n()) %>%
  group_by(ESPECIALIDAD, Estado_Final) %>%
  summarise(
    pct = (  n() / total_citas_especialidad ) * 100

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
    pct_cumplimiento = if_else(is.na(pct_cumplimiento), 0 , pct_cumplimiento)
  )

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
  select(
    TIPO_AFILIACION,
    Estado_Final,
    EDAD,
    Hora,
    dia_semana,
    GENERO,
    pct_cancelacion,
    pct_cumplimiento,
    pct_incumplimiento
  ) %>%
 # mutate(Hora = as.character(Hora)) %>%
  dummy_cols(cat_features) %>%
  select(
    -c(
      TIPO_AFILIACION,
      dia_semana,
      GENERO,
      GENERO_MASCULINO,
      Hora
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
 control = list(minsplit = 10, maxdepth = 10, xval = 15)
)

rpart.plot(decisionTree)

table(decisionTree$where)


plotcp(decisionTree)

decisionTree$cptable

dt_predictions <- predict(decisionTree,test, type="class")

conf_matrix <- confusionMatrix(dt_predictions,test$Estado_Final)
conf_matrix$byClass[,'F1']


# Estimacion por ARboles tipo CHAID ---------------------------------------

chaid_decisionTree <-ctree(Estado_Final~., data=train)
plot(chaid_decisionTree)

predictionsCHAID <- predict(chaid_decisionTree, test, type = 'class')



# Optimizaccion de hiperparametros ----------------------------------------

# Partir por especialidades medicas, medicina general y odontologia

