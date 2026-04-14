#-------------Análisis Exploratorio--------------------#
#-------------Materia Optativa I ----------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#

# ----------------Ruta del archivo --------------------#
# Cambiar de acuerdo a la ubicación de los archivos de datos
# setwd("C:/Users/Usuario/Documents/UNSL/Cuarto Año/Optativa I")
# setwd("/home/nadia/unsl/bio/code/unsl-bio/data")

# ------------------Carga de Librerias----------------#
# install.packages("ggplot2")
# install.packages("gridExtra")
library(readxl)
library(ggplot2)
library(gridExtra)
library(ggbiplot)
# ----------------- Creación de la base de datos --------------------------#
dataset_name <- "pmed.1002587.s005.xlsx"

melatonine_participante <- read_excel(dataset_name, sheet = "Combined", range="A1:H3735")
melatonine_fecha_actigrafo <- read_excel(dataset_name, sheet = "Combined", range="AG1:AG3735")
melatonine_actigrafo <- read_excel(dataset_name, sheet = "Combined", range="AM1:AT3735")
melatonine <- cbind(melatonine_participante, melatonine_fecha_actigrafo, melatonine_actigrafo)
melatonine$anio_mes <- format(melatonine$Date_Onset_ACT, "%Y%m")
melatonine$StudyPeriodWeek <- factor(melatonine$StudyPeriodWeek)
melatonine$anio_mes <- format(melatonine$Date_Onset_ACT, "%Y%m")

# ----------------- Escrutinio de Datos Faltantes (NAs) -------------------#
# El artículo menciona que no hubo imputación; se verifica cuántos nulls hay por variable (sobre un total 3734)
colSums(is.na(melatonine[, c("SOL_ACT", "SET1_ACT")]))

# ------------------------- Variables auxiliares --------------------------#
melatonine$TratamientoDesc <- ifelse(melatonine$Treatment == 1, "Placebo", "Melatonina 0.5 mg")
melatonine$TrabajaDesc <- ifelse(melatonine$`Work/Non-work` == 1, "Obligaciones", "Descanso")

# ------------------ Distribuciones de variables categóricas --------------------------#
# x11(50,30); # Sistema Operativo Linux
windows(50,30) # Sistema operativo Windows
par(mfrow=c(1,4))
barplot(table(melatonine$TratamientoDesc), main="Tratamiento", xlab="Tratamiento", ylab="Frecuencia")
barplot(table(melatonine$`Delayed/Not Delayed`), main="Diagnóstico retraso sueño", xlab="Retraso sueño", ylab="Frecuencia")
barplot(table(melatonine$TrabajaDesc), main="Trabaja día registro", xlab="Trabaja", ylab="Frecuencia")
barplot(table(melatonine$StudyPeriodWeek), main="Semana de estudio", xlab="Semana", ylab="Frecuencia")

# ------------------- Diagramas de caja para las variables continuas ------------------#
windows(50, 30); # Sistema operativo Windows
# x11(50,30); # Sistema Operativo Linux
par(mfrow=c(1,2))
boxplot(melatonine$SOL_ACT, col="paleturquoise4", ylab="Latencia de inicio de sueño - SOL (minutos)")
boxplot(melatonine$SET1_ACT, col="paleturquoise4", ylab="Eficiencia del sueño 1er tercio - SET1 (%)")

# -------------------- Histogramas variables continuas --------------------------------#
# x11(50,30); # Sistema Operativo Linux
windows(50, 30); # Sistema operativo Windows
par(mfrow=c(1,2))
hist(melatonine$SOL_ACT, breaks=30, main="Latencia de Sueño - Base", xlab="SOL (minutos)", ylab="Frecuencia")
hist(melatonine$SET1_ACT, breaks=30, main="Eficiencia sueño - 1er tercil - Base", xlab="SET1 (%)", ylab="Frecuencia")

# ------------------- Varianzas por tratamiento y semana -------------------------------#
windows()
# x11();
ggplot(melatonine, aes(x = factor(TratamientoDesc), fill=StudyPeriodWeek, y = SOL_ACT)) +
  geom_boxplot() +
  labs(title = "Latencia de Sueño",
       x = "Tratamiento",
       y = "Latencia de Sueño (min)") +
  theme_minimal()
# x11();
windows()
ggplot(melatonine, aes(x = factor(TratamientoDesc), fill = StudyPeriodWeek, y = SET1_ACT)) +
  geom_boxplot() +
  labs(title = "Eficiencia sueño - 1er tercio",
       x = "Tratamiento",
       y = "Eficiencia sueño (%)") +
  theme_minimal()

# ---------------- Independencia: Observaciones por Participante ------------------------#
obs_por_sujeto <- table(melatonine$ParticipantID)
hist(obs_por_sujeto, main="Nro de noches registradas por participante")

# ------------------------------ Análisis de fechas -------------------------------------#
meses <- as.data.frame(table(melatonine$anio_mes))

# x11(20,10);
windows(20,10);
ggplot(meses, aes(x=Var1, y=Freq)) + 
  geom_col(color="white", fill="paleturquoise4") +
  labs(x = "Mes", y = "Observaciones", title = "Meses abarcados por el estudio")

fechas_por_participante <-melatonine |>
  select(ParticipantID, StudyPeriodWeek, Date_Onset_ACT) |>
  na.omit() |>
  group_by(ParticipantID) |> 
  summarise(
    maximum = max(Date_Onset_ACT),
    minimum = min(Date_Onset_ACT),
    n = length(ParticipantID)
  )

fechas_por_participante$dias <- as.numeric(fechas_por_participante$maximum - fechas_por_participante$minimum)
# Se observan varios días de diferencia entre inicio y final del estudio por 
# participante si no se filta la primera semana
fechas_por_participante[order(-fechas_por_participante$dias),]


fechas_por_participante <-melatonine |>
  filter(StudyPeriodWeek != 0) |>
  select(ParticipantID, Date_Onset_ACT) |>
  na.omit() |>
  group_by(ParticipantID) |> 
  summarise(
    maximum = max(Date_Onset_ACT),
    minimum = min(Date_Onset_ACT),
    n = length(ParticipantID)
  )
fechas_por_participante$dias <- as.numeric(fechas_por_participante$maximum - fechas_por_participante$minimum)

# Si se consideran las semanas del estudio propiamente dichas, hay como máximo 28 
# días entre comienzo y final del estudio
fechas_por_participante[order(-fechas_por_participante$dias),]
######################################################################################
# Modelos
######################################################################################
variables_num <- c("TIB_ACT", "TST_ACT", "SOL_ACT", "SE_ACT", "WASO_ACT", "SET1_ACT", "SET2_ACT", "SET3_ACT")
#--------------------------- Cambio de nombre de variables --------------------------#
colnames(melatonine)[colnames(melatonine) == "Work/Non-work"] <- "Work_status"
colnames(melatonine)[colnames(melatonine) == "Delayed/Not Delayed"] <- "Delayed_status"

# ----------------------- GLMM Estandarizado ----------------------------------------#  
# install.packages("glmmTMB")
library(glmmTMB)

melatonine_estandar <- data.frame(melatonine)
melatonine_estandar[c("SOL_ACT")] <- scale(melatonine_estandar[c("SOL_ACT")])
melatonine_estandar$SET1_ACT <- melatonine_estandar$SET1_ACT/100
m1 <- glmmTMB(SOL_ACT ~ Treatment + Work_status, 
              data = melatonine_estandar, 
              family = gaussian)
# Remover ceros para usar beta
melatonine_estandar <- melatonine_estandar[melatonine_estandar$SET1_ACT > 0,]

m2 <- glmmTMB(SET1_ACT ~ Treatment + Work_status, 
              data = melatonine_estandar, 
              family = beta_family)
summary(m1)
summary(m2)

# -------------------------------------- PCA ----------------------------------------#  
melatonine_num <- melatonine[, variables_num]
summary(melatonine_num)

x <- na.omit(melatonine_num)
pca <- prcomp(x, scale = T, center = T)

summary(pca)
names(pca)

windows(20, 20);
# x11(20, 20);
ggbiplot(pca, obs.scale = 1, var.scale = 1,
                     # groups = iris$Species, 
                     point.size=1,
                     varname.size = 3, 
                     varname.color = "red",
                     varname.adjust = 1.2,
                     ellipse = F, 
                     circle = F) +
  theme_minimal(base_size = 20) 

vars
m3 <- glmmTMB(SOL_ACT ~ pca$x + Work_status, 
              data = melatonine_estandar, 
              family = gaussian)

#-----------------------Limpieza de valores NA -------------------------------#
columnas_uso <- c("SOL_ACT", "SET1_ACT", "Treatment", "StudyPeriodWeek",
                  "Work_status", "TIB_ACT", "TST_ACT", "SE_ACT",
                  "SET2_ACT", "SET3_ACT", "Date_Onset_ACT")

datos_clean <- na.omit(melatonine[, columnas_uso])

#--------------------MODELO BAYESIANO-----------------------------#
# install.packages("brms")
library(brms)
formula_multi <- mvbf(
  SOL_ACT ~ Treatment + StudyPeriodWeek + Work_status +
    TIB_ACT + TST_ACT + SE_ACT,
  SET1_ACT ~ Treatment + StudyPeriodWeek + Work_status +
    TIB_ACT + TST_ACT + SE_ACT
)
fit_multi <- brm(
  formula = formula_multi,
  data = datos_clean,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 2000
)

summary(fit_multi)
