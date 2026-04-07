#-------------Análisis Exploratorio--------------------#
#-------------Materia Optativa I ----------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#

# ----------------Ruta del archivo --------------------#
# Cambiar de acuerdo a la ubicación de los archivos de datos
#setwd("C:/Users/Usuario/Documents/UNSL/Cuarto Año/Optativa I")
# setwd("/home/nadia/unsl/bio/code/unsl-bio/data")

# ------------------Carga de Librerias----------------#
install.packages("ggplot2")
install.packages("gridExtra")
library(readxl)
library(ggplot2)
library(gridExtra)

# ----------------- Creación de la base de datos --------------------------#
melatonine_all <- read_excel("pmed.1002587.s005.xlsx", sheet = "Combined")
melatonine <- melatonine_all[,c("ParticipantID", "Treatment", "Delayed/Not Delayed", 
"StudyPeriodWeek", "Work/Non-work", "TIB_ACT", "TST_ACT", "SOL_ACT",	"SE_ACT",	
"WASO_ACT",	"SET1_ACT",	"SET2_ACT",	"SET3_ACT")]

# ----------------- Escrutinio de Datos Faltantes (NAs) -------------------#
# El artículo menciona que no hubo imputación; se verifica cuántos nulls hay por variable (sobre un total 3734)
colSums(is.na(melatonine[, c("SOL_ACT", "SET1_ACT")]))

# ------------------------- Variables auxiliares --------------------------#
melatonine$Base <- ifelse(melatonine$StudyPeriodWeek == 0, "Sí", "No")
melatonine$TratamientoDesc <- ifelse(melatonine$Treatment == 1, "Placebo", "Melatonina 0.5 mg")

# ----------------------- UA Base vs Tratamiento --------------------------#
baseline <- melatonine[melatonine$StudyPeriodWeek == 0,]
treatment <- melatonine[melatonine$StudyPeriodWeek != 0,]

# ------------------ Distribuciones de variables categóricas --------------------------#
x11(50,30); # Sistema Operativo Linux
windows(50,30) # Sistema operativo Windows
par(mfrow=c(1,4))
barplot(table(melatonine$Treatment), main="Tratamiento", xlab="Tratamiento", ylab="Frecuencia")
barplot(table(melatonine$`Delayed/Not Delayed`), main="Diagnóstico retraso sueño", xlab="Retraso sueño", ylab="Frecuencia")
barplot(table(melatonine$`Work/Non-work`), main="Trabaja día registro", xlab="Trabaja", ylab="Frecuencia")
barplot(table(melatonine$StudyPeriodWeek), main="Semana de estudio", xlab="Semana", ylab="Frecuencia")

# ------------------- Diagramas de caja para las variables continuas ------------------#
# windows(50, 30); # Sistema operativo Windows
x11(50,30); # Sistema Operativo Linux
par(mfrow=c(1,2))
boxplot(melatonine$SOL_ACT~melatonine$Base, col="darkorange", ylab="Latencia de inicio de sueño - SOL (minutos)", xlab="Semana base")
boxplot(melatonine$SET1_ACT~melatonine$Base, col="darkorange", ylab="Eficiencia del sueño 1er tercio - SET1 (%)", xlab="Semana base")

# -------------------- Histogramas variables continuas --------------------------------#
x11(50,30); # Sistema Operativo Linux
# windows(50, 30); # Sistema operativo Windows
par(mfrow=c(2,2))
hist(baseline$SOL_ACT, breaks=30, main="Latencia de Sueño - Base", xlab="SOL (minutos)", ylab="Frecuencia")
hist(baseline$SET1_ACT, breaks=30, main="Eficiencia sueño - 1er tercil - Base", xlab="SET1 (%)", ylab="Frecuencia")
hist(treatment$SOL_ACT, breaks=30, main="Latencia de Sueño - Tratamiento", xlab="SOL (minutos)", ylab="Frecuencia")
hist(treatment$SET1_ACT, breaks=30, main="Eficiencia sueño - 1er tercil - Tratamiento", xlab="SET1 (%)", ylab="Frecuencia")

# ------------------- Varianzas por tratamiento y semana -------------------------------#
#windows()
x11();ggplot(melatonine, aes(x = factor(TratamientoDesc), fill = Base, y = SOL_ACT)) +
  geom_boxplot() +
  labs(title = "Latencia de Sueño",
       x = "Tratamiento",
       y = "Latencia de Sueño (min)") +
  theme_minimal()
x11();
#windows()
ggplot(melatonine, aes(x = factor(TratamientoDesc), fill = Base, y = SET1_ACT)) +
  geom_boxplot() +
  labs(title = "Eficiencia sueño - 1er tercio",
       x = "Tratamiento",
       y = "Eficiencia sueño (%)") +
  theme_minimal()

# ---------------- Independencia: Observaciones por Participante ------------------------#
obs_por_sujeto <- table(melatonine$ParticipantID)
hist(obs_por_sujeto, main="Nro de noches registradas por participante")
