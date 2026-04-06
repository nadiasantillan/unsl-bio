# Cambiar de acuerdo a la ubicación de los archivos de datos
setwd("/home/nadia/unsl/bio/code/unsl-bio/data")

library(readxl)
melatonine_all <- read_excel("pmed.1002587.s005.xlsx", sheet = "Combined")
melatonine <- melatonine_all[,c("ParticipantID", "Treatment", "Delayed/Not Delayed", 
"StudyPeriodWeek", "Work/Non-work", "TIB_ACT", "TST_ACT", "SOL_ACT",	"SE_ACT",	
"WASO_ACT",	"SET1_ACT",	"SET2_ACT",	"SET3_ACT")]

x11(50,30); # Sistema Operativo Linux
# windows(50, 30); # Sistema operativo Windows
par(mfrow=c(1,4))
barplot(table(melatonine$Treatment), main="Tratamiento", xlab="Tratamiento", ylab="Frecuencia")
barplot(table(melatonine$`Delayed/Not Delayed`), main="Diagnóstico retraso sueño", xlab="Retraso sueño", ylab="Frecuencia")
barplot(table(melatonine$`Work/Non-work`), main="Trabaja día registro", xlab="Trabaja", ylab="Frecuencia")
barplot(table(melatonine$StudyPeriodWeek), main="Semana de estudio", xlab="Semana", ylab="Frecuencia")

x11(50,30); # Sistema Operativo Linux
# windows(50, 30); # Sistema operativo Windows
par(mfrow=c(2,4))
boxplot(melatonine$TIB_ACT, col="darkorange", ylab="Tiempo acostado - TIB (minutos)")
boxplot(melatonine$TST_ACT, col="darkorange", ylab="Tiempo total de sueño - TST (minutos)")
boxplot(melatonine$SOL_ACT, col="darkorange", ylab="Latencia de inicio de sueño - SOL (minutos)")
boxplot(melatonine$SE_ACT, col="darkorange", ylab="Eficiencia del sueño - SE (%)")
boxplot(melatonine$SET1_ACT, col="darkorange", ylab="Eficiencia del sueño 1er tercil - SET1 (%)")
boxplot(melatonine$SET2_ACT, col="darkorange", ylab="Eficiencia del sueño 2do tercil- SET2 (%)")
boxplot(melatonine$SET3_ACT, col="darkorange", ylab="Eficiencia del sueño 3er tercil - SET3 (%)")

x11(50,30); # Sistema Operativo Linux
# windows(50, 30); # Sistema operativo Windows
par(mfrow=c(2,4))
hist(melatonine$TIB_ACT, breaks=30, main="Tiempo acostado", xlab="TIB (minutos)", ylab="Frecuencia")
