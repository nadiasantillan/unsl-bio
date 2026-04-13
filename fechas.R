library(readxl)
library(ggplot2)
library(dplyr)

setwd("/home/nadia/unsl/bio/code/unsl-bio/data")
dataset_name <- "pmed.1002587.s005.xlsx"

melatonine_participante <- read_excel(dataset_name, sheet = "Combined", range="A1:G3735")
melatonine_fecha_actigrafo <- read_excel(dataset_name, sheet = "Combined", range="AG1:AG3735")
melatonine_actigrafo <- read_excel(dataset_name, sheet = "Combined", range="AM1:AT3735")
melatonine <- cbind(melatonine_participante, melatonine_fecha_actigrafo, melatonine_actigrafo)
melatonine$anio_mes <- format(melatonine$Date_Onset_ACT, "%Y%m")

meses <- as.data.frame(table(melatonine$anio_mes))

x11(20,10);
ggplot(meses, aes(x=Var1, y=Freq)) + 
  geom_col(color="white", fill="paleturquoise4") +
  labs(x = "Mes", y = "Frecuencia", title = "Meses abarcados por el estudio")

fechas_por_participante <-melatonine |>
  filter(StudyPeriod == "Treatment") |>
  select(ParticipantID, Date_Onset_ACT) |>
  na.omit() |>
  group_by(ParticipantID) |> 
  summarise(
    maximum = max(Date_Onset_ACT),
    minimum = min(Date_Onset_ACT),
    n = length(ParticipantID)
  )
fechas_por_participante$dias <- as.numeric(fechas_por_participante$maximum - fechas_por_participante$minimum)
fechas_por_participante[order(-fechas_por_participante$dias),]
