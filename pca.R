library(readxl)
library(ggplot2)
library(ggbiplot)

setwd("/home/nadia/unsl/bio/code/unsl-bio/data")

melatonine_participante <- read_excel("pmed.1002587.s005.xlsx", sheet = "Combined", range="A1:G3735")
melatonine_fecha_actigrafo <- read_excel("pmed.1002587.s005.xlsx", sheet = "Combined", range="AG1:AG3735")
melatonine_actigrafo <- read_excel("pmed.1002587.s005.xlsx", sheet = "Combined", range="AM1:AT3735")
melatonine <- cbind(melatonine_participante, melatonine_fecha_actigrafo, melatonine_actigrafo)
melatonine$anio_mes <- format(melatonine$Date_Onset_ACT, "%Y%m")

melatonine$Treatment <- factor(melatonine$Treatment)
melatonine$StudyPeriod <- factor(melatonine$StudyPeriod)

str(melatonine)

head(melatonine)

melatonine_num <- melatonine[, c("TIB_ACT", "TST_ACT", "SOL_ACT", "SE_ACT", "WASO_ACT", "SET1_ACT", "SET2_ACT", "SET3_ACT")]
summary(melatonine_num)

x <- na.omit(melatonine_num)
pca <- prcomp(x, scale = T, center = T)

summary(pca)
ggbiplot(pca, obs.scale = 1, var.scale = 1,
                   # groups = iris$Species, 
                   point.size=1,
                   varname.size = 3, 
                   varname.color = "red",
                   varname.adjust = 1.2,
                   ellipse = F, 
                   circle = F) +
  # labs( color = "Species") +
  theme_minimal(base_size = 20) 
