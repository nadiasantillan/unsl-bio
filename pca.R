library(readxl)

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

pca <- prcomp(na.omit(melatonine_num), scale = T, center = T)

names(pca)
pca $scale
biplot(pca)
