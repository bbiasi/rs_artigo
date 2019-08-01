setwd("D:/R/Project/Residuos_Solidos/30_06_19/rs_artigo")

library(readxl)
df_2010 <- readxl::read_excel("2010.xlsx")

df_2016 <-  readxl::read_excel("2016.xlsx")

residuos <- readxl::read_excel("residuos.xlsx")
