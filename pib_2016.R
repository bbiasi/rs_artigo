setwd("D:/R/Project/Residuos_Solidos/30_06_19/rs_artigo")

library(readxl)
pib_2016<- readxl::read_excel("IBGE/SEI_BA/pib_2016_retificada.xlsx", skip = 4)

# cleaner ----
library(dplyr)
df <- pib_2016 %>% 
  na.omit() %>% 
  dplyr::slice(-1) %>% 
  dplyr::select(Municípios, 8)

colnames(df) <- c("Municipios", "pib_pc_2016")

# save ----
library("xlsx")
write.xlsx(df, file = "2016_pib_pc.xlsx")
