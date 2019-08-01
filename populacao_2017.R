setwd("D:/R/Project/Residuos_Solidos/30_06_19/rs_artigo")

library(readxl)
pop_2017 <- readxl::read_excel("D:/R/Project/Residuos_Solidos/30_06_19/rs_artigo/IBGE/estimativa_dou_2017.xlsx", 
                               sheet = "Municípios", skip = 1)

# cleaner ----
library(dplyr)
levels(pop_2017$UF)

df <- pop_2017 %>% 
  dplyr::mutate(UF = as.factor(UF))

levels(df$UF)

df <- df %>% 
  dplyr::filter(UF == "BA") %>% 
  dplyr::rename("Municipio" = `NOME DO MUNICÍPIO`,
                "Pop_est_2017"  = `POPULAÇÃO ESTIMADA`) %>% 
  dplyr::select(Municipio, Pop_est_2017)
# View(df)

# save ----
write.table(df, file = "Pop_est_2017.txt", sep = "")
