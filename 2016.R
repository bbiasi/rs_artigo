setwd("D:/R/Project/Residuos_Solidos/30_06_19/rs_artigo")

library(readxl)
pop_2016 <- readxl::read_excel("IBGE/estimativa_dou_2016_20160913.xlsx", 
                               sheet = "Municípios", skip = 2)

# cleaner ----
library(dplyr)
levels(pop_2016$UF)

df <- pop_2016 %>% 
  dplyr::mutate(UF = as.factor(UF))

levels(df$UF)

df <- df %>% 
  dplyr::filter(UF == "BA") %>% 
  dplyr::rename("Municipio" = `NOME DO MUNICÍPIO`,
                "Pop_est_2016"  = `POPULAÇÃO ESTIMADA`) %>% 
  dplyr::select(Municipio, Pop_est_2016)
# View(df)

# save ----
write.table(df, file = "2016.txt", sep = "")
