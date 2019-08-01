setwd("D:/R/Project/Residuos_Solidos/30_06_19/rs_artigo")

library(readxl)
df_2010 <- readxl::read_excel("2010.xlsx")

df_2016 <- readxl::read_excel("2016.xlsx")
pib_pc_2016 <- readxl::read_excel("2016_pib_pc.xlsx")

residuos <- readxl::read_excel("residuos.xlsx")

# cleaner ----
library(dplyr)
library(stringr)

df_2010 <- df_2010 %>% 
  dplyr::select(-...1) %>% 
  dplyr::mutate_if(is.character, stringr::str_to_upper)

df_2016 <- df_2016 %>% 
  dplyr::select(-...1) %>% 
  dplyr::mutate_if(is.character, stringr::str_to_upper)

pib_pc_2016 <- pib_pc_2016 %>% 
  dplyr::select(-...1) %>% 
  dplyr::mutate_if(is.character, stringr::str_to_upper)

residuos <- residuos %>% 
  dplyr::select(-...1) %>% 
  dplyr::mutate_if(is.character, stringr::str_to_upper)

# join ----

df <- residuos %>% 
  dplyr::left_join(df_2010, by = "Municipio") %>% 
  dplyr::left_join(df_2016, by = "Municipio") %>% 
  dplyr::left_join(pib_pc_2016, by = "Municipio")

dplyr::glimpse(df)

df1 <- df %>% 
  dplyr::select(-Municipio) %>% 
  dplyr::mutate(Pop_est_2016 = as.numeric(as.factor(Pop_est_2016)))

dplyr::glimpse(df1)

# clustering ----
library(NbClust)
library(factoextra)

my_data <- df1 %>% 
  scale()

res.nbclust <- NbClust(my_data, distance = "euclidean",
                       min.nc = 2, max.nc = 10, 
                       method = "complete", index ="all")

factoextra::fviz_nbclust(res.nbclust) + theme_minimal()


