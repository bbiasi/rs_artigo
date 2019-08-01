setwd("D:/R/Project/Residuos_Solidos/30_06_19/rs_artigo")

library(readxl)
idh_2010 <- readxl::read_excel("D:/R/Project/Residuos_Solidos/30_06_19/rs_artigo/IBGE/IPEA/IDH_2010.xlsx")


# cleaner ----
levels(idh_2010$`Nome da Unidade da Federação`)

library(dplyr)
df <- idh_2010 %>% 
  dplyr::mutate(UF = as.factor(`Nome da Unidade da Federação`))

levels(df$UF)

df <- df %>% 
  dplyr::filter(UF == "Bahia") %>% 
  dplyr::select(Município, IDHM, `Renda per capita`, `População total`) %>% 
  dplyr::rename("Municipio" = Município,
                "renda_pc_2010" = `Renda per capita`,
                "pop_2010" = `População total`)

# save ----
write.table(df, file = "2010.txt", sep = "")
