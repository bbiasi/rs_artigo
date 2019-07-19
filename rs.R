setwd("D:/R/Residuos_Solidos/30_06_19")

# SNIS - Série Histórica
# Sistema Nacional de Informações sobre Saneamento
# 
# http://app4.cidades.gov.br/serieHistorica/#
#   
#   Fluxo e quantidade de resíduos
# Já essa busca tem como base do dado o fluxo e a quantidade de resíduo. 
# Permite filtrar por Ano de referência, Região, Estado, Tipo da unidade, 
# Operador da unidade, Município de Origem e Unidade de processamento.

# ano de referencia: 2002 ~ 2017
# https://imgur.com/APMiMdK
# https://imgur.com/LuG98Kg

# importando dados
library(readxl)
FluxoResiduos <- readxl::read_excel("FluxoResiduos.xlsx")

# visualizando NA
library(naniar)
naniar::gg_miss_var(FluxoResiduos)
naniar::vis_miss(FluxoResiduos)

library(dplyr)
library(tibble)
df <- FluxoResiduos %>% 
  tibble::rowid_to_column()

df1 <- df %>% 
  dplyr::filter(Município == "Alagoinhas")

df2 <- df %>% 
  dplyr::filter(`UP025 - Municípios de origem dos resíduos` == "Alagoinhas/BA")

df3 <- rbind(df1, df2)

df3 <- df3 %>% 
  dplyr::filter (! duplicated(rowid))

