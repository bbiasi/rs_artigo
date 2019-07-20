setwd("D:/R/Project/Residuos_Solidos/30_06_19/rs_artigo")

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

# Glossario
# http://snis.gov.br/glossarios

# RDO = residuos solidos domiciliares
# RPU = residuos solidos urbanos
# RSS = residuos de saude
# RCC = residuos de construcao civil
# RIN = residuos industriais
# RPO = resíduos dos serviços de podas de arvores
# SLU = limpeza urbana publica (servico, superintendencia, etc.)

# importando dados
library(readxl)
FluxoResiduos <- readxl::read_excel("FluxoResiduos.xlsx")

# visualizando NA ----
library(naniar)
naniar::gg_miss_var(FluxoResiduos)
naniar::vis_miss(FluxoResiduos)

# ----
library(dplyr)
library(tibble)
library(ggplot2)

Bahia <- FluxoResiduos %>% 
  tibble::rowid_to_column() %>% 
  dplyr::filter(Estado == "BA") %>% 
  dplyr::select(-c(`Código do Município`, Estado)) %>% 
  dplyr::rename("Domiciliar_urbano" = `UP007 - Quantidade de RDO e RPU recebida na unidade de processamento`,
                "Saude" = `UP008 - Quantidade de RSS recebida na unidade de processamento`,
                "Industrial" = `UP009 - Quantidade de RIN recebida na unidade de processamento`,
                "Const_civil" = `UP010 - Quantidade de RCC recebida na unidade de processamento`,
                "Outros" = `UP011 - Quantidade de outros tipos de resíduos recebida na unidade de processamento`,
                "Municipio" = Município,
                "Ano" = `Ano de Referência`,
                "Municipio_origem" = `UP025 - Municípios de origem dos resíduos`)

Alagoinhas <- Bahia %>% 
  dplyr::filter(Municipio == "Alagoinhas" | Municipio_origem == "Alagoinhas/BA")

Bahia %>% 
  na.omit() %>% 
  ggplot2::ggplot() +
  geom_boxplot(aes(x = Ano, 
                   y = Domiciliar_urbano,
                   fill = `UP003 - Tipo de unidade`)) +
  facet_wrap(~`UP004 - Operador da unidade`, scales = "free") +
  theme_classic() +
  theme(axis.text.x = element_blank())

