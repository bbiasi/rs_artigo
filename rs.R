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
# ATT = area de transbordo e triagem

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
library(forcats)
library(stringr)
library(tidyr)

Bahia <- FluxoResiduos %>% 
  tibble::rowid_to_column() %>% 
  dplyr::mutate(origem_estado = stringr::str_sub(`UP025 - Municípios de origem dos resíduos`, start = -2)) %>% 
  dplyr::filter(Estado == "BA") %>% 
  dplyr::select(-c(`Código do Município`, Estado)) %>% 
  dplyr::rename("Domiciliar_urbano" = `UP007 - Quantidade de RDO e RPU recebida na unidade de processamento`,
                "Saude" = `UP008 - Quantidade de RSS recebida na unidade de processamento`,
                "Industrial"  = `UP009 - Quantidade de RIN recebida na unidade de processamento`,
                "Const_civil" = `UP010 - Quantidade de RCC recebida na unidade de processamento`,
                "Outros" = `UP011 - Quantidade de outros tipos de resíduos recebida na unidade de processamento`,
                "Municipio" = Município,
                "Ano" = `Ano de Referência`,
                "Municipio_origem" = `UP025 - Municípios de origem dos resíduos`,
                "Op_Unidade" = `UP004 - Operador da unidade`,
                "Nome_und" = `Nome da Unidade`,
                "Tipo_und" = `UP003 - Tipo de unidade`,
                "poda" = `UP067 - Quantidade de RPO recebida na unidade de processamento`,
                "QTD_receb_municpio_und_proc"= `UP080 - Quantidade total de resíduos recebida na unidade de processamento por cada município`) %>% 
  dplyr::mutate(Municipio = as.factor(Municipio),
                Unidades  = as.factor(Unidades),
                Ano = as.factor(Ano),
                Op_Unidade = as.factor(Op_Unidade),
                Nome_und = as.factor(Nome_und),
                Municipio_origem = as.factor(Municipio_origem),
                Tipo_und = as.factor(Tipo_und),
                origem_estado = as.factor(origem_estado)) %>%
  dplyr::mutate(Tipo_und = forcats::fct_recode(Tipo_und,
                                               att_rcc_vol = "Área de transb e triagem de RCC e volumosos (=ATT)",
                                               aterro = "Aterro controlado",
                                               aterro_rcc = "Aterro de Resíduos da Construção Civil (=inertes)",
                                               aterro = "Aterro sanitário",
                                               autoclave = "Unid. tratamento por microondas ou autoclave",
                                               compostagem  = "Unidade de compostagem (pátio ou usina)",
                                               galhos_podas = "Unidade de manejo de galhadas e podas",
                                               transbordo  = "Unidade de transbordo",
                                               incineracao = "Unidade de tratamento por incineração",
                                               triagem  = "Unidade de triagem (galpão ou usina)",
                                               vala_rss = "Vala especifica de RSS"),
                Op_Unidade = forcats::fct_recode(Op_Unidade,
                                                 catadores = "Associação de catadores",
                                                 emp_priv  = "Empresa privada",
                                                 municipal = "Prefeitura ou SLU"))


# QTD_receb_municpio_und_proc = soma de residuos

levels(Bahia$Op_Unidade)
levels(Bahia$origem_estado)

table(Bahia$Ano)
#  A partir de 2009 ha um amplo incremento de informacao.
# Deste modo a analise sera focada a partir do ano de 2009.

Bahia <- Bahia %>% 
  dplyr::mutate(Ano = as.numeric(as.character(Ano))) %>% 
  dplyr::filter(Ano >= 2009) %>% 
  dplyr::mutate(Ano = as.factor(Ano))

table(Bahia$Ano)
table(Bahia$Tipo_und)
table(Bahia$origem_estado)

Bahia %>% 
  na.omit() %>% 
  ggplot2::ggplot() +
  geom_bar(aes(x = Op_Unidade, 
               y = Domiciliar_urbano,
               fill = Tipo_und),
           stat = "identity") +
  facet_wrap(~Ano, scales = "free") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

Alagoinhas <- Bahia %>% 
  dplyr::filter(Municipio == "Alagoinhas" | Municipio_origem == "Alagoinhas/BA")

Alagoinhas %>% 
  na.omit() %>% 
  ggplot2::ggplot() +
  geom_bar(aes(x = Op_Unidade, 
               y = Domiciliar_urbano,
               fill = Tipo_und),
           stat = "identity") +
  facet_wrap(~Ano, scales = "free") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

Alagoinhas %>% 
  na.omit() %>% 
  dplyr::filter(Tipo_und == "aterro") %>% 
  ggplot2::ggplot() +
  geom_bar(aes(x = Ano, 
               y = Domiciliar_urbano,
               fill = Op_Unidade),
           stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

outros_estados <- Bahia %>% 
  dplyr::filter(origem_estado != "BA") %>% 
  replace(is.na(.), 0) %>% 
  dplyr::filter(QTD_receb_municpio_und_proc > 0) %>% # remove petrolina
  dplyr::select(Ano, Municipio, Nome_und, Saude, Industrial, Outros, 
                Municipio_origem, Tipo_und, Op_Unidade) %>% 
  tidyr::gather("residuo", "tonelada", -c(Ano, Municipio, Nome_und, Municipio_origem,
                                          Tipo_und, Op_Unidade))

outros_estados %>% 
  ggplot2::ggplot() +
  geom_bar(aes(x = Ano, y = tonelada, fill = residuo),
           stat = "identity") +
  theme_bw()

outros_estados %>% 
  ggplot2::ggplot() +
  geom_bar(aes(x = Ano, y = tonelada, fill = residuo),
           stat = "identity") +
  facet_grid(Municipio~Municipio_origem) +
  theme_bw()




