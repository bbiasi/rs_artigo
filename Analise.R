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
  na.omit() %>%          
  base::scale()

res.nbclust <- NbClust(my_data, distance = "euclidean",
                       min.nc = 2, max.nc = 10, 
                       method = "complete", index ="all")

factoextra::fviz_nbclust(res.nbclust) + theme_minimal()

# validacao do clustering
library(clValid)

clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(my_data, nClust = 2:10,
                  clMethods = clmethods, validation = "internal")

summary(intern)
# plot(intern)

# Stability measures
stab <- clValid(my_data, nClust = 2:10, clMethods = clmethods,
                validation = "stability")

summary(stab)


# dendrograma ----
df_analise <- df1 %>% 
  na.omit() %>%          
  base::scale() %>% 
  stats::dist(method   = "euclidean") %>% 
  stats::hclust(method = "ward.D")

dend_cidade <- factoextra::fviz_dend(df_analise)
dend_cidade

dend_cidade <- factoextra::fviz_dend(df_analise, k = 8)
dend_cidade

df_cluster <- df_analise %>% 
  stats::cutree(k = 8) %>% 
  data.frame()

# descritiva ----
desc <- df %>% 
  dplyr::mutate(cluster = df_cluster$.)


library(highcharter)

highcharter::hchart(desc, "scatter", 
                    highcharter:::hcaes(x = Domiciliar_urbano_m, 
                                        y = IDHM, 
                                        group = cluster))

highcharter::hchart(desc, "scatter", 
                    highcharter:::hcaes(x = Domiciliar_urbano_m, 
                                        y = pib_pc_2016, 
                                        group = cluster))

# library(igraph)
# N <- nrow(df)
# 
# net <- sample_gnp(N, p = 2/N)
# wc <- cluster_walktrap(net)
# 
# V(net)$label <- seq(N)
# V(net)$name <- paste("I'm #", seq(N))
# V(net)$page_rank <- round(page.rank(net)$vector, 2)
# V(net)$betweenness <- round(betweenness(net), 2)
# V(net)$degree <- degree(net)
# V(net)$size <- V(net)$degree
# V(net)$comm <- membership(wc)
# V(net)$color <- colorize(membership(wc))
# 
# hchart(net, layout = layout_with_fr)


library(ggplot2)
library(ggiraph)

g <- ggplot2::ggplot(desc, aes(x = Domiciliar_urbano_m, 
                               y = pib_pc_2016,
                               color = renda_pc_2010))

my_gg <- g + geom_point_interactive(aes(tooltip = cluster), size = 2) 
ggiraph::girafe(code = print(my_gg))

my_gg <- g + geom_point_interactive(aes(tooltip = Municipio), size = 2) 
ggiraph::girafe(code = print(my_gg))


g <- ggplot2::ggplot(desc) +
  geom_point(aes(x = Domiciliar_urbano_m, 
                 y = pib_pc_2016,
                 color = Pop_est_2016,
                 size = IDHM),
             alpha = 0.5, show.legend = F) +
  facet_wrap(~cluster)
g

library(plotly)
plotly::ggplotly(g)


