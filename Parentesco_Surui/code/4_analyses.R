#################### Basic data analysis  #################### 
surui_igr <- asIgraph(surui_net)
V(surui_igr)$name <- V(surui_igr)$vertex.names

vertex_attr_names(surui_igr)
vertex_attr(surui_igr, "vertex.names")
# Densidad
densidad <- igraph::edge_density(surui_igr)
# Diámetro
diametro <- igraph::diameter(surui_igr, directed = TRUE)
# Número de nodos
n_nodos <- igraph::vcount(surui_igr)
# Número de vinculos
n_vinculos <- igraph::ecount(surui_igr)
# Grado medio (degree)
degree_medio <- mean(igraph::degree(surui_igr))
# Grado medio (out-degree)
outdegree_medio <- mean(igraph::degree(surui_igr, mode = "out"))
# Grado medio (in-degree)
indegree_medio <- mean(igraph::degree(surui_igr, mode = "in"))

# Centralidad Katz
katz_scores <- igraph::alpha_centrality(surui_igr) # en reemplazo a eigenvector ya que no sirve para redes directas
katz <- V(surui_igr)$name[which.max(katz_scores)]
# Betweenness
betweenness_scores <- igraph::betweenness(surui_igr, directed = TRUE)
betweenness <- V(surui_igr)$name[which.max(betweenness_scores)]
# Closeness
closeness_scores_out <- igraph::closeness(surui_igr, mode = "out")
closeness_scores_in <- igraph::closeness(surui_igr, mode = "in")
closeness_out <- V(surui_igr)$name[which.max(closeness_scores_out)]
closeness_in <- V(surui_igr)$name[which.max(closeness_scores_in)]

resultados <- list(
  "Densidad" = round(densidad, 3),
  "Diámetro" = round(diametro, 3),
  "N Nodos" = n_nodos,
  "N Vínculos" = n_vinculos,
  "Degree (mean)" = round(degree_medio, 2),
  "Degree out (mean)" = round(outdegree_medio, 2),
  "Degree in (mean)" = round(indegree_medio, 2),
  "Centralidad Katz" = katz,
  "Centralidad Betweenness" = betweenness,
  "Centralidad Closeness (out)" = closeness_out,
  "Centralidad Closeness (in)" = closeness_in
  )

tabla_metricas <- data.frame(
  Medida = names(resultados),
  Valor = unlist(resultados),
  row.names = NULL,
  stringsAsFactors = FALSE
)

rm(betweenness, betweenness_scores, closeness_out, closeness_in, closeness_scores_in, closeness_scores_out, degree_medio,
   densidad, diametro, indegree_medio, katz, katz_scores, n_nodos, n_vinculos,
   outdegree_medio)
#################### Plots  ####################
if(FALSE) {
plot(surui_net)
# Visualizacion 1
plot(surui_igr,
     vertex.label = NA,
     vertex.label.color = "black",
     vertex.size=c(5),
     edge.width = 0.5,                
     edge.arrow.size = 0.3,           
     edge.color = "gray70")
title("Flujo de Alianzas Matrimoniales Suruí")

# Visualizacion 2
V(surui_igr)$color1 <- case_when(
  V(surui_igr)$Mat_Tip1 == 1 ~ "#CD2626",     
  V(surui_igr)$Mat_Tip1 == 2 ~ "#1874CD",    
  V(surui_igr)$Mat_Tip1 == 3 ~ "#808080",  
  TRUE ~ "#FFFFFF")

plot(surui_igr,
     vertex.label = NA,
     vertex.color = V(surui_igr)$color1,
     vertex.size=c(5),
     edge.width = 0.5,                
     edge.arrow.size = 0.3,           
     edge.color = "gray70")
legend("topright",
       legend = c("Endogamia", "Exogamia", "Desconocido"),
       fill = c("#CD2626", "#1874CD", "#808080"),
       title = "Tipo de Unión",
       bty = "n")
title("Flujo de Alianzas Matrimoniales Suruí\nEndogamia-Exogamia (Clan)")

# Visualizacion 3

V(surui_igr)$color2 <- case_when(
  V(surui_igr)$Mat_Tip2 == 1 ~ "#FFC125",    
  V(surui_igr)$Mat_Tip2 == 2 ~ "#00BFFF",    
  V(surui_igr)$Mat_Tip2 == 3 ~ "#808080",  
  TRUE ~ "#FFFFFF")

plot(surui_igr,
     vertex.label = NA,
     vertex.color = V(surui_igr)$color2,
     vertex.size=c(5),
     edge.width = 0.5,                
     edge.arrow.size = 0.3,           
     edge.color = "gray70")
legend("topright",
       legend = c("Homogamia", "Heterogamia", "Desconocido"),
       fill = c("#FFC125", "#00BFFF", "#808080"),
       title = "Tipo de Unión",
       bty = "n")
title("Flujo de Alianzas Matrimoniales Suruí\nHomogamia-Heterogamia (Etnia)")
}
################ Modelos de Regresión ################## 
#list.vertex.attributes(surui_net)
#surui_net %v% "Gen_Padre"

modelo_1 <- ergm(surui_net ~ edges +
                   nodeicov("Num_Hijos") +
                   nodeocov("Num_Hijos") +
                   nodematch("Intergen", levels = 1:2) + # excluí Desconocido
                   gwesp(0.5, fixed = TRUE))

modelo_2 <- ergm(surui_net ~ edges +
                   nodeicov("Num_Hijos") +
                   nodeocov("Num_Hijos") +
                   nodematch("Mat_Tip1", levels = 1:2) + # excluí Desconocido
                   gwesp(0.5, fixed = TRUE))

modelo_3 <- ergm(surui_net ~ edges +
                   nodeicov("Num_Hijos") +
                   nodeocov("Num_Hijos") +
                   nodematch("Mat_Tip1", levels = 1:2) + # excluí Desconocido
                   nodematch("Mat_Tip2", levels = 1:2) + # excluí Desconocido
                   nodematch("Intergen", levels = 1:2) + # excluí Desconocido
                   gwesp(0.5, fixed = TRUE))
if(FALSE) {
summary(modelo_1)
summary(modelo_2)
summary(modelo_3)
}
print("================ ANÁLISIS LISTOS !!!! ====================") # Debugging flags


