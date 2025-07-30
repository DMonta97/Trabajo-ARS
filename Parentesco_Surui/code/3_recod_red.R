# Explorar datos

#list.vertex.attributes(red_network)
#red_network %v% "vertex.names"

red_network %v% "vertex.names" <- as.character(red_network %v% "vertex.names")
families_recod$vertex.names <- as.character(families_recod$vertex.names)

nodos_red <- data.frame(
  vertex.names = red_network %v% "vertex.names",
  stringsAsFactors = FALSE
)

nodos_con_atributos <- nodos_red %>%
  left_join(families_recod, by = "vertex.names")

nodos_sin_match <- nodos_con_atributos %>%
  filter(is.na(Id)) %>%
  pull(vertex.names)

# Eliminar nodos sin match
surui_net <- delete.vertices(
  red_network,
  which(red_network %v% "vertex.names" %in% nodos_sin_match)
)

#list.vertex.attributes(surui_net)

# Asignar Atributos
atributos_finales <- families_recod %>%
  filter(vertex.names %in% (surui_net %v% "vertex.names"))

atributos_finales <- atributos_finales[
  match(surui_net %v% "vertex.names", atributos_finales$vertex.names),
]

for (attr in c("Intergen", "Mat_Tip1", "Mat_Tip2", "Num_Hijos", "Gen_Padre", "Gen_Madre")) {
  surui_net %v% attr <- atributos_finales[[attr]]
}

#list.vertex.attributes(red_surui)
#red_surui %v% "Dist_Generacional"

# elegir submuestra en base a los componentes:

componentes <- component.dist(surui_net, connected = "weak")

tamanos <- sort(table(componentes$membership), decreasing = TRUE)
componente_elegido <- as.integer(names(tamanos)[1])

nodos_componente <- which(componentes$membership == componente_elegido)
surui_net <- get.inducedSubgraph(surui_net, v = nodos_componente)

rm(atributos_finales, nodos_con_atributos, nodos_red, red_network, attr, nodos_sin_match,
   families_recod, componentes, tamanos, componente_elegido, nodos_componente)

nombres_originales <- surui_net %v% "vertex.names"
nuevos_nombres <- paste0("Surui_", sprintf("%03d", 1:network.size(surui_net)))

correspondencia_nombres <- data.frame(
  original = nombres_originales,
  nuevo = nuevos_nombres,
  stringsAsFactors = FALSE
)

surui_net %v% "vertex.names" <- nuevos_nombres
assign("correspondencia_nombres", correspondencia_nombres, envir = .GlobalEnv)

rm(nombres_originales, nuevos_nombres)

print("================ RECODIFICIACIÓN LISTA (2) !!!! ====================") # Debugging flags
