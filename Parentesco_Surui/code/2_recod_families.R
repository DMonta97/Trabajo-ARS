
# Preparar datos de individuos
ind_dict <- individuals %>%
  select(Id, Clan1 = Clan1, Generation = Generation_Clan1, Ethnie = Ethnie)

# Función para determinar los atributos de cada familia
calculate_family_attributes <- function(family_row, ind_data) {
  father_id <- family_row$FatherId
  mother_id <- family_row$MotherId
  
  father_data <- ind_data %>% filter(Id == father_id)
  mother_data <- ind_data %>% filter(Id == mother_id)
  
  # Valores por defecto (numéricos)
  intergen <- 0L       # 0 = Desconocido
  tipo_mat1 <- 0L      # 0 = Desconocido, 1 = Endogamia, 2 = Exogamia
  tipo_mat2 <- 0L      # 0 = Desconocido, 1 = Homogamia, 2 = Heterogamia
  num_hijos <- 0L
  generacion_padre <- NA_integer_
  generacion_madre <- NA_integer_
  
  # Calcular atributos cuando hay datos disponibles
  if (nrow(father_data) > 0 && nrow(mother_data) > 0) {
    # Generaciones (se mantienen como NA si no hay datos)
    generacion_padre <- father_data$Generation
    generacion_madre <- mother_data$Generation
    
    # Intergeneracionalidad (convertida a numérico)
    if (!is.na(generacion_padre) && !is.na(generacion_madre)) {
      intergen <- ifelse(generacion_padre == generacion_madre, 1L, 2L) # 1=Isogen, 2=Anisogen
    }
    
    # Exogamia/Endogamia (numérico)
    if (!is.na(father_data$Clan1) && !is.na(mother_data$Clan1)) {
      tipo_mat1 <- ifelse(father_data$Clan1 == mother_data$Clan1, 1L, 2L) # 1=Endogamia, 2=Exogamia
    }
    
    # Homogamia/Heterogamia (numérico)
    if (!is.na(father_data$Ethnie) && !is.na(mother_data$Ethnie)) {
      tipo_mat2 <- ifelse(father_data$Ethnie == mother_data$Ethnie, 1L, 2L) # 1=Homogamia, 2=Heterogamia
    }
  }
  
  # Número de hijos (se mantiene igual)
  if (!is.na(family_row$Children)) {
    num_hijos <- length(str_split(family_row$Children, ";")[[1]])
  }
  
  # Retornar todos los atributos como data.frame
  return(data.frame(
    Intergen = intergen,
    Mat_Tip1 = tipo_mat1,
    Mat_Tip2 = tipo_mat2,
    Num_Hijos = num_hijos,
    Gen_Padre = generacion_padre,
    Gen_Madre = generacion_madre,
    stringsAsFactors = FALSE
  ))
}

nuevos_atributos <- families %>%
  rowwise() %>%
  do(calculate_family_attributes(., ind_dict)) %>%
  ungroup()

families_recod <- bind_cols(families, nuevos_atributos)
rm(families, ind_dict, individuals, nuevos_atributos, calculate_family_attributes)

families_recod <- families_recod %>%
  mutate(vertex.names = paste0(Id, " H ", FatherId, " (", FatherId, ") & F ", MotherId, " (", MotherId, ")"))

print("================ RECODIFICIACIÓN LISTA (1) !!!! ====================") # Debugging flags
