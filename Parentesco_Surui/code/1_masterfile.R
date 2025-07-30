
################################################## Preámbulo #################################################

# Limpiar pantalla y remover objetos existentes
cat("\014") 
rm(list = ls())

# Carga paquetes

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(igraph)
  library(network)
  library(intergraph)
  library(sna)
  library(statnet)
})

############################################# Importar datos ################################################# 

individuals <- read_excel("data/kinsources-surui.iur.xls", sheet = "Individuals")
families <- read_excel("data/kinsources-surui.iur.xls", sheet = "Families")

red_network <- read.paj("data/kinsources-surui-pgraph.paj")

############################################ Recodificación #################################################

source("code/2_recod_families.R")
source("code/3_recod_red.R")

############################################## Data analysis #################################################

source("code/4_analyses.R")
