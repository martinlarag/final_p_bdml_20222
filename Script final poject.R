rm(list = ls())

setwd("C:/Users/mlara/Desktop/BD&ML/Proyecto Final/Data")

if (!require(pacman))install.packages("pacman");library(pacman)

p_load(tidyverse,rio,skimr,viridis, haven
       gstat, ## variogram
       sf, ## leer/escribir/manipular datos espaciales
       leaflet, ## Visualizaciones dinámicas
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list 
       osmdata,
       stringr,
       dplyr,
       readr,
       purrr)

cg <- read_dta("PANEL_CARACTERISTICAS_GENERALES(2021).dta")
skim(cg)

bg <- read_dta("PANEL_BUEN_GOBIERNO(2021).dta")
skim(bg)
