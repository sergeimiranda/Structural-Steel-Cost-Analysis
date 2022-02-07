##TITLE: Script for Data Analysis from Structural Steel Provider
##AUTHOR: Sergio Miranda (sergeimiranda@hotmail.com)
##DATE: 04-02-2022

library(dplyr)
source("Metal_ETL.r")

##Loading file and getting processed data
  ##INPUT DATA
  Filename <- "2022-01-04_Lista de Precios-Carlos-Isla.xlsx"
  DolarBNA_220104 <- 108
  
  ###Variables Setting
  MetalData <- Metal_ETL(Filename)
  DolarValue <- DolarBNA_220104

####ANALYSIS 
  ##Price/weight ratio
  MetalData$Precio_uss <- MetalData$Precio/DolarValue
  MetalData$PrecioxPeso_uss <- MetalData$Precio_uss/MetalData$Peso
