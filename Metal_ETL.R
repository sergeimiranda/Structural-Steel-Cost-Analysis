##Data Processing Function. Part of Metal_analysis.r script

library(dplyr)
library(readxl)
library(stringr)

Metal_ETL <- function (Filename = "2022-01-04_Lista de Precios-Carlos-Isla.xlsx") {
  
##Loading file
  if (file.exists(Filename)){
        pricedata <- read_xlsx(Filename, skip = 4)
  } else {
    print("FILE DOES NOT EXISTS")
    return()
  }
  

##DATA CLEANING
  ##Removing NA files and keeping only needed columns 
  pricedata <- pricedata[!is.na(pricedata[,1]),
                   c("Artículo","Descripción","Precio","Precio c/IVA")]
  ##Renaming columns
  pricedata <- rename(pricedata, "Articulo" = "Artículo", 
                    "Descripcion" = "Descripción" , "Precio_IVA" = "Precio c/IVA")

##DATA TRANSFORMATION
  ##Making data tidy. Organizing blocks as categories
  categoria <- character()
  pricedata$Categoria <- NA
  
  for (i in 1:dim(pricedata)[1]) {
        if (is.na(pricedata$Precio[i])) {
                categoria <- pricedata$Descripcion[i]
        }
        pricedata$Categoria[i] <- categoria
  }
  ##Removing files used for categories
  pricedata <- pricedata[!is.na(pricedata$Precio),]
  
  ##Select the metalic products categories
  metal_pricedata <- pricedata[grepl("PROD.MET",pricedata$Categoria),]
  
  ##Removes "PROD.MET. = " from Categories
  metal_pricedata$Categoria <- gsub("PROD.MET. = ","",metal_pricedata$Categoria)
 
 
  #####PROCESSING 
  metal_pricedata$Seccion <- NA
  metal_pricedata$Dimension <- NA
  metal_pricedata$Espesor <- NA
  metal_pricedata$Peso <- NA
  metal_pricedata$Longitud <- NA
  
  for (i in 1:dim(metal_pricedata)[1]) {
    Seccion <- NA
    Dimension <- NA
    Espesor <- NA
    Peso <- NA
    Longitud <- NA
    
    ##TUBOS ESTRUCTURALES
      Selection <- "TUBOS ESTRUCTURALES" 
      if (grepl(Selection,metal_pricedata$Categoria[i])) {
        Seccion <- str_extract(metal_pricedata$Descripcion[i] , ".*?(?=[[:digit:]])")  ## ej: "CUADRADO "
        Seccion <- str_trim(Seccion)
        Medida <- str_extract(metal_pricedata$Descripcion[i] , "[[:digit:]].*(?=mm)") ## ej: "10 x 10 x 0.9"
        Medida <- str_split_fixed(Medida,"[X|x]",n=Inf) # Splits 
        Medida <- str_trim(Medida)
        if(Seccion != "REDONDO"){    ##checks whether tubes are not round
          Espesor <- Medida[3]  # Extracts the thickness
          Dimension <- paste(Medida[1],"x",Medida[2],"x",Espesor,sep = " ")  
        } else {
          Espesor <- Medida[2]  # Extracts the thickness
          Dimension <- paste(Medida[1],"x",Espesor,sep = " ")
        }
        Peso <- str_extract(metal_pricedata$Descripcion[i] , "(?<=mm).*[[:digit:]].*(?=Kg)")  ## ej: "    1.60"
        Longitud <- 6
      }
    ########
    
    ##HIERRO T
      Selection <- "HIERRO T" 
      if (metal_pricedata$Categoria[i]== Selection) {
        Seccion <- "PERFIL T"
        Medida <- str_split_fixed(metal_pricedata$Descripcion[i],"[X|x]",n=Inf)
        Medida <- str_trim(Medida)
        Espesor <- str_split_fixed(Medida[2],"([[:blank:]]*) ",n=Inf)[1]
        Dimension <- str_extract(Medida[1] , "([[:digit:]].*)") 
        Dimension <- paste(Dimension,"x",Espesor,sep = " ")
        Peso <- str_split_fixed(Medida[2],"([[:blank:]]*) ",n=Inf)[2]
        Longitud <- 6
      }
    #########
    
    ##HIERRO U CHICO
      Selection <- "HIERRO U CHICO" 
      if (metal_pricedata$Categoria[i]== Selection) {
        Seccion <- "PERFIL U CHICO"
        Medida <- str_extract(metal_pricedata$Descripcion[i] , "[[:digit:]].*(?=mm)")
        Medida <- str_split_fixed(Medida,"[X|x]",n=Inf) # Splits 
        Medida <- str_trim(Medida)
        Espesor <- Medida[3]  
        Dimension <- paste(Medida[1],"x",Medida[2],"x",Espesor,sep = " ")  
        Peso <- str_extract(metal_pricedata$Descripcion[i] , "(?<=mm).*[[:digit:]].*(?=Kg)")
        Longitud <- 6
      }
      ########      
      
   ##ANGULO
      Selection <- "HIERRO ANGULO" 
      if (metal_pricedata$Categoria[i]== Selection) {
        Seccion <- "PERFIL ANGULO"
        Medida <- str_split_fixed(metal_pricedata$Descripcion[i],"[X|x]",n=Inf)
        Medida <- str_trim(Medida)
        Espesor <- Medida[2]
        Dimension <- str_extract(Medida[1] , "([[:digit:]].*)") 
        Dimension <- paste(Dimension,"x",Espesor,sep = " ")
        Longitud <- str_trim(str_extract(Medida[3] , "([[:digit:]].*?(?=M))") )
        Peso <- str_trim(str_extract(Medida[4] , "(?<=[/)]).*[[:digit:]].*?(?=K|k)"))
      }
      ########      
      
   ##PLANCHUELA
      Selection <- "PLANCHUELAS" 
      if (metal_pricedata$Categoria[i]== Selection) {
        Seccion <- str_extract(metal_pricedata$Descripcion[i] , ".*?(?=[[:digit:]])")
        Seccion <- str_trim(Seccion)
        Medida <- str_split_fixed(metal_pricedata$Descripcion[i],"[X|x]",n=Inf)
        Medida <- str_trim(Medida)
        Espesor <- str_trim(str_extract(Medida[2] , "([[:digit:]].*?(?=[(]))") )
        #Espesor <- str_trim(str_extract(Medida[2] , "([[:digit:]].*?(?=[[:blank:]]))") )  Planchuela perf.
        Dimension <- str_extract(Medida[1] , "([[:digit:]].*)") 
        Dimension <- paste(Dimension,"x",Espesor,sep = " ")
        Longitud <- str_trim(str_extract(Medida[4] , "([[:digit:]].*?(?=M))") )
        Peso <- str_trim(str_extract(Medida[3] , "(?<=[/)]).*[[:digit:]].*?(?=K|k)"))
        
        ###FALTA PLANCHUELA PERFORADA
        
      }
   ########      
    
   ##HIERRO IPN
      Selection <- "HIERRO IPN" 
      if (metal_pricedata$Categoria[i]== Selection) {
        Seccion <- "PERFIL IPN"
        Medida <- str_extract(metal_pricedata$Descripcion[i] , "[[:digit:]].*(?=mm)")
        Dimension <- str_trim(Medida)
        Longitud <- str_trim(str_extract(metal_pricedata$Descripcion[i] , "(\\d+)*.?(?=M)")) #regex: last digits before M
        Peso <- str_trim(str_extract(metal_pricedata$Descripcion[i] , "(\\d+)*.?(?=K|k)"))     #regex: last digits before K
      }
   ########      
      
   ##HIERRO UPN
      Selection <- "HIERRO UPN" 
      if (metal_pricedata$Categoria[i]== Selection) {
        Seccion <- "PERFIL UPN"
        Medida <- str_extract(metal_pricedata$Descripcion[i] , "[[:digit:]].*(?=mm)")
        Dimension <- str_trim(Medida)
        Longitud <- str_trim(str_extract(metal_pricedata$Descripcion[i] , "(\\d+)*.?(?=M)")) #regex: last digits before M
        Peso <- str_trim(str_extract(metal_pricedata$Descripcion[i] , "(\\d+)*.?(?=K|k)"))     #regex: last digits before K
      }
   ########      
      
   ##PERFIL U
      Selection <- "PERFIL U" 
      if (grepl(Selection,metal_pricedata$Categoria[i])) {
        Seccion <- str_extract(metal_pricedata$Descripcion[i] , ".*?(?=[[:digit:]])")
        Seccion <- str_trim(Seccion)
        Medida <- str_split_fixed(metal_pricedata$Descripcion[i],"[X|x]",n=Inf) # Splits 
        Medida <- str_trim(Medida)
        Espesor <- Medida[3]  # Extracts the thickness
        Dimension <- paste(str_extract(Medida[1] , "([[:digit:]]+)"),"x",Medida[2],"x",Espesor,sep = " ")  
        Longitud <- str_trim(str_extract(Medida[4] , "(\\d+)*.?(?=m)"))
        Peso <- str_trim(str_extract(Medida[4] , "(\\d+.\\d+)|(\\d+)*.?(?=K|k)"))     #regex: last digits before K (with dot)
      }
   ########      
    
   ##PERFIL C
      Selection <- "PERFIL C" 
        if (grepl(Selection,metal_pricedata$Categoria[i])) {
        Seccion <- str_extract(metal_pricedata$Descripcion[i] , ".*?(?=[[:digit:]])")
        Seccion <- str_trim(Seccion)
        Medida <- str_split_fixed(metal_pricedata$Descripcion[i],"[X|x]",n=Inf) # Splits 
        Medida <- str_trim(Medida)
        Espesor <- str_trim(str_extract(Medida[4] , "(\\d+.\\d+)|(\\d+)*.?(?=mm)"))  # Extracts the thickness including dot
        Dimension <- paste(str_extract(Medida[1] , "([[:digit:]]+)"),"x",Medida[2],"x",Medida[3],"x",Espesor,sep = " ")  
        Longitud <- 12 ## Vienen todos de 12 metros
        Peso <- str_trim(str_extract(Medida[4] , "(?<=mm).*(\\d+.\\d+)|(\\d+)*.?(?=K|k)"))     #regex: last digits before K (with dot)
        } 
   ########      
      
   ##PERFIL W/HEB/IPE
      Selection <- "PERFIL W/HEB/IPE" 
      if (grepl(Selection,metal_pricedata$Categoria[i])) {
        Seccion <- str_extract(metal_pricedata$Descripcion[i] , ".*?(?=[[:digit:]])")
        Seccion <- str_trim(Seccion)
        Medida <- str_extract(metal_pricedata$Descripcion[i] , "[[:digit:]]+")
        
        if (grepl("x",metal_pricedata$Descripcion[i])) {
          Aux <- str_extract(str_split_fixed(metal_pricedata$Descripcion[i],"[X|x]",n=Inf)[2] ,"(\\d+.\\d+)|(\\d+)+")
          Dimension <- paste(Medida, "x", Aux, sep = " ")
        } else {
          Dimension <- Medida
        }
        Espesor <- NA
        Longitud <- 12 ## Vienen todos de 12 metros
        Peso <- str_trim(str_extract(metal_pricedata$Descripcion[i] , "((\\d+.\\d*)|(\\d+))*.?(?=K|k)"))     #regex: last digits before K (with dot)
        
        if(grepl("\\.",Peso)) {               ## Checks for thousand separator with dot and removes it. Mistake in data format
          if((nchar(Peso)-str_locate(Peso,"\\.")[1]) > 2) {
            Peso <- sub("\\.","",Peso)  ## Checks if digits are bigger than 2 (thousand separators mistake in data)
          }
        }
      } 
   ########      
      
   ##CAÑOS ASTM
      Selection <- "CAÑOS ASTM" 
      if (grepl(Selection,metal_pricedata$Categoria[i])) {
        Seccion <- str_extract(metal_pricedata$Descripcion[i] , ".*?(?=[(])")
        Seccion <- str_trim(Seccion)
        Medida <- str_trim(str_extract(metal_pricedata$Descripcion[i] , "(?<=[)]).*?(?=x)"))
        Medida <- sub("\"", " ", Medida) ##Eliminates " 
        Dimension <- str_trim(str_extract(metal_pricedata$Descripcion[i] , "(?=[(]).*?(?<=[)])")) #adds milimiters dimensions
        Dimension <- paste(Medida,Dimension,sep = " ")
        Espesor <- as.numeric(str_extract(Dimension , "(?<=x).*?(?=[)])")) # extracts thickness in mm
        Longitud <- str_trim(str_extract(metal_pricedata$Descripcion[i], "((\\d+.\\d+)|(\\d+))+.?(?=m|M)"))
        Diameter <- as.numeric(str_extract(Dimension , "(?<=[()]).*?(?=[x])")) #extracts external diameter
        Peso <- 3.141592/4*((Diameter/1000)^2-((Diameter-2*Espesor)/1000)^2)*as.numeric(Longitud)*7850   # Calculates weight of steel tube
      }        
   ########    
   
   ##CAÑOS USO MECANICO
      Selection <- "CAÑOS USO MECANICO" 
      if (grepl(Selection,metal_pricedata$Categoria[i])) {
        Seccion <- str_extract(metal_pricedata$Descripcion[i] , ".*?(?=[[:digit:]])")
        Seccion <- str_trim(Seccion)
        Medida <- str_trim(str_extract(metal_pricedata$Descripcion[i]  , "(?<=[(]).*?(?=[)])"))
        Medida <- sub("\"", " ", Medida) ##Eliminates " 
        Dimension <- Medida   
        Espesor <- str_split_fixed(metal_pricedata$Descripcion[i],"[X|x]",n=Inf)[2]
        Longitud <- str_trim(str_extract(metal_pricedata$Descripcion[i], "((\\d+.\\d+)|(\\d+))+.?(?=M)"))
        Peso <- str_trim(str_extract(metal_pricedata$Descripcion[i], "((\\d+.\\d+)|(\\d+))+.?(?=k|K)"))
      }        
  ######## 

  ##HIERRO TREFILADO 1045
      Selection <- "HIERRO TREFILADO 1045" 
      if (grepl(Selection,metal_pricedata$Categoria[i])) {
        Seccion <- Selection
        Medida <- str_trim(str_extract(metal_pricedata$Descripcion[i], "(\\d+).?(?=mm)"))
        Dimension <- Medida   
        Espesor <- NA
        Longitud <- 1 ## Se coloca un largo genérico, varía entre 3 a 5.9 metros
        Peso <- str_trim(str_extract(metal_pricedata$Descripcion[i], "((\\d+.\\d+)|(\\d+))+.?(?=k|K)")) # el peso es por unidad de metro
      }        
  ######## 
      
  ##HIERRO LISO
      Selection <- "HIERRO LISO" 
      if (grepl(Selection,metal_pricedata$Categoria[i])) {
        if (!grepl("TONELADA",metal_pricedata$Descripcion[i])){  #Discards measure by Ton
          Seccion <- Selection
          Medida <- str_trim(str_extract(metal_pricedata$Descripcion[i], "(\\d+)."))
          Dimension <- Medida   
          Espesor <- NA
          Longitud <- str_trim(str_extract(metal_pricedata$Descripcion[i], "((\\d+.\\d+)|(\\d+))+.?(?=m|M)"))
          Peso <- str_trim(str_extract(metal_pricedata$Descripcion[i], "((\\d+.\\d+)|(\\d+))+.?(?=k|K)")) # el peso es por unidad de metro
        }
      }        
  ######## 
      
  ##CHAPA NEGRA L. CALIENTE
      Selection <- "CHAPA NEGRA L. CALIENTE" 
      if (grepl(Selection,metal_pricedata$Categoria[i])) {
        if (!grepl("OCASIONAL",metal_pricedata$Descripcion[i])){  #Discards OCASIONAL values
          Seccion <- Selection
          Medida <- str_trim(str_extract(metal_pricedata$Descripcion[i], "(\\d+.\\d+)(?=x).(\\d+.\\d+)"))
          Dimension <- Medida   
          Espesor <- str_trim(str_extract(metal_pricedata$Descripcion[i], "(\\d+.\\d+)"))
          Longitud <- NA
          Peso <- str_trim(str_extract(metal_pricedata$Descripcion[i], "((\\d+.\\d+)|(\\d+))+.?(?=k|K)"))
        }
      }        
  ######## 
      
  ##CHAPA NEGRA L. FRIO
      Selection <- "CHAPA NEGRA L. FRIO" 
      if (grepl(Selection,metal_pricedata$Categoria[i])) {
        if (!grepl("OUT",metal_pricedata$Descripcion[i])){  #Discards OUT values
          Seccion <- Selection
          Medida <- str_trim(str_extract(metal_pricedata$Descripcion[i], "(\\d+.\\d+)(?=x).(\\d+.\\d+)"))
          Dimension <- Medida   
          Espesor <- str_trim(str_extract(metal_pricedata$Descripcion[i], "(\\d+.\\d+)"))
          Longitud <- NA
          Peso <- str_trim(str_extract(metal_pricedata$Descripcion[i], "((\\d+.\\d+)|(\\d+))+.?(?=k|K)"))
        }
      }        
  ######## 
   
  #################### 
   
    #Adding columns
    metal_pricedata$Seccion[i] <- Seccion
    metal_pricedata$Dimension[i] <- Dimension
    metal_pricedata$Espesor[i] <- Espesor
    metal_pricedata$Peso[i] <- str_trim(Peso)
    metal_pricedata$Longitud[i] <- Longitud
  }
  
  ########### Set as numeric
  metal_pricedata$Peso <- as.numeric(metal_pricedata$Peso)  
  metal_pricedata$Espesor <- as.numeric(metal_pricedata$Espesor)
  metal_pricedata$Longitud <- as.numeric(metal_pricedata$Longitud)
  
  ##########REMOVE not used items
  metal_pricedata <- metal_pricedata[!is.na(metal_pricedata$Peso),]  #removes items without weight
  metal_pricedata <- metal_pricedata[!grepl("OUT",metal_pricedata$Seccion),] #removes items with "OUT" (meaning unknown)
  
  #################Tidying data
  for (i in 1:dim(metal_pricedata)[1]) {
    if (grepl("TUBOS ESTRUCTURALES",metal_pricedata$Categoria[i])) {
      metal_pricedata$Categoria[i] <- "TUBO ESTRUCTURAL"
    }
    if (grepl("CUADRADO",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "TUBO CUADRADO"
    }
    if (grepl("RECTANG",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "TUBO RECTANGULAR"
    }
    if (grepl("REDONDO",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "TUBO REDONDO"
    }
  }
  
  #################Translation
  for (i in 1:dim(metal_pricedata)[1]) {
    if (grepl("TUBO CUADRADO",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "SQUARE TUBE"
    }
    if (grepl("TUBO RECTANGULAR",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "RECTANGULAR TUBE"
    }
    if (grepl("TUBO REDONDO",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "ROUND TUBE"
    }
    if (grepl("PLANCHUELA",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "PLATE"
    }
    if (grepl("PERFIL ANGULO",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "ANGLE"
    }
    if (grepl("PERFIL T",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "T SECTION"
    }
    if (grepl("PERFIL U CHICO",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "U SECTION-SMALL"
    }
    if (grepl("PERFIL IPN",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "IPN SECTION"
    }
    if (grepl("PERFIL UPN",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "UPN SECTION"
    }
    if (grepl("CHAPA NEGRA L. CALIENTE",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "HOT ROLLED STEEL SHEET"
    }
    if (grepl("CHAPA NEGRA L. FRIO",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "COLD ROLLED STEEL SHEET"
    }
    if (grepl("HIERRO LISO",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "ROUND BAR"
    }
    if (grepl("HIERRO TREFILADO 1045",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "COLD DRAWN 1045 STEEL"
    }
    if (grepl("PERFIL U",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "U SECTION"
    }
    if (grepl("CAÑO USO MECANICO",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "TUBE-MECHANICAL USE"
    }
    if (grepl("PERFIL C",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "C SECTION"
    }
    if (grepl("CAÑO ASTM S-40",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "TUBE-ASTM S-40"
    }
    if (grepl("PERFIL C GALV",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "C SECTION-GALV."
    }
    if (grepl("PERFIL HEB GREY",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "HEB GREY SECTION"
    }
    if (grepl("PERFIL IPE",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "IPE SECTION"
    }
    if (grepl("PERFIL W",metal_pricedata$Seccion[i])) {
      metal_pricedata$Seccion[i] <- "W SECTION"
    }
    
  }
  
return (metal_pricedata)
}

  #### ON WORK  
  
###CHECKING  
    #  head(metal_pricedata[!is.na(metal_pricedata[,6]),],100)
    #  unique(metal_pricedata$Categoria)  #checks vector of unique category values

###SELECTING  
#  Selection <- "CAÑOS ASTM" 
#  C <- metal_pricedata[metal_pricedata$Categoria == Selection,] 
#  Medida <-C[5,2]
  