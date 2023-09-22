library(readr)
library(sf)
library(tidyverse)
library(lubridate)
library(plotly)
library(zoo)

#### 1.- Creacion de datos formato SWAT ####

##### PRECIPITACION ####

#### PCP 4.5 ####

# Secuencia de fechas para todo el periodo con datos de cclimatico

init_date <- dmy("01/01/2006")
end_date <- dmy("31/12/2100")

dates <- seq(init_date, end_date, 1)

length(dates)

# Importamos archivo de pcp4.5
pcp_4.5 <- read.csv("C:/ASG/datos_cc_guajaraz/RCP_4.5/pcp4.5.csv", sep = ";")
colnames(pcp_4.5)


modelos <- unique(pcp_4.5$Modelo)
puntos <- unique(pcp_4.5$Punto)

#Quitar columna final
pcp_4.5 <- pcp_4.5[,c(1:37)]

#Quitar geometria
geometry <- tibble(punto = unique(pcp_4.5$Punto), lat = unique(pcp_4.5$Latitud), long = unique(pcp_4.5$Longitud))
geometry$elev <- c(801, 750, 617, 1119, 755, 661, 834, 783, 668)


pcp_45 <- pcp_4.5[,-c(5,6)]


pcp_longer <- pcp_45 %>% pivot_longer(., cols = -c("Año","Mes", "Modelo", "Punto"), ) %>% 
              mutate(value = str_replace(string = value, pattern = ",", replacement = "."),
                     value = as.numeric(value))

nass <- is.na(pcp_longer$value)


#### Tabla 4.5 pcp ####
tab_days_info <-  pcp_longer[!nass, ] %>%  filter(., Punto == "point-5") %>% 
  group_by(Modelo) %>% summarise(ndays = length(value), init_year = min(Año), end_year = max(Año)) %>% 
  mutate(case = case_when(ndays == 34698 ~ "days ok",
                   ndays == 34675 ~ "day 29/02 no", 
                   ndays == 33645 ~ "days 31 no, ends in 11/2099"), 
         diff_days = 34698 - ndays) %>% 
  mutate(esc_number = c(1:8, 13, 14, 15, 16, 9:12)) %>% 
  arrange(., esc_number)



modelos_buenos <- tab_days_info %>% filter(., case == "days ok") %>% .$Modelo
# Numeros de anios bisiestos
sum(leap_year(2006:2100))



tibble_normales <- c()
for(i in 1:length(modelos_buenos)){
mmmp <- pcp_longer %>% filter(Año < 2020, Modelo == modelos_buenos[i], Punto == "point-5") 
nas_mmp <- is.na(mmmp$value)

filterednas<- mmmp[!nas_mmp,]

models_pcp <- filterednas$value

tibble_normales <- tibble( tibble_normales, models_pcp, .name_repair = "unique")}


colnames(tibble_normales) <- c(modelos_buenos)


pcp_obs_yr <- pcp_obs %>% group_by(year = year(date)) %>% summarise(pcp_yr_obs = sum(pcp)) 

tibble_normales <- tibble_normales %>% 
  cbind(., date =  seq(dmy("01/01/2006"), dmy("31/12/2019"), 1)) %>% 
  .[,c(13,1:12)]


pcpplott <- tibble_normales %>% #left_join(., pcp_obs, "date") %>% 
  group_by(year = year(date)) %>% 
  summarise_at(modelos_buenos, sum) %>% 
  cbind(obs_pcp = pcp_obs_yr$pcp_yr_obs) %>% 
  pivot_longer(., -c(year, obs_pcp)) %>% 
  ggplot(aes(x= year))+
  geom_line(aes(y = value, color = name))+
  geom_line(aes(y = obs_pcp), color = "black")

ggplotly(pcpplott)

  

#Generar datos en formato SWAT para los 12 con dias normales

modelos_buenos <- tab_days_info %>% filter(., case == "days ok") %>% .$Modelo


path_save <- "C:/ASG/datos_cc_guajaraz/New_files/RCP_4.5/PCP_FILES/"

for(i in 1:length(modelos_buenos)){
  
  modelo <- modelos_buenos[i]
  data <- pcp_longer %>% filter(Modelo == modelo) 
  directorio <- paste(i, "_", modelo, "/", sep = "")
  dir.create(paste(path_save, directorio, sep = "")  )                     
  
  for(n in 1:length(puntos)){
  punto_st <- paste("point-", n, sep = "")
  mmmp <- data %>%  filter(Punto == punto_st)
  nas_mmp <- is.na(mmmp$value)
  
  filterednas<- mmmp[!nas_mmp,]
  
  tab <- tibble(date = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), value = round(filterednas$value, 5)) %>%  
    mutate(year = year(date), yday = yday(date)) %>% select(year, yday, value)
  nbyr <-  length(unique(tab$year))
  tstep <-  0
  lat <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"lat"] %>% str_replace(., ",", ".")
  lon <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"long"]%>% str_replace(., ",", ".")
  elev <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"elev"]%>% str_replace(., ",", ".")

  
  # write.table(tab,  paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F, col.names = F) 
  
  
  firstline <- paste( "punto", str_remove(punto_st, "point-"),"_pcp.pcp: Precipitation data - file written with R, ",  gsub("\\(|\\)", "", Sys.time()), sep = "")
  second_line <- paste("nbyr", "tstep", "lat", "lon", "elev", sep = "     " )
  third_line <- tibble(nbyr, tstep, lat, lon, elev ) 
  third_line <- paste(third_line$nbyr, third_line$tstep, third_line$lat, third_line$lon, third_line$elev, sep = "  ")
  
  # con <- file(paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), "w" )
  
  writeLines(c(firstline,second_line, third_line ),paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = "")) 
  write.table(tab,  file = paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), sep = "\t", col.names = F, row.names = F, quote =  F, append = T) 
  
 #close(con = con)
 #
 #models_pcp <- tibble("20060101" = filterednas$value)
 #
 #write.csv(models_pcp, paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F)
  
  }}


# Generar datos en formato SWAT para el modelo sin bisiestos

modelo_sinbis <- tab_days_info %>% filter(., case == "day 29/02 no") %>% .$Modelo
data <- pcp_longer %>% filter(., Modelo == modelo_sinbis)

nnnas <- is.na(data$value)

data <- data[!nnnas,]


# En los dias 29/02 vamos a repetir el dato del dia 28/02

init_date <- dmy("01/01/2006")
end_date <- dmy("31/12/2100")
dates <- seq(init_date, end_date, 1)

# A qué día de la secuencia corresponden los dias 29/02??

bisiest_days <- tibble(dates) %>% mutate(nday = seq(1, length(dates), 1), month = month(dates), dayss = day(dates),  
                         daymonth = paste(month, dayss, sep = "/")) %>% 
  filter(., daymonth == "2/29") %>% .$nday

# Esto hay que hacerlo para cada punto, poner que la celda inexistente del dia 29/02 == al valor del 28/02

anios <- 2006:2101

# Crear filas vacias con los campos value y punto como nas
filas_insert <- tibble(Año = anios[leap_year(2006:2100)], Mes = 2, Modelo = "IPSL-IPSL-CM5A-MR_r1i1p1_SMHI-RCA4_v1", Punto = NA, name = "D?.a.29", value = NA)
# Tibble para ver los valores del dia 28/02
values_prev_tib <- tibble(bis_day = bisiest_days, bis_yrnumb = seq(1, length(bisiest_days), 1)) %>% mutate(., prev_day = bis_day - bis_yrnumb)


  todos_puntos <- c()
for(i in 1:length(puntos)){
  punto <- paste("point-", i, sep = "")
  mmmp <- data %>%  filter(Punto == punto)
  # Este no se altera, para ver los valores del dia 28
  data_set <- data %>%  filter(Punto == punto)
  
  for(m in 1:length(bisiest_days)){
    bisiestday <- bisiest_days[m]
    mmmp <- tibble::add_row(mmmp, .before = bisiestday, 
                             Año = filas_insert[m, "Año"]$Año,
                             Mes = filas_insert[m, "Mes"]$Mes,
                             Modelo = filas_insert[m, "Modelo"]$Modelo,
                             Punto = punto,
                             name = filas_insert[m, "name"]$name,
                             value = data_set[values_prev_tib[m, "prev_day"]$prev_day, "value"]$value)
  }

  todos_puntos <- todos_puntos %>% rbind(., mmmp)
  
  }


  directorio <- paste(13, "_", modelo_sinbis, "/", sep = "")
  dir.create(paste(path_save, directorio, sep = ""))                   
  
  for(n in 1:length(puntos)){
    punto_st <- paste("point-", n, sep = "")
    mmmp <- todos_puntos %>%  filter(Punto == punto_st)

    tab <- tibble(date = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), value = round(mmmp$value, 5)) %>%  
      mutate(year = year(date), yday = yday(date)) %>% select(year, yday, value)
    nbyr <-  length(unique(tab$year))
    tstep <-  0
    lat <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"lat"] %>% str_replace(., ",", ".")
    lon <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"long"]%>% str_replace(., ",", ".")
    elev <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"elev"]%>% str_replace(., ",", ".")
    
    
    # write.table(tab,  paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F, col.names = F) 
    
    
    firstline <- paste( "punto", str_remove(punto_st, "point-"),"_pcp.pcp: Precipitation data - file written with R, ",  gsub("\\(|\\)", "", Sys.time()), sep = "")
    second_line <- paste("nbyr", "tstep", "lat", "lon", "elev", sep = "     " )
    third_line <- tibble(nbyr, tstep, lat, lon, elev ) 
    third_line <- paste(third_line$nbyr, third_line$tstep, third_line$lat, third_line$lon, third_line$elev, sep = "  ")
    
    # con <- file(paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), "w" )
    
    writeLines(c(firstline,second_line, third_line ),paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = "")) 
    write.table(tab,  file = paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), sep = "\t", col.names = F, row.names = F, quote =  F, append = T) 
    
    
   #  models_pcp <- tibble("20060101" = mmmp$value)
    
    # write.csv(models_pcp, paste(path_save, directorio,  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F)
    
  }

  
# Generar datos en formato SWAT para el modelo sin dias 31
 # Este modelo para el 4.5 no tiene el mes de diciembre del 2099
  
  library(zoo)
  
  modelo_sin31s <- tab_days_info %>% filter(., case == "days 31 no, ends in 11/2099") %>% .$Modelo
  

  # Eliminamos los valores 31 que no tienen sentido (Feb, Apr, Jun, Sep, Nov)
  
  id_mod <- c(14:16)
  
  for(i in 1:length(modelo_sin31s)){
    
    modelo <- modelo_sin31s[i]
     data <- pcp_longer %>% filter(., Modelo == modelo)

     directorio <- paste(id_mod[i], "_", modelo_sin31s[i], "/", sep = "")
     dir.create(paste(path_save, directorio, sep = ""))  
     
  
dias_inex <- data %>% mutate(dayseq = seq(1, length(data$Mes), 1)) %>% 
  subset(., Mes %in% c(2,4,6,9,11)) %>% 
        subset(., name == "Día.31") %>% .[,"dayseq"]


data <- data[-dias_inex$dayseq,]

dias_inex2 <- data %>% mutate(dayseq = seq(1, length(data$Mes), 1)) %>% 
  subset(., Mes %in% c(2)) %>% 
  subset(., name == "Día.30") %>% .[,"dayseq"]

data <- data[-dias_inex2$dayseq,]


dias_inex3 <- data %>% mutate(dayseq = seq(1, length(data$Mes), 1)) %>% 
  subset(., Año %in% anios[!leap_year(2006:2100)]) %>% 
  subset(., Mes == 2) %>% 
  subset(., name == "Día.29")

data <- data[-dias_inex3$dayseq,]

data$value <- zoo::na.locf(data$value)
  

for(n in 1:length(puntos)){
  punto_st <- paste("point", n, sep = "-")
  
  dat_punt <- data %>% filter(., Punto == punto_st) %>% .[,"value"]
  #exp_val <- tibble("20060101" = dat_punt$value)
  
  tab <- tibble(date = seq(dmy("01/01/2006"), dmy("30/11/2099"), 1), value = round(dat_punt$value, 5)) %>%  
    mutate(year = year(date), yday = yday(date)) %>% select(year, yday, value)
  nbyr <-  length(unique(tab$year))
  tstep <-  0
  lat <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"lat"] %>% str_replace(., ",", ".")
  lon <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"long"]%>% str_replace(., ",", ".")
  elev <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"elev"]%>% str_replace(., ",", ".")
  
  
  # write.table(tab,  paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F, col.names = F) 
  
  
  firstline <- paste( "punto", str_remove(punto_st, "point-"),"_pcp.pcp: Precipitation data - file written with R, ",  gsub("\\(|\\)", "", Sys.time()), sep = "")
  second_line <- paste("nbyr", "tstep", "lat", "lon", "elev", sep = "     " )
  third_line <- tibble(nbyr, tstep, lat, lon, elev ) 
  third_line <- paste(third_line$nbyr, third_line$tstep, third_line$lat, third_line$lon, third_line$elev, sep = "  ")
  
  # con <- file(paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), "w" )
  
  writeLines(c(firstline,second_line, third_line ),paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = "")) 
  write.table(tab,  file = paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), sep = "\t", col.names = F, row.names = F, quote =  F, append = T) 
  
 # write.csv(exp_val, paste(path_save, directorio, "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F)
  
}

}

  #### PCP 8.5 ####
  
  # Secuencia de fechas para todo el periodo con datos de cclimatico
  
  init_date <- dmy("01/01/2006")
  end_date <- dmy("31/12/2100")
  dates <- seq(init_date, end_date, 1)
  length(dates)
  
  # Importamos archivo de pcp8.5
  pcp_8.5 <- read.csv("C:/ASG/datos_cc_guajaraz/RCP_8.5/pcp8.5.csv", sep = ";")
  colnames(pcp_8.5)

  modelos <- unique(pcp_8.5$Modelo)
  puntos <- unique(pcp_8.5$Punto)
  
  #Quitar columna final
  pcp_8.5 <- pcp_8.5[,c(1:37)]
  
  #Quitar geometria
  geometry <- tibble(punto = unique(pcp_8.5$Punto), lat = unique(pcp_8.5$Latitud), long = unique(pcp_8.5$Longitud))
  geometry$elev <- c(801, 750, 617, 1119, 755, 661, 834, 783, 668)
  
  pcp_85 <- pcp_8.5[,-c(5,6)]
  
  pcp_longer <- pcp_85 %>% pivot_longer(., cols = -c("Año","Mes", "Modelo", "Punto"), ) %>% 
    mutate(value = str_replace(string = value, pattern = ",", replacement = "."),
           value = as.numeric(value))
  
  nass <- is.na(pcp_longer$value)
  
  #### Tabla 8.5 pcp ####
  tab_days_info <-  pcp_longer[!nass, ] %>%  filter(., Punto == "point-1") %>% 
    group_by(Modelo) %>% summarise(ndays = length(value), init_year = min(Año), end_year = max(Año)) %>% 
    mutate(case = case_when(ndays == 34698 ~ "days ok",
                            ndays == 34675 ~ "day 29/02 no", 
                            ndays == 33675 ~ "days 31 no, ends in 12/2099",), 
           diff_days = 34698 - ndays) %>% 
    mutate(esc_number = c(1:8, 13, 14, 15, 16, 9:12)) %>% 
    arrange(., esc_number)
  
  modelos_buenos <- tab_days_info %>% filter(., case == "days ok") %>% .$Modelo
  # Numeros de anios bisiestos
  sum(leap_year(2006:2100))
  
  tibble_normales <- c()
  for(i in 1:length(modelos_buenos)){
    mmmp <- pcp_longer %>% filter(Año < 2020, Modelo == modelos_buenos[i], Punto == "point-5") 
    nas_mmp <- is.na(mmmp$value)
    
    filterednas<- mmmp[!nas_mmp,]
    
    models_pcp <- filterednas$value
    
    tibble_normales <- tibble( tibble_normales, models_pcp, .name_repair = "unique")}
  
  colnames(tibble_normales) <- c(modelos_buenos)
  
  pcp_obs_yr <- pcp_obs %>% group_by(year = year(date)) %>% summarise(pcp_yr_obs = sum(pcp)) 
  
  tibble_normales <- tibble_normales %>% 
    cbind(., date =  seq(dmy("01/01/2006"), dmy("31/12/2019"), 1)) %>% 
    .[,c(13,1:12)]
  
 
  #Generar datos en formato SWAT para los 12 con dias normales
  
  modelos_buenos <- tab_days_info %>% filter(., case == "days ok") %>% .$Modelo
  
  tibble_normales <- c()
  
  path_save <- "C:/ASG/datos_cc_guajaraz/New_files/RCP_8.5/PCP_FILES/"
  
  for(i in 1:length(modelos_buenos)){
    
    modelo <- modelos_buenos[i]
    data <- pcp_longer %>% filter(Modelo == modelo) 
    directorio <- paste(i, "_", modelo, "/", sep = "")
    dir.create(paste(path_save, directorio, sep = "")  )                     
    
    for(n in 1:length(puntos)){
      punto_st <- paste("point-", n, sep = "")
      mmmp <- data %>%  filter(Punto == punto_st)
      nas_mmp <- is.na(mmmp$value)
      
      filterednas<- mmmp[!nas_mmp,]
      
      tab <- tibble(date = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), value = round(filterednas$value, 5)) %>%  
        mutate(year = year(date), yday = yday(date)) %>% select(year, yday, value)
      nbyr <-  length(unique(tab$year))
      tstep <-  0
      lat <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"lat"] %>% str_replace(., ",", ".")
      lon <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"long"]%>% str_replace(., ",", ".")
      elev <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"elev"]%>% str_replace(., ",", ".")
      
      
      # write.table(tab,  paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F, col.names = F) 
      
      
      firstline <- paste( "punto", str_remove(punto_st, "point-"),"_pcp.pcp: Precipitation data - file written with R, ",  gsub("\\(|\\)", "", Sys.time()), sep = "")
      second_line <- paste("nbyr", "tstep", "lat", "lon", "elev", sep = "     " )
      third_line <- tibble(nbyr, tstep, lat, lon, elev ) 
      third_line <- paste(third_line$nbyr, third_line$tstep, third_line$lat, third_line$lon, third_line$elev, sep = "  ")
      
      # con <- file(paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), "w" )
      
      writeLines(c(firstline,second_line, third_line ),paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = "")) 
      write.table(tab,  file = paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), sep = "\t", col.names = F, row.names = F, quote =  F, append = T) 
      
      
      
      
      
      
      
      
      #models_pcp <- tibble("20060101" = filterednas$value)
      
      #write.csv(models_pcp, paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F)
      
    }}
  
  
  # Generar datos en formato SWAT para el modelo sin bisiestos
  
  modelo_sinbis <- tab_days_info %>% filter(., case == "day 29/02 no") %>% .$Modelo
  data <- pcp_longer %>% filter(., Modelo == modelo_sinbis)
  
  nnnas <- is.na(data$value)
  
  data <- data[!nnnas,]
  
  
  # En los dias 29/02 vamos a repetir el dato del dia 28/02
  
  init_date <- dmy("01/01/2006")
  end_date <- dmy("31/12/2100")
  dates <- seq(init_date, end_date, 1)
  
  # A qué día de la secuencia corresponden los dias 29/02??
  
  bisiest_days <- tibble(dates) %>% mutate(nday = seq(1, length(dates), 1), month = month(dates), dayss = day(dates),  
                                           daymonth = paste(month, dayss, sep = "/")) %>% 
    filter(., daymonth == "2/29") %>% .$nday
  
  # Esto hay que hacerlo para cada punto, poner que la celda inexistente del dia 29/02 == al valor del 28/02
  
  anios <- 2006:2101
  
  # Crear filas vacias con los campos value y punto como nas
  filas_insert <- tibble(Año = anios[leap_year(2006:2100)], Mes = 2, Modelo = "IPSL-IPSL-CM5A-MR_r1i1p1_SMHI-RCA4_v1", Punto = NA, name = "D?.a.29", value = NA)
  # Tibble para ver los valores del dia 28/02
  values_prev_tib <- tibble(bis_day = bisiest_days, bis_yrnumb = seq(1, length(bisiest_days), 1)) %>% mutate(., prev_day = bis_day - bis_yrnumb)
  
  
  todos_puntos <- c()
  for(i in 1:length(puntos)){
    punto <- paste("point-", i, sep = "")
    mmmp <- data %>%  filter(Punto == punto)
    # Este no se altera, para ver los valores del dia 28
    data_set <- data %>%  filter(Punto == punto)
    
    for(m in 1:length(bisiest_days)){
      bisiestday <- bisiest_days[m]
      mmmp <- tibble::add_row(mmmp, .before = bisiestday, 
                              Año = filas_insert[m, "Año"]$Año,
                              Mes = filas_insert[m, "Mes"]$Mes,
                              Modelo = filas_insert[m, "Modelo"]$Modelo,
                              Punto = punto,
                              name = filas_insert[m, "name"]$name,
                              value = data_set[values_prev_tib[m, "prev_day"]$prev_day, "value"]$value)
    }
    
    todos_puntos <- todos_puntos %>% rbind(., mmmp)
    
  }
  
  
  directorio <- paste(13, "_", modelo_sinbis, "/", sep = "")
  dir.create(paste(path_save, directorio, sep = ""))                   
  
  for(n in 1:length(puntos)){
    punto_st <- paste("point-", n, sep = "")
    mmmp <- todos_puntos %>%  filter(Punto == punto_st)
    
    tab <- tibble(date = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), value = round(mmmp$value, 5)) %>%  
      mutate(year = year(date), yday = yday(date)) %>% select(year, yday, value)
    nbyr <-  length(unique(tab$year))
    tstep <-  0
    lat <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"lat"] %>% str_replace(., ",", ".")
    lon <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"long"]%>% str_replace(., ",", ".")
    elev <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"elev"]%>% str_replace(., ",", ".")
    
    
    # write.table(tab,  paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F, col.names = F) 
    
    
    firstline <- paste( "punto", str_remove(punto_st, "point-"),"_pcp.pcp: Precipitation data - file written with R, ",  gsub("\\(|\\)", "", Sys.time()), sep = "")
    second_line <- paste("nbyr", "tstep", "lat", "lon", "elev", sep = "     " )
    third_line <- tibble(nbyr, tstep, lat, lon, elev ) 
    third_line <- paste(third_line$nbyr, third_line$tstep, third_line$lat, third_line$lon, third_line$elev, sep = "  ")
    
    # con <- file(paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), "w" )
    
    writeLines(c(firstline,second_line, third_line ),paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = "")) 
    write.table(tab,  file = paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), sep = "\t", col.names = F, row.names = F, quote =  F, append = T) 
    
    
   # models_pcp <- tibble("20060101" = mmmp$value)
    
    #write.csv(models_pcp, paste(path_save, directorio,  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F)
    
  }
  
  
  # Generar datos en formato SWAT para el modelo sin dias 31
  
  library(zoo)
  
  modelo_sin31s <- tab_days_info %>% filter(., case == "days 31 no, ends in 12/2099") %>% .$Modelo
  
  # Eliminamos los valores 31 que no tienen sentido (Feb, Apr, Jun, Sep, Nov)
  
  anios <- 2006:2101
  id_mod <- c(14:16)
  
  for(i in 1:length(modelo_sin31s)){
    
    modelo <- modelo_sin31s[i]
    data <- pcp_longer %>% filter(., Modelo == modelo_sin31s[i])
    
    directorio <- paste(id_mod[i], "_", modelo_sin31s[i], "/", sep = "")
    dir.create(paste(path_save, directorio, sep = ""))  
    
    
    dias_inex <- data %>% mutate(dayseq = seq(1, length(data$Mes), 1)) %>% 
      subset(., Mes %in% c(2,4,6,9,11)) %>% 
      subset(., name == "Día.31") %>% .[,"dayseq"]
    
    
    data <- data[-dias_inex$dayseq,]
    
    dias_inex2 <- data %>% mutate(dayseq = seq(1, length(data$Mes), 1)) %>% 
      subset(., Mes %in% c(2)) %>% 
      subset(., name == "Día.30") %>% .[,"dayseq"]
    
    data <- data[-dias_inex2$dayseq,]
    
    
    dias_inex3 <- data %>% mutate(dayseq = seq(1, length(data$Mes), 1)) %>% 
      subset(., Año %in% anios[!leap_year(2006:2100)]) %>% 
      subset(., Mes == 2) %>% 
      subset(., name == "Día.29")
    
    data <- data[-dias_inex3$dayseq,]
    
    data$value <- zoo::na.locf(data$value)
    
    
    for(n in 1:length(puntos)){
      punto <- paste("point", n, sep = "-")
      
      dat_punt <- data %>% filter(., Punto == punto) %>% .[,"value"]
      #exp_val <- tibble("20060101" = dat_punt$value)
      
      tab <- tibble(date = seq(dmy("01/01/2006"), dmy("31/12/2099"), 1), value = round(dat_punt$value, 5)) %>%  
        mutate(year = year(date), yday = yday(date)) %>% select(year, yday, value)
      nbyr <-  length(unique(tab$year))
      tstep <-  0
      lat <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"lat"] %>% str_replace(., ",", ".")
      lon <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"long"]%>% str_replace(., ",", ".")
      elev <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"elev"]%>% str_replace(., ",", ".")
      
      
      # write.table(tab,  paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F, col.names = F) 
      
      
      firstline <- paste( "punto", str_remove(punto_st, "point-"),"_pcp.pcp: Precipitation data - file written with R, ",  gsub("\\(|\\)", "", Sys.time()), sep = "")
      second_line <- paste("nbyr", "tstep", "lat", "lon", "elev", sep = "     " )
      third_line <- tibble(nbyr, tstep, lat, lon, elev ) 
      third_line <- paste(third_line$nbyr, third_line$tstep, third_line$lat, third_line$lon, third_line$elev, sep = "  ")
      
      # con <- file(paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), "w" )
      
      writeLines(c(firstline,second_line, third_line ),paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = "")) 
      write.table(tab,  file = paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), sep = "\t", col.names = F, row.names = F, quote =  F, append = T) 
      
    
     # write.csv(exp_val, paste(path_save, directorio, "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F)
      
    }
    
  }  
  
  

  ##### TEMPERATURA ####
  
  # Hay que crear archivos con dos columnas, una con Tmax y la otra con Tmin
  
  #### TMAX y Tmin 4.5 ####
    path_save <- "C:/ASG/datos_cc_guajaraz/New_files/RCP_4.5/TMP_FILES/"
    
  # Secuencia de fechas para todo el periodo con datos de cclimatico
  
  init_date <- dmy("01/01/2006")
  end_date <- dmy("31/12/2100")
  
  dates <- seq(init_date, end_date, 1)
  
  # Importamos archivos de TMax4.5 t tmin4.5. Ambos tienen el mismo nº de datos
  tmax_4.5 <- read.csv("C:/ASG/datos_cc_guajaraz/RCP_4.5/tmax4.5.csv", sep = ";")
  tmin_4.5 <- read.csv("C:/ASG/datos_cc_guajaraz/RCP_4.5/tmin4.5.csv", sep = ";")

  # Orden de modelos y puntos es igual entre Tmax y Tmin
  modelos <- unique(tmax_4.5$Modelo)
  puntos <- unique(tmax_4.5$Punto)
  
  #Quitar columna final
  tmax_4.5 <- tmax_4.5[,c(1:37)]
  tmin_4.5 <- tmin_4.5[,c(1:37)]
  
  #Quitar geometria
  tmax_4.5 <- tmax_4.5[,-c(5,6)]
  tmin_4.5 <- tmin_4.5[,-c(5,6)]

  TMAX_longer <- tmax_4.5 %>% pivot_longer(., cols = -c("Año","Mes", "Modelo", "Punto"), ) %>% 
    mutate(value = str_replace(string = value, pattern = ",", replacement = "."),
           value = as.numeric(value))
  Tmin_longer <- tmin_4.5 %>% pivot_longer(., cols = -c("Año","Mes", "Modelo", "Punto"), ) %>% 
    mutate(value = str_replace(string = value, pattern = ",", replacement = "."),
           value = as.numeric(value))
  
  nass <- is.na(Tmin_longer$value)
  
  
  #### Tabla 4.5 TMAX ####
  tab_days_info <-  TMAX_longer[!nass, ] %>%  filter(., Punto == "point-5") %>% 
    group_by(Modelo) %>% summarise(ndays = length(value), init_year = min(Año), end_year = max(Año)) %>% 
    mutate(case = case_when(ndays == 34698 ~ "days ok",
                            ndays == 34675 ~ "day 29/02 no", 
                            ndays == 33645 ~ "days 31 no, ends in 11/2099"), 
           diff_days = 34698 - ndays) %>% 
    mutate(esc_number = c(1:8, 13, 14, 15, 16, 9:12)) %>% 
    arrange(., esc_number)

  # Generar los Modelos con los dias bien
  
  modelos_buenos <- tab_days_info %>% filter(., case == "days ok") %>% .$Modelo
  # Numeros de anios bisiestos
  sum(leap_year(2006:2100))

  #Generar datos en formato SWAT para los 12 con dias normales
  
  modelos_buenos <- tab_days_info %>% filter(., case == "days ok") %>% .$Modelo

  tibble_normales <- c()
  
  path_save <- "C:/ASG/datos_cc_guajaraz/New_files//RCP_4.5/TMP_FILES/"
  
  for(i in 1:length(modelos_buenos)){
    
    modelo <- modelos_buenos[i]
    data_TMAX <- TMAX_longer %>% filter(Modelo == modelo) 
    data_Tmin <- Tmin_longer %>% filter(Modelo == modelo) 
    directorio <- paste(i, "_", modelo, "/", sep = "")
    dir.create(paste(path_save, directorio, sep = "")  )                     
    
    for(n in 1:length(puntos)){
      punto_st <- paste("point-", n, sep = "")
      TMax <- data_TMAX %>%  filter(Punto == punto_st)
      Tmin <- data_Tmin %>%  filter(Punto == punto_st)
      nas_mmp <- is.na(TMax$value)
      
      filterednasTMAX<- TMax[!nas_mmp,]
      filterednasTmin<- Tmin[!nas_mmp,]
      
      
      models_tmp <- tibble(tmax = filterednasTMAX$value, tmin = filterednasTmin$value) %>% mutate("20060101" = paste(tmax, tmin, sep = ","))
      
      
      mmmp <- todos_puntos %>%  filter(Punto == punto_st)
      
      tab <- tibble(date = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), 
                    tmax = round(filterednasTMAX$value,5), 
                    tmin = round(filterednasTmin$value,5)) %>%  
        mutate(year = year(date), yday = yday(date)) %>% select(year, yday, tmax, tmin)
      nbyr <-  length(unique(tab$year))
      tstep <-  0
      lat <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"lat"] %>% str_replace(., ",", ".")
      lon <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"long"]%>% str_replace(., ",", ".")
      elev <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"elev"]%>% str_replace(., ",", ".")
      
      
      # write.table(tab,  paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F, col.names = F) 
      
      
      firstline <- paste( "punto", str_remove(punto_st, "point-"),"_pcp.pcp: Temperature data - file written with R, ",  gsub("\\(|\\)", "", Sys.time()), sep = "")
      second_line <- paste("nbyr", "tstep", "lat", "lon", "elev", sep = "     " )
      third_line <- tibble(nbyr, tstep, lat, lon, elev ) 
      third_line <- paste(third_line$nbyr, third_line$tstep, third_line$lat, third_line$lon, third_line$elev, sep = "  ")
      
      # con <- file(paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), "w" )
      
      writeLines(c(firstline,second_line, third_line ),paste(path_save, directorio, "/",  "punto", n, "_tmp.tmp", sep = "")) 
      write.table(tab,  file = paste(path_save, directorio, "/",  "punto", n, "_tmp.tmp", sep = ""), sep = "\t", col.names = F, row.names = F, quote =  F, append = T) 
      
      #write.table(models_tmp[,3], file = paste(path_save,  directorio, "/", "punto", n, "_tmp.tmp", sep = ""),quote = F, row.names = F)
      
    }}
  
   
  # Generar datos en formato SWAT para el modelo sin bisiestos
  
  modelo_sinbis <- tab_days_info %>% filter(., case == "day 29/02 no") %>% .$Modelo
  data_TMax <- TMAX_longer %>% filter(., Modelo == modelo_sinbis)
  data_Tmin <- Tmin_longer %>% filter(., Modelo == modelo_sinbis)
  
  nnnas <- is.na(data_TMax$value)
  
  data_TMax <- data_TMax[!nnnas,]
  data_Tmin <- data_Tmin[!nnnas,]
  
  
  # En los dias 29/02 vamos a repetir el dato del dia 28/02
  init_date <- dmy("01/01/2006")
  end_date <- dmy("31/12/2100")
  dates <- seq(init_date, end_date, 1)
  
  # A qué día de la secuencia corresponden los dias 29/02??
  bisiest_days <- tibble(dates) %>% mutate(nday = seq(1, length(dates), 1), month = month(dates), dayss = day(dates),  
                                           daymonth = paste(month, dayss, sep = "/")) %>% 
    filter(., daymonth == "2/29") %>% .$nday
  
  # Esto hay que hacerlo para cada punto, poner que la celda inexistente del dia 29/02 == al valor del 28/02
  
  anios <- 2006:2101
  
  # Crear filas vacias con los campos value y punto como nas
  filas_insert <- tibble(Año = anios[leap_year(2006:2100)], Mes = 2, Modelo = "IPSL-IPSL-CM5A-MR_r1i1p1_SMHI-RCA4_v1", Punto = NA, name = "D?.a.29", value = NA)
  # Tibble para ver los valores del dia 28/02
  values_prev_tib <- tibble(bis_day = bisiest_days, bis_yrnumb = seq(1, length(bisiest_days), 1)) %>% mutate(., prev_day = bis_day - bis_yrnumb)
  
  
  todos_puntos_TMax <- c()
  todos_puntos_Tmin <- c()
  for(i in 1:length(puntos)){
    punto <- paste("point-", i, sep = "")
    tmp_Max <- data_TMax %>%  filter(Punto == punto)
    tmp_min <- data_Tmin %>%  filter(Punto == punto)
    # Este no se altera, para ver los valores del dia 28
    data_set_TMax <- data_TMax %>%  filter(Punto == punto)
    data_set_Tmin <- data_Tmin %>%  filter(Punto == punto)
    
    for(m in 1:length(bisiest_days)){
      bisiestday <- bisiest_days[m]
      tmp_Max <- tibble::add_row(tmp_Max, .before = bisiestday, 
                              Año = filas_insert[m, "Año"]$Año,
                              Mes = filas_insert[m, "Mes"]$Mes,
                              Modelo = filas_insert[m, "Modelo"]$Modelo,
                              Punto = punto,
                              name = filas_insert[m, "name"]$name,
                              value = data_set_TMax[values_prev_tib[m, "prev_day"]$prev_day, "value"]$value)
      tmp_min <- tibble::add_row(tmp_min, .before = bisiestday, 
                              Año = filas_insert[m, "Año"]$Año,
                              Mes = filas_insert[m, "Mes"]$Mes,
                              Modelo = filas_insert[m, "Modelo"]$Modelo,
                              Punto = punto,
                              name = filas_insert[m, "name"]$name,
                              value = data_set_Tmin[values_prev_tib[m, "prev_day"]$prev_day, "value"]$value)
    }
    
    todos_puntos_TMax <- todos_puntos_TMax %>% rbind(., tmp_Max)
    todos_puntos_Tmin <- todos_puntos_Tmin %>% rbind(., tmp_min)
    
  }
  

  directorio <- paste(13, "_", modelo_sinbis, "/", sep = "")
  dir.create(paste(path_save, directorio, sep = ""))                   
  
  for(n in 1:length(puntos)){
    punto_st <- paste("point-", n, sep = "")
    
    tmp_save_files <- tibble(Punto = todos_puntos_TMax$Punto, tmax = todos_puntos_TMax$value, tmin = todos_puntos_Tmin$value) %>% 
      mutate("20060101" = paste(tmax, tmin, sep = ","))
    
    ffile <- tmp_save_files %>%  filter(Punto == punto_st)
    
    
    tab <- tibble(date = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), 
                  tmax = round(ffile$tmax,5), 
                  tmin = round(ffile$tmin,5)) %>%  
      mutate(year = year(date), yday = yday(date)) %>% select(year, yday, tmax, tmin)
    nbyr <-  length(unique(tab$year))
    tstep <-  0
    lat <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"lat"] %>% str_replace(., ",", ".")
    lon <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"long"]%>% str_replace(., ",", ".")
    elev <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"elev"]%>% str_replace(., ",", ".")
    
    
    # write.table(tab,  paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F, col.names = F) 
    
    
    firstline <- paste( "punto", str_remove(punto_st, "point-"),"_pcp.pcp: Temperature data - file written with R, ",  gsub("\\(|\\)", "", Sys.time()), sep = "")
    second_line <- paste("nbyr", "tstep", "lat", "lon", "elev", sep = "     " )
    third_line <- tibble(nbyr, tstep, lat, lon, elev ) 
    third_line <- paste(third_line$nbyr, third_line$tstep, third_line$lat, third_line$lon, third_line$elev, sep = "  ")
    
    # con <- file(paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), "w" )
    
    writeLines(c(firstline,second_line, third_line ),paste(path_save, directorio, "/",  "punto", n, "_tmp.tmp", sep = "")) 
    write.table(tab,  file = paste(path_save, directorio, "/",  "punto", n, "_tmp.tmp", sep = ""), sep = "\t", col.names = F, row.names = F, quote =  F, append = T) 
    
    
    
    #write.table(ffile[,4], file = paste(path_save,  directorio, "/", "punto", n, "_tmp.tmp", sep = ""),quote = F, row.names = F)
  }
  
  
  
  # Generar datos en formato SWAT para el modelo sin dias 31
  
  library(zoo)
  
  modelo_sin31s <- tab_days_info %>% filter(., case == "days 31 no, ends in 11/2099") %>% .$Modelo
  
  # Eliminamos los valores 31 que no tienen sentido (Feb, Apr, Jun, Sep, Nov)
  
  anios <- 2006:2099
  id_mod <- c(14:16)
  
  for(i in 1:length(modelo_sin31s)){
    
    modelo <- modelo_sin31s[i]
    data_TMax <- TMAX_longer %>% filter(., Modelo == modelo)
    data_Tmin <- Tmin_longer %>% filter(., Modelo == modelo)
    
    directorio <- paste(id_mod[i], "_", modelo_sin31s[i], "/", sep = "")
    dir.create(paste(path_save, directorio, sep = ""))  
    
    
    dias_inex <- data_TMax %>% mutate(dayseq = seq(1, length(data_TMax$Mes), 1)) %>% 
      subset(., Mes %in% c(2,4,6,9,11)) %>% 
      subset(., name == "Día.31") %>% .[,"dayseq"]
    
    
    data_TMax <- data_TMax[-dias_inex$dayseq,]
    data_Tmin <- data_Tmin[-dias_inex$dayseq,]
    
    dias_inex2 <- data_TMax %>% mutate(dayseq = seq(1, length(data_TMax$Mes), 1)) %>% 
      subset(., Mes %in% c(2)) %>% 
      subset(., name == "Día.30") %>% .[,"dayseq"]
    
    data_TMax <- data_TMax[-dias_inex2$dayseq,]
    data_Tmin <- data_Tmin[-dias_inex2$dayseq,]
    
    
    dias_inex3 <- data_TMax %>% mutate(dayseq = seq(1, length(data_TMax$Mes), 1)) %>% 
      subset(., Año %in% anios[!leap_year(2006:2100)]) %>% 
      subset(., Mes == 2) %>% 
      subset(., name == "Día.29")
    
    data_TMax <- data_TMax[-dias_inex3$dayseq,]
    data_Tmin <- data_Tmin[-dias_inex3$dayseq,]
    
    data_TMax$value <- zoo::na.locf(data_TMax$value)
    data_Tmin$value <- zoo::na.locf(data_Tmin$value)
    
    
    for(n in 1:length(puntos)){
      punto <- paste("point", n, sep = "-")
      
      dat_punt_Tmax <- data_TMax %>% filter(., Punto == punto) %>% .[,"value"]
      dat_punt_Tmin <- data_Tmin %>% filter(., Punto == punto) %>% .[,"value"]
      exp_val <- tibble(tmp_max = dat_punt_Tmax$value, tmp_min = dat_punt_Tmin$value) %>% 
        mutate("20060101" = paste(tmp_max, tmp_min, sep = ","))
      
      
      
      tab <- tibble(date = seq(dmy("01/01/2006"), dmy("30/11/2099"), 1), 
                    tmax = round(dat_punt_Tmax$value,5), 
                    tmin = round(dat_punt_Tmin$value,5)) %>%  
        mutate(year = year(date), yday = yday(date)) %>% select(year, yday, tmax, tmin)
      nbyr <-  length(unique(tab$year))
      tstep <-  0
      lat <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"lat"] %>% str_replace(., ",", ".")
      lon <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"long"]%>% str_replace(., ",", ".")
      elev <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"elev"]%>% str_replace(., ",", ".")
      
      
      # write.table(tab,  paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F, col.names = F) 
      
      
      firstline <- paste( "punto", str_remove(punto_st, "point-"),"_pcp.pcp: Temperature data - file written with R, ",  gsub("\\(|\\)", "", Sys.time()), sep = "")
      second_line <- paste("nbyr", "tstep", "lat", "lon", "elev", sep = "     " )
      third_line <- tibble(nbyr, tstep, lat, lon, elev ) 
      third_line <- paste(third_line$nbyr, third_line$tstep, third_line$lat, third_line$lon, third_line$elev, sep = "  ")
      
      # con <- file(paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), "w" )
      
      writeLines(c(firstline,second_line, third_line ),paste(path_save, directorio, "/",  "punto", n, "_tmp.tmp", sep = "")) 
      write.table(tab,  file = paste(path_save, directorio, "/",  "punto", n, "_tmp.tmp", sep = ""), sep = "\t", col.names = F, row.names = F, quote =  F, append = T) 
      
      
      
      
      
      
      
      #write.table(exp_val[,3], file = paste(path_save,  directorio, "/", "punto", n, "_tmp.tmp", sep = ""),
           #       quote = F, row.names = F)
      
    }
    
  }    
  
  
  #### TMAX y Tmin 8.5 ####
  
  path_save <- "C:/ASG/datos_cc_guajaraz/New_files/RCP_8.5/TMP_FILES/"
  
  # Secuencia de fechas para todo el periodo con datos de cclimatico
  
  init_date <- dmy("01/01/2006")
  end_date <- dmy("31/12/2100")
  
  dates <- seq(init_date, end_date, 1)
  
  # Importamos archivos de TMax8.5 t tmin8.5. Ambos tienen el mismo nº de datos
  tmax_8.5 <- read.csv("C:/ASG/datos_cc_guajaraz/RCP_8.5/TMax8.5.csv", sep = ";")
  tmin_8.5 <- read.csv("C:/ASG/datos_cc_guajaraz/RCP_8.5/tmin8.5.csv", sep = ";")
  
  # Orden de modelos y puntos es igual entre Tmax y Tmin
  modelos <- unique(tmax_8.5$Modelo)
  puntos <- unique(tmax_8.5$Punto)
  
  #Quitar columna final
  tmax_8.5 <- tmax_8.5[,c(1:37)]
  tmin_8.5 <- tmin_8.5[,c(1:37)]
  
  #Quitar geometria
  tmax_8.5 <- tmax_8.5[,-c(5,6)]
  tmin_8.5 <- tmin_8.5[,-c(5,6)]
  
  
  TMAX_longer <- tmax_8.5 %>% pivot_longer(., cols = -c("Año","Mes", "Modelo", "Punto"), ) %>% 
    mutate(value = str_replace(string = value, pattern = ",", replacement = "."),
           value = as.numeric(value))
  Tmin_longer <- tmin_8.5 %>% pivot_longer(., cols = -c("Año","Mes", "Modelo", "Punto"), ) %>% 
    mutate(value = str_replace(string = value, pattern = ",", replacement = "."),
           value = as.numeric(value))
  
  nass <- is.na(Tmin_longer$value)
  
  
  #### Tabla 8.5 TMAX ####
  tab_days_info <-  TMAX_longer[!nass, ] %>%  filter(., Punto == "point-5") %>% 
    group_by(Modelo) %>% summarise(ndays = length(value), init_year = min(Año), end_year = max(Año)) %>% 
    mutate(case = case_when(ndays == 34698 ~ "days ok",
                            ndays == 34675 ~ "day 29/02 no", 
                            ndays == 33675 ~ "days 31 no, ends in 12/2099"), 
           diff_days = 34698 - ndays) %>% 
    mutate(esc_number = c(1:8, 13, 14, 15, 16, 9:12)) %>% 
    arrange(., esc_number)
  
  
  # Generar los Modelos con los dias bien
  
  modelos_buenos <- tab_days_info %>% filter(., case == "days ok") %>% .$Modelo
  # Numeros de anios bisiestos
  sum(leap_year(2006:2100))
  
  
  #Generar datos en formato SWAT para los 12 con dias normales
  
  modelos_buenos <- tab_days_info %>% filter(., case == "days ok") %>% .$Modelo
  
  
  tibble_normales <- c()
  
  path_save <- "C:/ASG/datos_cc_guajaraz/New_files/RCP_8.5/TMP_FILES/"
  
  for(i in 1:length(modelos_buenos)){
    
    modelo <- modelos_buenos[i]
    data_TMAX <- TMAX_longer %>% filter(Modelo == modelo) 
    data_Tmin <- Tmin_longer %>% filter(Modelo == modelo) 
    directorio <- paste(i, "_", modelo, "/", sep = "")
    dir.create(paste(path_save, directorio, sep = "")  )                     
    
    for(n in 1:length(puntos)){
      punto_st <- paste("point-", n, sep = "")
      TMax <- data_TMAX %>%  filter(Punto == punto_st)
      Tmin <- data_Tmin %>%  filter(Punto == punto_st)
      nas_mmp <- is.na(TMax$value)
      
      filterednasTMAX<- TMax[!nas_mmp,]
      filterednasTmin<- Tmin[!nas_mmp,]
      
      models_tmp <- tibble(tmax = filterednasTMAX$value, tmin = filterednasTmin$value) %>% mutate("20060101" = paste(tmax, tmin, sep = ","))
    
      
      tab <- tibble(date = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), 
                    tmax = round(models_tmp$tmax,5), 
                    tmin = round(models_tmp$tmin,5)) %>%  
        mutate(year = year(date), yday = yday(date)) %>% select(year, yday, tmax, tmin)
      nbyr <-  length(unique(tab$year))
      tstep <-  0
      lat <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"lat"] %>% str_replace(., ",", ".")
      lon <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"long"]%>% str_replace(., ",", ".")
      elev <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"elev"]%>% str_replace(., ",", ".")
      
      
      # write.table(tab,  paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F, col.names = F) 
      
      
      firstline <- paste( "punto", str_remove(punto_st, "point-"),"_pcp.pcp: Temperature data - file written with R, ",  gsub("\\(|\\)", "", Sys.time()), sep = "")
      second_line <- paste("nbyr", "tstep", "lat", "lon", "elev", sep = "     " )
      third_line <- tibble(nbyr, tstep, lat, lon, elev ) 
      third_line <- paste(third_line$nbyr, third_line$tstep, third_line$lat, third_line$lon, third_line$elev, sep = "  ")
      
      # con <- file(paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), "w" )
      
      writeLines(c(firstline,second_line, third_line ),paste(path_save, directorio, "/",  "punto", n, "_tmp.tmp", sep = "")) 
      write.table(tab,  file = paste(path_save, directorio, "/",  "punto", n, "_tmp.tmp", sep = ""), sep = "\t", col.names = F, row.names = F, quote =  F, append = T) 
      
      
      
      
      #write.table(models_tmp[,3], file = paste(path_save,  directorio, "/", "punto", n, "_tmp.tmp", sep = ""),quote = F, row.names = F)
      
    }}
  
  
  # Generar datos en formato SWAT para el modelo sin bisiestos
  
  modelo_sinbis <- tab_days_info %>% filter(., case == "day 29/02 no") %>% .$Modelo
  data_TMax <- TMAX_longer %>% filter(., Modelo == modelo_sinbis)
  data_Tmin <- Tmin_longer %>% filter(., Modelo == modelo_sinbis)
  
  nnnas <- is.na(data_TMax$value)
  
  data_TMax <- data_TMax[!nnnas,]
  data_Tmin <- data_Tmin[!nnnas,]
  
  
  # En los dias 29/02 vamos a repetir el dato del dia 28/02
  
  init_date <- dmy("01/01/2006")
  end_date <- dmy("31/12/2100")
  dates <- seq(init_date, end_date, 1)
  
  # A qué día de la secuencia corresponden los dias 29/02??
  
  bisiest_days <- tibble(dates) %>% mutate(nday = seq(1, length(dates), 1), month = month(dates), dayss = day(dates),  
                                           daymonth = paste(month, dayss, sep = "/")) %>% 
    filter(., daymonth == "2/29") %>% .$nday
  
  # Esto hay que hacerlo para cada punto, poner que la celda inexistente del dia 29/02 == al valor del 28/02
  
  anios <- 2006:2101
  
  # Crear filas vacias con los campos value y punto como nas
  filas_insert <- tibble(Año = anios[leap_year(2006:2100)], Mes = 2, Modelo = "IPSL-IPSL-CM5A-MR_r1i1p1_SMHI-RCA4_v1", Punto = NA, name = "D?.a.29", value = NA)
  # Tibble para ver los valores del dia 28/02
  values_prev_tib <- tibble(bis_day = bisiest_days, bis_yrnumb = seq(1, length(bisiest_days), 1)) %>% mutate(., prev_day = bis_day - bis_yrnumb)
  
  
  todos_puntos_TMax <- c()
  todos_puntos_Tmin <- c()
  for(i in 1:length(puntos)){
    punto <- paste("point-", i, sep = "")
    tmp_Max <- data_TMax %>%  filter(Punto == punto)
    tmp_min <- data_Tmin %>%  filter(Punto == punto)
    # Este no se altera, para ver los valores del dia 28
    data_set_TMax <- data_TMax %>%  filter(Punto == punto)
    data_set_Tmin <- data_Tmin %>%  filter(Punto == punto)
    
    for(m in 1:length(bisiest_days)){
      bisiestday <- bisiest_days[m]
      tmp_Max <- tibble::add_row(tmp_Max, .before = bisiestday, 
                                 Año = filas_insert[m, "Año"]$Año,
                                 Mes = filas_insert[m, "Mes"]$Mes,
                                 Modelo = filas_insert[m, "Modelo"]$Modelo,
                                 Punto = punto,
                                 name = filas_insert[m, "name"]$name,
                                 value = data_set_TMax[values_prev_tib[m, "prev_day"]$prev_day, "value"]$value)
      tmp_min <- tibble::add_row(tmp_min, .before = bisiestday, 
                                 Año = filas_insert[m, "Año"]$Año,
                                 Mes = filas_insert[m, "Mes"]$Mes,
                                 Modelo = filas_insert[m, "Modelo"]$Modelo,
                                 Punto = punto,
                                 name = filas_insert[m, "name"]$name,
                                 value = data_set_Tmin[values_prev_tib[m, "prev_day"]$prev_day, "value"]$value)
    }
    
    todos_puntos_TMax <- todos_puntos_TMax %>% rbind(., tmp_Max)
    todos_puntos_Tmin <- todos_puntos_Tmin %>% rbind(., tmp_min)
    
  }
  
  
  directorio <- paste(13, "_", modelo_sinbis, "/", sep = "")
  dir.create(paste(path_save, directorio, sep = ""))                   
  
  for(n in 1:length(puntos)){
    punto_st <- paste("point-", n, sep = "")
    
    tmp_save_files <- tibble(Punto = todos_puntos_TMax$Punto, tmax = todos_puntos_TMax$value, tmin = todos_puntos_Tmin$value) %>% 
      mutate("20060101" = paste(tmax, tmin, sep = ","))
  
    ffile <- tmp_save_files %>%  filter(Punto == punto_st)
    
    
    tab <- tibble(date = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), 
                  tmax = round(ffile$tmax,5), 
                  tmin = round(ffile$tmin,5)) %>%  
      mutate(year = year(date), yday = yday(date)) %>% select(year, yday, tmax, tmin)
    nbyr <-  length(unique(tab$year))
    tstep <-  0
    lat <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"lat"] %>% str_replace(., ",", ".")
    lon <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"long"]%>% str_replace(., ",", ".")
    elev <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"elev"]%>% str_replace(., ",", ".")
    
    
    # write.table(tab,  paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F, col.names = F) 
    
    
    firstline <- paste( "punto", str_remove(punto_st, "point-"),"_pcp.pcp: Temperature data - file written with R, ",  gsub("\\(|\\)", "", Sys.time()), sep = "")
    second_line <- paste("nbyr", "tstep", "lat", "lon", "elev", sep = "     " )
    third_line <- tibble(nbyr, tstep, lat, lon, elev ) 
    third_line <- paste(third_line$nbyr, third_line$tstep, third_line$lat, third_line$lon, third_line$elev, sep = "  ")
    
    # con <- file(paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), "w" )
    
    writeLines(c(firstline,second_line, third_line ),paste(path_save, directorio, "/",  "punto", n, "_tmp.tmp", sep = "")) 
    write.table(tab,  file = paste(path_save, directorio, "/",  "punto", n, "_tmp.tmp", sep = ""), sep = "\t", col.names = F, row.names = F, quote =  F, append = T) 
    
    
    
    
    
   # write.table(ffile[,4], file = paste(path_save,  directorio, "/", "punto", n, "_tmp.tmp", sep = ""),quote = F, row.names = F)
    
  }
  
  
  
  # Generar datos en formato SWAT para el modelo sin dias 31
  
  library(zoo)
  
  modelo_sin31s <- tab_days_info %>% filter(., case == "days 31 no, ends in 12/2099") %>% .$Modelo
  
  # Eliminamos los valores 31 que no tienen sentido (Feb, Apr, Jun, Sep, Nov)
  
  anios <- 2006:2100
  id_mod <- c(14:16)
  
  for(i in 1:length(modelo_sin31s)){
    
    modelo <- modelo_sin31s[i]
    data_TMax <- TMAX_longer %>% filter(., Modelo == modelo)
    data_Tmin <- Tmin_longer %>% filter(., Modelo == modelo)
    
    directorio <- paste(id_mod[i], "_", modelo_sin31s[i], "/", sep = "")
    dir.create(paste(path_save, directorio, sep = ""))  
    
    
    dias_inex <- data_TMax %>% mutate(dayseq = seq(1, length(data_TMax$Mes), 1)) %>% 
      subset(., Mes %in% c(2,4,6,9,11)) %>% 
      subset(., name == "Día.31") %>% .[,"dayseq"]
    
    
    data_TMax <- data_TMax[-dias_inex$dayseq,]
    data_Tmin <- data_Tmin[-dias_inex$dayseq,]
    
    dias_inex2 <- data_TMax %>% mutate(dayseq = seq(1, length(data_TMax$Mes), 1)) %>% 
      subset(., Mes %in% c(2)) %>% 
      subset(., name == "Día.30") %>% .[,"dayseq"]
    
    data_TMax <- data_TMax[-dias_inex2$dayseq,]
    data_Tmin <- data_Tmin[-dias_inex2$dayseq,]
    
    
    dias_inex3 <- data_TMax %>% mutate(dayseq = seq(1, length(data_TMax$Mes), 1)) %>% 
      subset(., Año %in% anios[!leap_year(2006:2100)]) %>% 
      subset(., Mes == 2) %>% 
      subset(., name == "Día.29")
    
    data_TMax <- data_TMax[-dias_inex3$dayseq,]
    data_Tmin <- data_Tmin[-dias_inex3$dayseq,]
    
    data_TMax$value <- zoo::na.locf(data_TMax$value)
    data_Tmin$value <- zoo::na.locf(data_Tmin$value)
    
    
    for(n in 1:length(puntos)){
      punto_st <- paste("point", n, sep = "-")
      
      dat_punt_Tmax <- data_TMax %>% filter(., Punto == punto_st) %>% .[,"value"]
      dat_punt_Tmin <- data_Tmin %>% filter(., Punto == punto_st) %>% .[,"value"]
      exp_val <- tibble(tmp_max = dat_punt_Tmax$value, tmp_min = dat_punt_Tmin$value) %>% 
        mutate("20060101" = paste(tmp_max, tmp_min, sep = ","))
     
      tab <- tibble(date = seq(dmy("01/01/2006"), dmy("31/12/2099"), 1), 
                    tmax = round(dat_punt_Tmax$value,5), 
                    tmin = round(dat_punt_Tmin$value,5)) %>%  
        mutate(year = year(date), yday = yday(date)) %>% select(year, yday, tmax, tmin)
      nbyr <-  length(unique(tab$year))
      tstep <-  0
      lat <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"lat"] %>% str_replace(., ",", ".")
      lon <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"long"]%>% str_replace(., ",", ".")
      elev <- geometry %>%  filter(., punto ==  punto_st) %>% .[,"elev"]%>% str_replace(., ",", ".")
      
      
      # write.table(tab,  paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), quote = F, row.names = F, col.names = F) 
      
      
      firstline <- paste( "punto", str_remove(punto_st, "point-"),"_pcp.pcp: Temperature data - file written with R, ",  gsub("\\(|\\)", "", Sys.time()), sep = "")
      second_line <- paste("nbyr", "tstep", "lat", "lon", "elev", sep = "     " )
      third_line <- tibble(nbyr, tstep, lat, lon, elev ) 
      third_line <- paste(third_line$nbyr, third_line$tstep, third_line$lat, third_line$lon, third_line$elev, sep = "  ")
      
      # con <- file(paste(path_save, directorio, "/",  "punto", n, "_pcp.pcp", sep = ""), "w" )
      
      writeLines(c(firstline,second_line, third_line ),paste(path_save, directorio, "/",  "punto", n, "_tmp.tmp", sep = "")) 
      write.table(tab,  file = paste(path_save, directorio, "/",  "punto", n, "_tmp.tmp", sep = ""), sep = "\t", col.names = F, row.names = F, quote =  F, append = T) 

      
      
     # write.table(exp_val[,3], file = paste(path_save,  directorio, "/", "punto", n, "_tmp.tmp", sep = ""),quote = F, row.names = F)
      
    }
    
  }  
  
  
  ####2.1 CALCULO DE LA MEDIA EN LA CUENCA A PARTIR DE LOS DATOS CREADOS ####

  # Bucle para extraer los datos y calcular la media de los 7 puntos
  # Con eso despues se exportan tibbles
  
  path_orig <- "D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_formato_SWAT/"
  
  escenarios <- c("RCP_4.5", "RCP_8.5")
  variables <- c("PCP_FILES", "TMP_FILES")
  
  setwd(paste(path_orig, escenarios[1], "/", variables[1], sep = ""))
  directorios <- list.dirs() %>% .[-1] %>% str_remove(., "./") %>% tibble(names = .) %>% 
    mutate(first = substr(.$names, start = 1, stop = 2)) %>% 
    mutate(num = as.numeric(str_remove(.$first, "_"))) %>% arrange(., num) %>% 
    .[, c(3,1)]
  
    #Lista vacia con varias sub-listas
     escenarios_result <- list(RCP_4.5 = list(
                                               PCP_4.5 = list(), 
                                               TMP_4.5 = list()), 
                               
                               RCP_8.5 = list(
                                               PCP_8.5 = list(), 
                                               TMP_8.5 = list()))
     
     # Para cada uno de los directorios con los 2 escenarios                                                                                           
     for(escenario in 1:length(escenarios)){
       
       setwd(paste(path_orig, escenarios[escenario],sep =  ""))
       dirs <- list.dirs(full.names = F, recursive = F)
     
     # Para cada una de las dos variables dentro de cada escenario                                                                                          
     for(variable in 1:length(dirs)){
       
       setwd(paste(path_orig, escenarios[escenario],"/", dirs[variable], sep = ""))
       
      if(dirs[variable] == "PCP_FILES"){
        
      # Para cada una de los 16 modelos para cada variables dentro de cada escenario                                                                                          
      for(model in 1:length(directorios$names[1:16])){
       
       setwd(paste(path_orig, escenarios[escenario],"/", dirs[variable], "/", directorios$names[model],sep = ""))
       
              files <- list.files() %>% .[c(1:2, 4:6, 8,9)]  # SE HAN SELECCIONADO 7 DE LOS 9 PUNTOS, 2 ESTABAN BASTANTE FUERA
              pcp_points <- c()
      
      # Para cada uno de los 9 puntos dentro de cada uno de los 16 modelos para cada variables dentro de cada escenario                                                                                          
     for(f in 1:length(files)){
  
       pcp_point <- read_table(files[f])
       pcp_points <- tibble(pcp_points, pcp_point,.name_repair = "minimal")
       pcp_aa <- pcp_points %>% apply(.,  1, mean) %>% 
                 tibble(nday = seq(1,length(pcp_point$`20060101`), 1), pcp_aa = .)
       
       pcp_model <- tibble(pcp_aa)
       }
              if(escenarios[escenario] == "RCP_4.5"){
                  escenarios_result$RCP_4.5$PCP_4.5[[model]] <- pcp_model}

              if(escenarios[escenario] == "RCP_8.5"){
                escenarios_result$RCP_8.5$PCP_8.5[[model]] <- pcp_model}
      }
      }
 
       if(dirs[variable] == "TMP_FILES"){
         for(model in 1:length(directorios$names[1:16])){
           
           setwd(paste(path_orig, escenarios[escenario],"/", dirs[variable], "/", directorios$names[model],sep = ""))
           
           files <- list.files() %>% .[c(1:2, 4:6, 8,9)]  # SE HAN SELECCIONADO 7 DE LOS 9 PUNTOS, 2 ESTABAN BASTANTE FUERA
           tmax_points <- c()
           tmin_points <- c()       
       
       
           for(f in 1:length(files)){

           tmp_point <- read.table(files[f],sep = ",",skip = 1,  header = F)
           
           tmax_points <- tibble(tmax_points, tmp_point$V1,.name_repair = "minimal")
           tmax_aa <-  tmax_points %>% apply(.,  1, mean) %>% tibble(nday = seq(1,length(tmp_point$V1), 1), Tmax_aa = .)
           
           tmin_points <- tibble(tmin_points, tmp_point$V2,.name_repair = "minimal")
           tmin_aa <-  tmin_points %>% apply(.,  1, mean) %>% tibble(nday = seq(1,length(tmp_point$V1), 1), tmin_aa = .)
           
           tmp_model <- tibble(nday = seq(1,length(tmp_point$V1), 1), tmax = tmax_aa$Tmax_aa, tmin = tmin_aa$tmin_aa)
             
           }
           
           if(escenarios[escenario] == "RCP_4.5"){
             escenarios_result$RCP_4.5$TMP_4.5[[model]] <- tmp_model}
           
           if(escenarios[escenario] == "RCP_8.5"){
             escenarios_result$RCP_8.5$TMP_8.5[[model]] <- tmp_model}
           }
       }
     }
       
     }
  

       names(escenarios_result$RCP_4.5$PCP_4.5) <- directorios$names
       names(escenarios_result$RCP_4.5$TMP_4.5) <- directorios$names
       names(escenarios_result$RCP_8.5$PCP_8.5) <- directorios$names
       names(escenarios_result$RCP_8.5$TMP_8.5) <- directorios$names
     
     
     
      write_rds(escenarios_result, "D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/list_cc_data.rds")
     
      
      escenarios_result <- read_rds("D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/list_cc_data.rds")  
      escenarios_result <- read_rds("C:/ASG/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/list_cc_data.rds")  
    

      
  #### 2.2 CREACIÓN DE TIBBLES CON LA MEDIA DE TODA LA CUENCA ####
      
# Extraer los valores a formato tibble. Cuidado con la diferncia de longitud entre escenaios

# Hemos transformado la lista a tibbles

#   pcp 4.5

pcp_4.5 <- escenarios_result$RCP_4.5$PCP_4.5


colnms <- c()
for(i in 1:length(pcp_4.5)){
  colnm <- names(pcp_4.5[i])
  colnms <- c(colnms, colnm)
}  

# Modelos 1-13
pcps <- c()
for(i in 1:13){
  pcp <- tibble(pcp_4.5[[i]][[2]])
  colnames(pcp) <- names(pcp_4.5[i])
  pcps <- tibble(pcps, pcp, .name_repair = "unique") 
}
# Modelos 14-16
pcps_kk <- c()
for(i in 14:16){
  pcp_kk <- tibble(pcp_4.5[[i]][[2]])
  colnames(pcp_kk) <- names(pcp_4.5[i])
  pcps_kk <- tibble(pcps_kk, pcp_kk, .name_repair = "unique")
}

pcps_kk[c(34303:34698),] <- NA

pcps_entero <- tibble(pcps, pcps_kk)


#creamos la tabla para la pcp del escenario 4.5 en el periodo indicaso y lo guardamos como csv

pcps_entero<- tibble(date  = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), pcps_entero)

 write.csv(x = pcps_entero,
           file = "D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_pcp_4.5.csv", 
           row.names = F, quote = F )
 
 rm(pcps_entero)


#   pcp 8.5

pcp_8.5 <- escenarios_result$RCP_8.5$PCP_8.5

colnms <- c()
for(i in 1:length(pcp_8.5)){
  colnm <- names(pcp_8.5[i])
  colnms <- c(colnms, colnm)
}  

# Modelos 1-13
pcps <- c()
for(i in 1:13){
  pcp <- tibble(pcp_8.5[[i]][[2]])
  colnames(pcp) <- names(pcp_8.5[i])
  pcps <- tibble(pcps, pcp, .name_repair = "unique")
}

# Modelos 14-16

pcps_kk <- c()
for(i in 14:16){
  pcp_kk <- tibble(pcp_8.5[[i]][[2]])
  colnames(pcp_kk) <- names(pcp_8.5[i])
  pcps_kk <- tibble(pcps_kk, pcp_kk, .name_repair = "unique")
}


pcps_kk[c(34334:34698),] <- NA

pcps_entero <- tibble(pcps, pcps_kk)

pcps_entero<- tibble(date  = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), pcps_entero)

write.csv(x = pcps_entero,
          file = "D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_pcp_8.5.csv", 
          row.names = F, quote = F )


#   TMAX 4.5

tmax_4.5 <- escenarios_result$RCP_4.5$TMP_4.5

colnms <- c()
for(i in 1:length(tmax_4.5)){
  colnm <- names(tmax_4.5[i])
  colnms <- c(colnms, colnm)
}  

# Modelos 1-13
tmaxs <- c()
for(i in 1:13){
  tmax <- tibble(tmax_4.5[[i]][[2]])
  colnames(tmax) <- names(tmax_4.5[i])
  tmaxs <- tibble(tmaxs, tmax, .name_repair = "unique")
}
# Modelos 14-16
tmaxs_kk <- c()
for(i in 14:16){
  tmax_kk <- tibble(tmax_4.5[[i]][[2]])
  colnames(tmax_kk) <- names(tmax_4.5[i])
  tmaxs_kk <- tibble(tmaxs_kk, tmax_kk, .name_repair = "unique")
}

tmaxs_kk[c(34303:34698),] <- NA

tmaxs_entero <- tibble(tmaxs, tmaxs_kk)
tmaxs_entero<- tibble(date  = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), tmaxs_entero)

write.csv(x = tmaxs_entero,
          file = "D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_tmax_4.5.csv", 
          row.names = F, quote = F )
rm(tmaxs_entero)


#   TMAX 8.5

tmax_8.5 <- escenarios_result$RCP_8.5$TMP_8.5

colnms <- c()
for(i in 1:length(tmax_8.5)){
  colnm <- names(tmax_8.5[i])
  colnms <- c(colnms, colnm)
}  

# Modelos 1-13
tmaxs <- c()
for(i in 1:13){
  tmax <- tibble(tmax_8.5[[i]][[2]])
  colnames(tmax) <- names(tmax_8.5[i])
  tmaxs <- tibble(tmaxs, tmax, .name_repair = "unique")
}
# Modelos 14-16
tmaxs_kk <- c()
for(i in 14:16){
  tmax_kk <- tibble(tmax_8.5[[i]][[2]])
  colnames(tmax_kk) <- names(tmax_8.5[i])
  tmaxs_kk <- tibble(tmaxs_kk, tmax_kk, .name_repair = "unique")
}

tmaxs_kk[c(34334:34698),] <- NA

tmaxs_entero <- tibble(tmaxs, tmaxs_kk)
tmaxs_entero<- tibble(date  = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), tmaxs_entero)

write.csv(x = tmaxs_entero,
          file = "D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_tmax_8.5.csv", 
          row.names = F, quote = F )
rm(tmaxs_entero)


#   TMin 4.5

tmin_4.5 <- escenarios_result$RCP_4.5$TMP_4.5

colnms <- c()
for(i in 1:length(tmin_4.5)){
  colnm <- names(tmin_4.5[i])
  colnms <- c(colnms, colnm)
}  

# Modelos 1-13
tmins <- c()
for(i in 1:13){
  tmin <- tibble(tmin_4.5[[i]][[3]])
  colnames(tmin) <- names(tmin_4.5[i])
  tmins <- tibble(tmins, tmin, .name_repair = "unique")
}
# Modelos 14-16
tmins_kk <- c()
for(i in 14:16){
  tmin_kk <- tibble(tmin_4.5[[i]][[3]])
  colnames(tmin_kk) <- names(tmin_4.5[i])
  tmins_kk <- tibble(tmins_kk, tmin_kk, .name_repair = "unique")
}

tmins_kk[c(34303:34698),] <- NA

tmins_entero <- tibble(tmins, tmins_kk)
tmins_entero<- tibble(date  = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), tmins_entero)

write.csv(x = tmins_entero,
          file = "D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_tmin_4.5.csv", 
          row.names = F, quote = F )
rm(tmins_entero)


#   TMin 8.5

tmin_8.5 <- escenarios_result$RCP_8.5$TMP_8.5

colnms <- c()
for(i in 1:length(tmin_8.5)){
  colnm <- names(tmin_8.5[i])
  colnms <- c(colnms, colnm)
}  

# Modelos 1-13
tmins <- c()
for(i in 1:13){
  tmin <- tibble(tmin_8.5[[i]][[3]])
  colnames(tmin) <- names(tmin_8.5[i])
  tmins <- tibble(tmins, tmin, .name_repair = "unique")
}
# Modelos 14-16
tmins_kk <- c()
for(i in 14:16){
  tmin_kk <- tibble(tmin_8.5[[i]][[3]])
  colnames(tmin_kk) <- names(tmin_8.5[i])
  tmins_kk <- tibble(tmins_kk, tmin_kk, .name_repair = "unique")
}

tmins_kk[c(34334:34698),] <- NA

tmins_entero <- tibble(tmins, tmins_kk)
tmins_entero<- tibble(date  = seq(dmy("01/01/2006"), dmy("31/12/2100"), 1), tmins_entero)

write.csv(x = tmins_entero,
          file = "D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_tmin_8.5.csv", 
          row.names = F, quote = F )
rm(tmins_entero)



####  3. Comparacion de modelos con Indices ####

tmp_pcp_daily <-read.csv("C:/ASG/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/TMP_PCP_OBS.csv")


pcp4.5<-  read.csv("C:/ASG/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_pcp_4.5.csv")
pcp_4.5_2006_2019 <-subset(pcp4.5, date>= as.Date("2006-01-01") & date <= as.Date("2019-12-31"))

pcp8.5<-  read.csv("C:/ASG/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_pcp_8.5.csv")
pcp_8.5_2006_2019 <-subset(pcp8.5, date>= as.Date("2006-01-01") & date <= as.Date("2019-12-31"))

tmax4.5<-  read.csv("C:/ASG/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_tmax_4.5.csv")
tmax_4.5_2006_2019 <-subset(tmax4.5, date>= as.Date("2006-01-01") & date <= as.Date("2019-12-31"))

tmax8.5<-  read.csv("C:/ASG/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_tmax_8.5.csv")
tmax_8.5_2006_2019 <-subset(tmax8.5, date>= as.Date("2006-01-01") & date <= as.Date("2019-12-31"))

tmin4.5<-  read.csv("C:/ASG/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_tmin_4.5.csv")
tmin_4.5_2006_2019 <-subset(tmin4.5, date>= as.Date("2006-01-01") & date <= as.Date("2019-12-31"))

tmin8.5<-  read.csv("C:/ASG/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_tmin_8.5.csv")
tmin_8.5_2006_2019 <-subset(tmin8.5, date>= as.Date("2006-01-01") & date <= as.Date("2019-12-31"))


# Creacion de una lista con los datos que se van a comparar

# Media y desviacion de datos observados

obs_tmin_mon <- tmp_pcp_daily %>%
  .[,c("date", "tmin")] %>% group_by(month(date)) %>% 
  summarise(min_tmp_month = mean(tmin), sd_obs_tmin = sd(tmin)) %>% 
  select(., "month" = `month(date)`, min_tmp_month, sd_obs_tmin)

obs_tmax_mon <- tmp_pcp_daily %>%
  .[,c("date", "tmax")] %>% group_by(month(date)) %>% 
  summarise(max_tmp_month = mean(tmax), sd_obs_tmax = sd(tmax)) %>% 
  select(., "month" = `month(date)`, max_tmp_month, sd_obs_tmax)


obs_pcp_mon <- tmp_pcp_daily %>% 
  .[, c("date", "pcp")] %>% group_by(year(date), month(date)) %>% 
  summarise(month_pcp = sum(pcp)) %>% 
  mutate(mon_year = my(paste(`month(date)`,`year(date)`, sep = "/"))) %>% 
  group_by(month(mon_year)) %>% summarise(mean_pcp_month = mean(month_pcp), 
                                          sd_pcp_month = sd(month_pcp)) %>% 
  select(., "month" = `month(mon_year)`, mean_pcp_month, sd_pcp_month)


# Scenarios data

# PCP4.5

pcp_mean_4.5 <- tibble(pcp_4.5_2006_2019) %>% mutate(date = ymd(date)) %>% 
  group_by(year(date), month(date)) %>% 
  summarise(across(starts_with("X"), sum)) %>% 
  mutate(mon_year = my(paste(`month(date)`,`year(date)`, sep = "/"))) %>% 
  group_by("month" = month(mon_year)) %>% summarise(across(starts_with("X"), mean)) %>% 
  pivot_longer(., -month) %>% mutate(Modelo = str_sub(name, 2,3)) %>% 
  mutate(Modelo = str_remove(Modelo, "_"), name = str_sub(name, 3, 90)) %>% 
  mutate(name = str_remove(name, "_")) %>% 
  rename(., "mean_pcp" = "value")

pcp_sd_4.5 <- tibble(pcp_4.5_2006_2019) %>% mutate(date = ymd(date)) %>% 
  group_by(year(date), month(date)) %>% 
  summarise(across(starts_with("X"), sum)) %>% 
  mutate(mon_year = my(paste(`month(date)`,`year(date)`, sep = "/"))) %>% 
  group_by("month" = month(mon_year)) %>% summarise(across(starts_with("X"), sd)) %>% 
  pivot_longer(., -month) %>% mutate(Modelo = str_sub(name, 2,3)) %>% 
  mutate(Modelo = str_remove(Modelo, "_"), name = str_sub(name, 3, 90)) %>% 
  mutate(name = str_remove(name, "_")) %>% 
  rename(., "sd_pcp" = "value")

pcp_4.5_compest <- pcp_mean_4.5 %>% left_join(., pcp_sd_4.5, by = c("month", "name", "Modelo")) %>% 
  .[,c("month","Modelo", "name", "mean_pcp", "sd_pcp")]%>% cbind(Escenario = "4.5")

# PCP8.5

pcp_mean_8.5 <- tibble(pcp_8.5_2006_2019) %>% mutate(date = ymd(date)) %>% 
  group_by(year(date), month(date)) %>% 
  summarise(across(starts_with("X"), sum)) %>% 
  mutate(mon_year = my(paste(`month(date)`,`year(date)`, sep = "/"))) %>% 
  group_by("month" = month(mon_year)) %>% summarise(across(starts_with("X"), mean)) %>% 
  pivot_longer(., -month) %>% mutate(Modelo = str_sub(name, 2,3)) %>% 
  mutate(Modelo = str_remove(Modelo, "_"), name = str_sub(name, 3, 90)) %>% 
  mutate(name = str_remove(name, "_")) %>% 
  rename(., "mean_pcp" = "value")

pcp_sd_8.5 <- tibble(pcp_8.5_2006_2019) %>% mutate(date = ymd(date)) %>% 
  group_by(year(date), month(date)) %>% 
  summarise(across(starts_with("X"), sum)) %>% 
  mutate(mon_year = my(paste(`month(date)`,`year(date)`, sep = "/"))) %>% 
  group_by("month" = month(mon_year)) %>% summarise(across(starts_with("X"), sd)) %>% 
  pivot_longer(., -month) %>% mutate(Modelo = str_sub(name, 2,3)) %>% 
  mutate(Modelo = str_remove(Modelo, "_"), name = str_sub(name, 3, 90)) %>% 
  mutate(name = str_remove(name, "_")) %>% 
  rename(., "sd_pcp" = "value")

pcp_8.5_compest <- pcp_mean_8.5 %>% left_join(., pcp_sd_8.5, by = c("month", "name", "Modelo")) %>% 
  .[,c("month","Modelo", "name", "mean_pcp", "sd_pcp")] %>% cbind(Escenario = "8.5")

# TMIN 4.5

tmp_min_4.5 <- tibble(tmin_4.5_2006_2019) %>% mutate(date = ymd(date)) %>% 
  group_by("month" = month(date)) %>% summarise(across(starts_with("X"), mean)) %>% 
  pivot_longer(., -month) %>% mutate(Modelo = str_sub(name, 2,3)) %>% 
  mutate(Modelo = str_remove(Modelo, "_"), name = str_sub(name, 3, 90)) %>% 
  mutate(name = str_remove(name, "_")) %>% 
  rename(., "mean_tmin" = "value")

tmp_min_sd_4.5 <- tibble(tmin_4.5_2006_2019) %>% mutate(date = ymd(date)) %>% 
  group_by("month" = month(date)) %>% summarise(across(starts_with("X"), sd)) %>% 
  pivot_longer(., -month) %>% mutate(Modelo = str_sub(name, 2,3)) %>% 
  mutate(Modelo = str_remove(Modelo, "_"), name = str_sub(name, 3, 90)) %>% 
  mutate(name = str_remove(name, "_")) %>% 
  rename(., "sd_tmin" = "value")

tmin_4.5_compest <- tmp_min_4.5 %>% left_join(., tmp_min_sd_4.5, by = c("month", "name", "Modelo")) %>% 
  .[,c("month","Modelo", "name", "mean_tmin", "sd_tmin")] %>% cbind(Escenario = "4.5")

# TMAX 4.5

tmp_max_4.5 <- tibble(tmax_4.5_2006_2019) %>% mutate(date = ymd(date)) %>% 
  group_by("month" = month(date)) %>% summarise(across(starts_with("X"), mean)) %>% 
  pivot_longer(., -month) %>% mutate(Modelo = str_sub(name, 2,3)) %>% 
  mutate(Modelo = str_remove(Modelo, "_"), name = str_sub(name, 3, 90)) %>% 
  mutate(name = str_remove(name, "_")) %>% 
  rename(., "mean_tmax" = "value")

tmp_max_sd_4.5 <- tibble(tmax_4.5_2006_2019) %>% mutate(date = ymd(date)) %>% 
  group_by("month" = month(date)) %>% summarise(across(starts_with("X"), sd)) %>% 
  pivot_longer(., -month) %>% mutate(Modelo = str_sub(name, 2,3)) %>% 
  mutate(Modelo = str_remove(Modelo, "_"), name = str_sub(name, 3, 90)) %>% 
  mutate(name = str_remove(name, "_")) %>% 
  rename(., "sd_tmax" = "value")

tmax_4.5_compest <- tmp_max_4.5 %>% left_join(., tmp_max_sd_4.5, by = c("month", "name", "Modelo")) %>% 
  .[,c("month","Modelo", "name", "mean_tmax", "sd_tmax")] %>% cbind(Escenario = "4.5")

# TMIN 8.5

tmp_min_8.5 <- tibble(tmin_8.5_2006_2019) %>% mutate(date = ymd(date)) %>% 
  group_by("month" = month(date)) %>% summarise(across(starts_with("X"), mean)) %>% 
  pivot_longer(., -month) %>% mutate(Modelo = str_sub(name, 2,3)) %>% 
  mutate(Modelo = str_remove(Modelo, "_"), name = str_sub(name, 3, 90)) %>% 
  mutate(name = str_remove(name, "_")) %>% 
  rename(., "mean_tmin" = "value")

tmp_min_sd_8.5 <- tibble(tmin_8.5_2006_2019) %>% mutate(date = ymd(date)) %>% 
  group_by("month" = month(date)) %>% summarise(across(starts_with("X"), sd)) %>% 
  pivot_longer(., -month) %>% mutate(Modelo = str_sub(name, 2,3)) %>% 
  mutate(Modelo = str_remove(Modelo, "_"), name = str_sub(name, 3, 90)) %>% 
  mutate(name = str_remove(name, "_")) %>% 
  rename(., "sd_tmin" = "value")

tmin_8.5_compest <- tmp_min_8.5 %>% left_join(., tmp_min_sd_8.5, by = c("month", "name", "Modelo")) %>% 
  .[,c("month","Modelo", "name", "mean_tmin", "sd_tmin")] %>% cbind(Escenario = "8.5")

# TMAX 8.5

tmp_max_8.5 <- tibble(tmax_8.5_2006_2019) %>% mutate(date = ymd(date)) %>% 
  group_by("month" = month(date)) %>% summarise(across(starts_with("X"), mean)) %>% 
  pivot_longer(., -month) %>% mutate(Modelo = str_sub(name, 2,3)) %>% 
  mutate(Modelo = str_remove(Modelo, "_"), name = str_sub(name, 3, 90)) %>% 
  mutate(name = str_remove(name, "_")) %>% 
  rename(., "mean_tmax" = "value")

tmp_max_sd_8.5 <- tibble(tmax_8.5_2006_2019) %>% mutate(date = ymd(date)) %>% 
  group_by("month" = month(date)) %>% summarise(across(starts_with("X"), sd)) %>% 
  pivot_longer(., -month) %>% mutate(Modelo = str_sub(name, 2,3)) %>% 
  mutate(Modelo = str_remove(Modelo, "_"), name = str_sub(name, 3, 90)) %>% 
  mutate(name = str_remove(name, "_")) %>% 
  rename(., "sd_tmax" = "value")

tmax_8.5_compest <- tmp_max_8.5 %>% left_join(., tmp_max_sd_8.5, by = c("month", "name", "Modelo")) %>% 
  .[,c("month", "Modelo", "name", "mean_tmax", "sd_tmax")] %>% cbind(Escenario = "8.5")


list("obs_pcp" = obs_pcp_mon,
     "obs_tmin" = obs_tmin_mon,
     "obs_tmax" = obs_tmax_mon,
     "pcp_4.5" = pcp_4.5_compest,
     "pcp_8.5" = pcp_8.5_compest,
     "tmin_4.5" = tmin_4.5_compest,
     "tmin_8.5" = tmin_8.5_compest,
     "tmax_4.5" = tmax_4.5_compest,
     "tmax_8.5" = tmax_8.5_compest) %>% write_rds(., "D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/comp_media_sd_modelos.rds")


comp_meansd <- read_rds("C:/ASG/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/comp_media_sd_modelos.rds")


# Calculo de indices: Para cada mes, para cada variablr, para cada estadísitco y para cada modelo se calcula el
# error relativo (Valor_esc - Valor_obs / (Valor_obs)), y después se suman todos los índices. El modelo con menor
# valor de índice total el el mejor

models <- unique(comp_meansd$pcp_4.5$Modelo)

# PCP mean and sd errors
ids_pcp4.5_mean <- c()
ids_pcp4.5_sd <- c()
for(i in 1:length(models)){
  tibb <- comp_meansd$pcp_4.5 %>% left_join(., comp_meansd$obs_pcp, "month") %>% 
    filter(Modelo == models[i]) %>% mutate(mean_err = ((abs(mean_pcp-mean_pcp_month )/mean_pcp_month )),
                                           sd_err = (abs(sd_pcp  - sd_pcp_month )/sd_pcp_month))
  id_pcp4.5_mean <- sum(tibb$mean_err)
  ids_pcp4.5_mean <- c(ids_pcp4.5_mean, id_pcp4.5_mean)
  id_pcp4.5_sd <- sum(tibb$sd_err)
  ids_pcp4.5_sd <- c(ids_pcp4.5_sd, id_pcp4.5_sd)
}

ids_pcp8.5_mean <- c()
ids_pcp8.5_sd <- c()
for(i in 1:length(models)){
  tibb <- comp_meansd$pcp_8.5 %>% left_join(., comp_meansd$obs_pcp, "month") %>% 
    filter(Modelo == models[i]) %>% mutate(mean_err = ((abs(mean_pcp-mean_pcp_month )/mean_pcp_month )),
                                           sd_err = (abs(sd_pcp  - sd_pcp_month )/sd_pcp_month))
  id_pcp8.5_mean <- sum(tibb$mean_err)
  ids_pcp8.5_mean <- c(ids_pcp8.5_mean, id_pcp8.5_mean)
  id_pcp8.5_sd <- sum(tibb$mean_err)
  ids_pcp8.5_sd <- c(ids_pcp8.5_sd, id_pcp8.5_sd)
}

# TMIN mean and sd errors
ids_tmin4.5_mean <- c()
ids_tmin4.5_sd <- c()
for(i in 1:length(models)){
  tibb <- comp_meansd$tmin_4.5 %>% left_join(., comp_meansd$obs_tmin, "month") %>% 
    filter(Modelo == models[i]) %>% mutate(mean_err = ((abs(mean_tmin- min_tmp_month )/min_tmp_month )),
                                           sd_err = (abs(sd_tmin  - sd_obs_tmin )/sd_obs_tmin))
  id_tmin4.5_mean <- sum(tibb$mean_err)
  ids_tmin4.5_mean <- c(ids_tmin4.5_mean, id_tmin4.5_mean)
  id_tmin4.5_sd <- sum(tibb$sd_err)
  ids_tmin4.5_sd <- c(ids_tmin4.5_sd, id_tmin4.5_sd)
}

ids_tmin8.5_mean <- c()
ids_tmin8.5_sd <- c()
for(i in 1:length(models)){
  tibb <- comp_meansd$tmin_8.5 %>% left_join(., comp_meansd$obs_tmin, "month") %>% 
    filter(Modelo == models[i]) %>% mutate(mean_err = ((abs(mean_tmin- min_tmp_month )/min_tmp_month )),
                                           sd_err = (abs( sd_tmin  - sd_obs_tmin )/sd_obs_tmin))
  id_tmin8.5_mean <- sum(tibb$mean_err)
  ids_tmin8.5_mean <- c(ids_tmin8.5_mean, id_tmin8.5_mean)
  id_tmin8.5_sd <- sum(tibb$sd_err)
  ids_tmin8.5_sd <- c(ids_tmin8.5_sd, id_tmin8.5_sd)
}

# TMAX mean and sd errors

ids_tmax4.5_mean <- c()
ids_tmax4.5_sd <- c()
for(i in 1:length(models)){
  tibb <- comp_meansd$tmax_4.5 %>% left_join(., comp_meansd$obs_tmax, "month") %>% 
    filter(Modelo == models[i]) %>% mutate(mean_err = ((abs(mean_tmax- max_tmp_month )/max_tmp_month )),
                                           sd_err = (abs(sd_tmax  - sd_obs_tmax )/sd_obs_tmax))
  id_tmax4.5_mean <- sum(tibb$mean_err)
  ids_tmax4.5_mean <- c(ids_tmax4.5_mean, id_tmax4.5_mean)
  id_tmax4.5_sd <- sum(tibb$sd_err)
  ids_tmax4.5_sd <- c(ids_tmax4.5_sd, id_tmax4.5_sd)
}

ids_tmax8.5_mean <- c()
ids_tmax8.5_sd <- c()
for(i in 1:length(models)){
  tibb <- comp_meansd$tmax_8.5 %>% left_join(., comp_meansd$obs_tmax, "month") %>% 
    filter(Modelo == models[i]) %>% mutate(mean_err = ((abs(mean_tmax- max_tmp_month )/max_tmp_month )),
                                           sd_err = (abs(sd_tmax  - sd_obs_tmax )/sd_obs_tmax))
  id_tmax8.5_mean <- sum(tibb$mean_err)
  ids_tmax8.5_mean <- c(ids_tmax8.5_mean, id_tmax8.5_mean)
  id_tmax8.5_sd <- sum(tibb$sd_err)
  ids_tmax8.5_sd <- c(ids_tmax8.5_sd, id_tmax8.5_sd)
}


fin_id_4.5 <- tibble(Modelo = models, ids_pcp4.5_mean, ids_pcp4.5_sd, ids_tmax4.5_mean, ids_tmax4.5_sd, ids_tmin4.5_mean, ids_tmin4.5_sd) %>% 
  group_by(Modelo) %>% summarise(ids_fin_4.5 = ids_pcp4.5_mean+ ids_pcp4.5_sd +0.5*ids_tmax4.5_mean+ 0.5*ids_tmax4.5_sd +0.5*ids_tmin4.5_mean +0.5*ids_tmin4.5_sd ) %>% arrange(., as.numeric(Modelo))
fin_id_8.5 <- tibble(Modelo = models, ids_pcp8.5_mean, ids_pcp8.5_sd, ids_tmax8.5_mean, ids_tmax8.5_sd, ids_tmin8.5_mean, ids_tmin8.5_sd) %>% 
  group_by(Modelo) %>% summarise(ids_fin_8.5 = ids_pcp8.5_mean+ ids_pcp8.5_sd +0.5*ids_tmax8.5_mean+ 0.5*ids_tmax8.5_sd +0.5*ids_tmin8.5_mean +0.5*ids_tmin8.5_sd ) %>% arrange(., as.numeric(Modelo))

fin_id_4.5 %>% left_join(., fin_id_8.5, "Modelo") %>% arrange(., as.numeric(Modelo)) %>% mutate(id_mean = (ids_fin_4.5 +ids_fin_8.5 )/2) %>% 
arrange(., id_mean)


tab_join <- tab_days_info %>% select(Modelo, esc_number) %>% rename(model = Modelo, Modelo = esc_number) %>% mutate(Modelo = as.character(Modelo))

tab_fin <- fin_id_4.5 %>% left_join(., fin_id_8.5, "Modelo") %>% arrange(., as.numeric(Modelo)) %>% mutate(id_mean = (ids_fin_4.5 +ids_fin_8.5 )/2) %>% 
           arrange(., id_mean) %>% left_join(., tab_join, "Modelo") %>% select(Modelo, model, ids_fin_4.5, ids_fin_8.5, id_mean) 


colnames(tab_fin) <- c("Model ID", "Model", "RCP 4.5 Index error", "RCP 8.5 Index error", "Average index error")
  
 write.csv(tab_fin, file = "C:/ASG/datos_cc_guajaraz/error_table.csv", quote = F, row.names = F) 










#### 4. Comparación de modelos con estadísticos (R2, RMSE, PBIAS)####
library(hydroGOF)


# Observados
tmp_pcp_daily <- read.csv("D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/TMP_PCP_OBS.csv")

# Modelos datos diarios
pcp4.5<-  read.csv("D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_pcp_4.5.csv")
pcp_4.5_2006_2019 <-subset(pcp4.5, date>= as.Date("2006-01-01") & date <= as.Date("2019-12-31"))

pcp8.5<-  read.csv("D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_pcp_8.5.csv")
pcp_8.5_2006_2019 <-subset(pcp8.5, date>= as.Date("2006-01-01") & date <= as.Date("2019-12-31"))

tmax4.5<-  read.csv("D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_tmax_4.5.csv")
tmax_4.5_2006_2019 <-subset(tmax4.5, date>= as.Date("2006-01-01") & date <= as.Date("2019-12-31"))

tmax8.5<-  read.csv("D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_tmax_8.5.csv")
tmax_8.5_2006_2019 <-subset(tmax8.5, date>= as.Date("2006-01-01") & date <= as.Date("2019-12-31"))

tmin4.5<-  read.csv("D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_tmin_4.5.csv")
tmin_4.5_2006_2019 <-subset(tmin4.5, date>= as.Date("2006-01-01") & date <= as.Date("2019-12-31"))

tmin8.5<-  read.csv("D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tibble_tmin_8.5.csv")
tmin_8.5_2006_2019 <-subset(tmin8.5, date>= as.Date("2006-01-01") & date <= as.Date("2019-12-31"))



#### 4.1. PCP ####

#De los datos observados cogemos la fecha y la precipitacion, despues agrupamos por año mes y sumamos

pcp_obs_mensual <- tibble(date =tmp_pcp_daily$date, pcp=tmp_pcp_daily$pcp)%>% mutate(mes = month(date), anio = year(date)) %>%
  mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% group_by(mon_year) %>% 
  . [,c(5,2)]  %>% summarise(pcp_mensual= sum(pcp))


# 4.5
#Agrupamos por año y mes y sumamos las precipitaciones, obteniendo un valor mensual, ponemos primero la columna de año y mes y quitamos las dos primeras

pcp_4.5_aniomes <-pcp_4.5_2006_2019 %>% mutate(mes = month(date), anio = year(date)) %>% group_by(anio, mes)
pcp_4.5_comp<- pcp_4.5_aniomes %>% summarise(across(starts_with("X"), sum)) %>% mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% 
  . [,c(19,3:18)] 

plotlines <- pcp_4.5_comp %>% pivot_longer(., -mon_year) %>% ggplot(., aes( x = mon_year, y = value, color = name))+geom_line()


#una vez tenemos los dos conjuntos de datos en el formato adecuado para poder compararlos sacamos los estadísticos

nse_q <- pcp_4.5_comp %>%
  select(-mon_year) %>% 
  map_dbl(., ~NSE(.x,pcp_obs_mensual$pcp_mensual ))

pbias_q <- pcp_4.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~pbias(.x, pcp_obs_mensual$pcp_mensual))

rsq_r2 <-  function(x, y ) cor(x, y)^2


r2_q <- pcp_4.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~rsq_r2(.x, pcp_obs_mensual$pcp_mensual))

rmse_q <-  pcp_4.5_comp%>% select(-mon_year) %>% map_dbl(., ~rmse(.x, pcp_obs_mensual$pcp_mensual))


rmsecol <- rmse_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")
r2col <- r2_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")
pbiascol <- pbias_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")


Tibble_pcp4.5<-tibble(Escenario = "RCP 4.5", "Variable" = "PCP", Modelo =  rmsecol$Modelo, RMSE = rmsecol$., R2 = r2col$., PBIAS = pbiascol$.)

# 8.5

#Agrupamos por año y mes y sumamos las precipitaciones, obteniendo un valor mensual, ponemos primero la columna de año y mes y quitamos las dos primeras

pcp_8.5_aniomes <-pcp_8.5_2006_2019 %>% mutate(mes = month(date), anio = year(date)) %>% group_by(anio, mes)
pcp_8.5_comp<- pcp_8.5_aniomes %>% summarise(across(starts_with("X"), sum)) %>% mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% 
  . [,c(19,3:18)] 

plotlines <- pcp_8.5_comp %>% pivot_longer(., -mon_year) %>% ggplot(., aes( x = mon_year, y = value, color = name))+geom_line()


#una vez tenemos los dos conjuntos de datos en el formato adecuado para poder compararlos sacamos los estadísticos

nse_q <- pcp_8.5_comp %>%
  select(-mon_year) %>% 
  map_dbl(., ~NSE(.x,pcp_obs_mensual$pcp_mensual ))

pbias_q <- pcp_8.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~pbias(.x, pcp_obs_mensual$pcp_mensual))

rsq_r2 <-  function(x, y ) cor(x, y)^2


r2_q <- pcp_8.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~rsq_r2(.x, pcp_obs_mensual$pcp_mensual))

rmse_q <-  pcp_8.5_comp%>% select(-mon_year) %>% map_dbl(., ~rmse(.x, pcp_obs_mensual$pcp_mensual))


rmsecol <- rmse_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")
r2col <- r2_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")
pbiascol <- pbias_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")


Tibble_pcp8.5<-tibble(Escenario = "RCP 8.5", "Variable" = "PCP", Modelo =  rmsecol$Modelo, RMSE = rmsecol$., R2 = r2col$., PBIAS = pbiascol$.)


#### 4.2. TMPS ####

#De los datos observados cogemos la fecha y las temperaturas, despues agrupamos por año mes y calculamos media

Tmax_obs_mensual <- tibble(date =tmp_pcp_daily$date, tmax=tmp_pcp_daily$tmax)%>% mutate(mes = month(date), anio = year(date)) %>%
  mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% group_by(mon_year) %>% 
  . [,c(5,2)]  %>% summarise(tmax_mensual= mean(tmax))

Tmin_obs_mensual <- tibble(date =tmp_pcp_daily$date, tmin=tmp_pcp_daily$tmin)%>% mutate(mes = month(date), anio = year(date)) %>%
  mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% group_by(mon_year) %>% 
  . [,c(5,2)]  %>% summarise(tmin_mensual= mean(tmin))

# TMAX

#4.5
tmax_4.5_aniomes <-tmax_4.5_2006_2019 %>% mutate(mes = month(date), anio = year(date)) %>% group_by(anio, mes)
tmax_4.5_comp<- tmax_4.5_aniomes %>% summarise(across(starts_with("X"), mean)) %>% mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% 
  . [,c(19,3:18)] 


nse_q <- tmax_4.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~NSE(.x,Tmax_obs_mensual$tmax_mensual ))

pbias_q <- tmax_4.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~pbias(.x, Tmax_obs_mensual$tmax_mensual))

rsq_r2 <-  function(x, y ) cor(x, y)^2


r2_q <- tmax_4.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~rsq_r2(.x, Tmax_obs_mensual$tmax_mensual))

rmse_q <-  tmax_4.5_comp%>% select(-mon_year) %>% map_dbl(., ~rmse(.x, Tmax_obs_mensual$tmax_mensual))


rmsecol <- rmse_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")
r2col <- r2_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")
pbiascol <- pbias_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")


Tibble_tmax4.5<-tibble(Escenario = "RCP 4.5", "Variable" = "TMAX", Modelo =  rmsecol$Modelo, RMSE = rmsecol$., R2 = r2col$., PBIAS = pbiascol$.)

#8.5
tmax_8.5_aniomes <-tmax_8.5_2006_2019 %>% mutate(mes = month(date), anio = year(date)) %>% group_by(anio, mes)
tmax_8.5_comp<- tmax_8.5_aniomes %>% summarise(across(starts_with("X"), mean)) %>% mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% 
  . [,c(19,3:18)] 


nse_q <- tmax_8.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~NSE(.x,Tmax_obs_mensual$tmax_mensual ))

pbias_q <- tmax_8.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~pbias(.x, Tmax_obs_mensual$tmax_mensual))

rsq_r2 <-  function(x, y ) cor(x, y)^2


r2_q <- tmax_8.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~rsq_r2(.x, Tmax_obs_mensual$tmax_mensual))

rmse_q <-  tmax_8.5_comp%>% select(-mon_year) %>% map_dbl(., ~rmse(.x, Tmax_obs_mensual$tmax_mensual))


rmsecol <- rmse_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")
r2col <- r2_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")
pbiascol <- pbias_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")


Tibble_tmax8.5<-tibble(Escenario = "RCP 8.5", "Variable" = "TMAX", Modelo =  rmsecol$Modelo, RMSE = rmsecol$., R2 = r2col$., PBIAS = pbiascol$.)


# Tmin

# 4.5

tmin_4.5_aniomes <-tmin_4.5_2006_2019 %>% mutate(mes = month(date), anio = year(date)) %>% group_by(anio, mes)

tmin_4.5_comp<- tmin_4.5_aniomes %>% summarise(across(starts_with("X"), mean)) %>% mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% 
  . [,c(19,3:18)] 



#una vez tenemos los dos conjuntos de datos en el formato adecuado para poder compararlos sacamos los estadísticos

nse_q <- tmin_4.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~NSE(.x,Tmin_obs_mensual$tmin_mensual))

pbias_q <- tmin_4.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~pbias(.x, Tmin_obs_mensual$tmin_mensual))

rsq_r2 <-  function(x, y ) cor(x, y)^2


r2_q <- tmin_4.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~rsq_r2(.x, Tmin_obs_mensual$tmin_mensual))

rmse_q <-  tmin_4.5_comp%>% select(-mon_year) %>% map_dbl(., ~rmse(.x, Tmin_obs_mensual$tmin_mensual))

rmsecol <- rmse_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")
r2col <- r2_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")
pbiascol <- pbias_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")


Tibble_tmin4.5<-tibble(Escenario = "RCP 4.5", "Variable" = "TMIN", Modelo =  rmsecol$Modelo, RMSE = rmsecol$., R2 = r2col$., PBIAS = pbiascol$.)

# 8.5

tmin_8.5_aniomes <-tmin_8.5_2006_2019 %>% mutate(mes = month(date), anio = year(date)) %>% group_by(anio, mes)

tmin_8.5_comp<- tmin_8.5_aniomes %>% summarise(across(starts_with("X"), mean)) %>% mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% 
  . [,c(19,3:18)] 



#una vez tenemos los dos conjuntos de datos en el formato adecuado para poder compararlos sacamos los estadísticos

nse_q <- tmin_8.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~NSE(.x,Tmin_obs_mensual$tmin_mensual))

pbias_q <- tmin_8.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~pbias(.x, Tmin_obs_mensual$tmin_mensual))

rsq_r2 <-  function(x, y ) cor(x, y)^2


r2_q <- tmin_8.5_comp %>% 
  select(-mon_year) %>% 
  map_dbl(., ~rsq_r2(.x, Tmin_obs_mensual$tmin_mensual))

rmse_q <-  tmin_8.5_comp%>% select(-mon_year) %>% map_dbl(., ~rmse(.x, Tmin_obs_mensual$tmin_mensual))

rmsecol <- rmse_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")
r2col <- r2_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")
pbiascol <- pbias_q %>%  tibble(.) %>% rownames_to_column(., var ="Modelo")


Tibble_tmin8.5<-tibble(Escenario = "RCP 8.5", "Variable" = "TMIN", Modelo =  rmsecol$Modelo, RMSE = rmsecol$., R2 = r2col$., PBIAS = pbiascol$.)


#### 4.3 Tabla unida estadisticas modelos ####


tabla_estadisticos_modelosclim <- Tibble_pcp4.5 %>% rbind(Tibble_pcp8.5) %>% 
  rbind(Tibble_tmax4.5) %>% rbind( Tibble_tmax8.5)%>% 
  rbind( Tibble_tmin4.5) %>% rbind(Tibble_tmin8.5)

write.csv(tabla_estadisticos_modelosclim, 
          "D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tabla_estadisticos_all.csv",  
          row.names = F, quote = F)


tabla_estadisticos_modelosclim <- read.csv("D:/R_wd/Repositorio_ACMA/datos_cc_guajaraz/datos_cc_guajaraz_tibbles/tabla_estadisticos_all.csv")


r2_COMP <- tabla_estadisticos_modelosclim %>% 
  mutate(., Modelo = factor(Modelo,levels = c(1:16))) %>% 
  # pivot_longer(., -c(Escenario, Variable, Modelo)) %>% 
  ggplot(aes(x = Modelo))+
  geom_point(aes(y = R2, color = Escenario, shape = Variable), size = 4)+facet_wrap("Variable", scales = "free" )+
  scale_shape(guide = "none")+scale_color_discrete(guide = "none")+xlab("")+ggtitle("Statistical comparison of climate models performance (reference period 2006-2019)")

RMSE_COMP <- tabla_estadisticos_modelosclim %>% 
  mutate(., Modelo = factor(Modelo,levels = c(1:16))) %>% 
  # pivot_longer(., -c(Escenario, Variable, Modelo)) %>% 
  ggplot(aes(x = Modelo))+
  geom_point(aes(y = RMSE, color = Escenario, shape = Variable), size = 4)+facet_wrap("Variable", scales = "free" )+
  scale_shape(guide = "none")+scale_color_discrete(guide = "none")+xlab("")


PBIAS_COMP <- tabla_estadisticos_modelosclim %>% 
  mutate(., Modelo = factor(Modelo,levels = c(1:16))) %>% 
  # pivot_longer(., -c(Escenario, Variable, Modelo)) %>% 
  ggplot(aes(x = Modelo))+
  geom_point(aes(y = PBIAS, color = Escenario, shape = Variable), size = 4)+facet_wrap("Variable", scales = "free" )

est_plot <- r2_COMP / RMSE_COMP / PBIAS_COMP



ranktmax_tab  <- tabla_estadisticos_modelosclim %>% 
  group_by(Modelo) %>% 
  filter(Variable == "TMAX") %>% #, Modelo %in% c(4,7,6,14,9)) %>% 
  summarise(RMSE_mn = mean(RMSE),
            R2_mn = mean(R2), 
            PBIAS_mn = mean(abs(PBIAS))) %>% ungroup(.) %>% 
  mutate(rank_rmse = rank(-RMSE_mn), 
         rank_pbias = rank(-PBIAS_mn), 
         rank_r2 = rank(R2_mn))%>% 
  mutate(rank_fin = rank_rmse + rank_pbias+ rank_r2) %>% arrange(., -rank_fin)

ranktmin_tabn <-   tabla_estadisticos_modelosclim %>% 
  group_by(Modelo) %>% 
  filter(Variable == "TMIN") %>% #, Modelo %in% c(4,7,6,14,9)) %>% 
  summarise(RMSE_mn = mean(RMSE),
            R2_mn = mean(R2), 
            PBIAS_mn = mean(abs(PBIAS))) %>% ungroup(.) %>% 
  mutate(rank_rmse = rank(-RMSE_mn), 
         rank_pbias = rank(-PBIAS_mn), 
         rank_r2 = rank(R2_mn))%>% 
  mutate(rank_fin = rank_rmse + rank_pbias+ rank_r2) %>% arrange(., -rank_fin)

rankpcp_tab <- tabla_estadisticos_modelosclim %>% 
  group_by(Modelo) %>% 
  filter(Variable == "PCP") %>% #, Modelo %in% c(4,7,6,14,9)) %>% 
  summarise(RMSE_mn = mean(RMSE),
            R2_mn = mean(R2), 
            PBIAS_mn = mean(abs(PBIAS))) %>% ungroup(.) %>% 
  mutate(rank_rmse = rank(-RMSE_mn), 
         rank_pbias = rank(-PBIAS_mn), 
         rank_r2 = rank(R2_mn))%>% 
  mutate(rank_fin = rank_rmse + rank_pbias+ rank_r2) %>% arrange(., -rank_fin)
            

a <- tibble(pcp_mod = rankpcp_tab$Modelo, pcp_rank = rankpcp_tab$rank_fin)
b <- tibble(tmax_mod = ranktmax_tab$Modelo, tmax_rank = ranktmax_tab$rank_fin)
c <- tibble(tmin_mod = ranktmin_tabn$Modelo, tmin_rank = ranktmin_tabn$rank_fin)



a %>% left_join(., b, c("pcp_mod" = "tmax_mod")) %>% left_join(., c,  c("pcp_mod" = "tmin_mod")) %>% group_by(pcp_mod) %>% 
  summarise(rank_med = mean(c(pcp_rank, 0.5*tmax_rank, 0.5*tmin_rank))) %>% arrange(., -rank_med)


##### 5. Gráficos de los modelos vs observados ####

selected <- c(4,6,7, 9, 14)

# PCP

pcp_obs_mensual <- tibble(date =tmp_pcp_daily$date, pcp=tmp_pcp_daily$pcp)%>% mutate(mes = month(date), anio = year(date)) %>%
  mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% group_by(mon_year) %>% 
  . [,c(5,2)]  %>% summarise(pcp_mensual= sum(pcp))

tmin_obs_mensual <- tibble(date =tmp_pcp_daily$date, tmin=tmp_pcp_daily$tmin)%>% mutate(mes = month(date), anio = year(date)) %>%
  mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% group_by(mon_year) %>% 
  . [,c(5,2)]  %>% summarise(tmin_mensual= mean(tmin))




pcp_4.5_aniomes <-pcp_4.5_2006_2019 %>% mutate(mes = month(date), anio = year(date)) %>% group_by(anio, mes)
pcp_4.5_comp<- pcp_4.5_aniomes %>% summarise(across(starts_with("X"), sum)) %>% mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% 
  . [,c(19,3:18)] 


pcp_4.5_forplot <- pcp_4.5_comp %>% cbind(pcp_obs = pcp_obs_mensual$pcp_mensual) %>% pivot_longer(., -c(mon_year, pcp_obs)) %>% 
  mutate(Model = str_sub(name, 2,3), Model = str_remove(Model, "_"), Model = factor(Model, levels = c(1:16))) %>% 
  mutate(name = str_sub(name, 4, 90), name = str_remove(name, "_")) 


ggplotly(pcp_4.5_forplot %>% filter(., Model %in% selected) %>% 
  ggplot(aes(x = mon_year, ))+
  geom_line(aes(y = value, color = Model))+
  geom_line(aes(y = pcp_obs), color = "blue", linetype = 2, linewidth = 0.9))



tmin_4.5_aniomes <-tmin_4.5_2006_2019 %>% mutate(mes = month(date), anio = year(date)) %>% group_by(anio, mes)
tmin_4.5_comp<- tmin_4.5_aniomes %>% summarise(across(starts_with("X"), mean)) %>% mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% 
  . [,c(19,3:18)] 


tmin_4.5_forplot <- tmin_4.5_comp %>% cbind(tmin_obs = tmin_obs_mensual$tmin_mensual) %>% pivot_longer(., -c(mon_year, tmin_obs)) %>% 
  mutate(Model = str_sub(name, 2,3), Model = str_remove(Model, "_"), Model = factor(Model, levels = c(1:16))) %>% 
  mutate(name = str_sub(name, 4, 90), name = str_remove(name, "_")) 


tmin_4.5_forplot %>% filter(., Model %in% selected) %>% 
  ggplot(aes(x = mon_year, ))+
  geom_line(aes(y = value, color = Model))+
  geom_line(aes(y = tmin_obs), color = "blue", linetype = 2, linewidth = 0.9)






