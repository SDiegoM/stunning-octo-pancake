library(raster)
library(landscapemetrics)
library(sf)
library(dplyr)
library(tidyr)


# Indicar direccion de los archivo a utilizar 

# Direccion del raster con las categorías
path_raster <- "D:/Code Examples/data/Tiposdebosque.tif"

# Shape que delimita el area a trabajar
path_shape_buffer <- "D:/Code Examples/Buffer equipos/buffer_equipos.shp"


# Cargar archivos 
shape_buffers <- st_read(path_shape_buffer)
raster_cobertura <- raster(path_raster)

# Visualizar
plot(raster_cobertura)
plot(shape_buffers, add = TRUE)

# CORTAR RASTER DE COBERTURA A LA EXTENSION DE CADA BUFFER

# Primero, crear raster stack vacio donde se guardaran todos los raster cortados
r_stack <- raster::stack()

# Loop para cortar cada raster a la extension de cada objeto de la capa shape (cada buffer en este caso) y guardar en el rasterStack 
for (i in 1:nrow(shape_buffers)) {
  raster_crop <- crop(raster_cobertura, extent(shape_buffers))
  raster_mask <-raster::mask(raster_crop, shape_buffers[i, ])
  r_stack <- raster::stack(r_stack, raster_mask)
}

# Visualizar resultado
plot(r_stack)

# Nombrar los raster de acuerdo la informacion de la columna codigo en la capa shape
names(r_stack) <- shape_buffers$Codigo

# CALCULAR LAS METRICAS DE PAISAJE

# Ver lista de metricas disponibles
View(lsm_abbreviations_names)

# Crear un tabla vacia donde se guardaran las metricas
res_ind_paisaje <- data.frame()

# Loop para calcular las metricas seleccionadas* para cada buffer y guardarlo en la tabla vacia creada anteriormente
# *las metricas seleccionadas se indican en el atributo what de la funcion calculate_lsm, estas son indicadas utilizando el nombre de la funcion (ver lsm_abbreviations_names)

for (i in 1:nrow(shape_buffers)) {
  ind_por_sitio <- calculate_lsm(landscape = r_stack[[i]], what = c("lsm_l_te", "lsm_l_condent", "lsm_l_contag", "lsm_l_np", "lsm_l_ent", "lsm_l_area mn", "lsm_l_shape_mn", "lsm_l_frac_mn", "lsm_l_area_mn"), full_name = TRUE) 
  
  ind_por_sitio <- ind_por_sitio %>%
  mutate(Codigo = names(r_stack[[i]]))
  
  res_ind_paisaje <- rbind(res_ind_paisaje, ind_por_sitio)
}

View(res_ind_paisaje)

# Pasar filas a columnas
res_ind_paisaje2 <- res_ind_paisaje %>%
  dplyr::select(metric, value, Codigo) %>% 
  tidyr::pivot_wider(names_from = metric, values_from = value)

View(res_ind_paisaje2)


#Guardar 

path_guardado <- "D:/Licenciatura/TFG/Indices de paisaje"

setwd(path_guardado)
write.csv(res_ind_paisaje2, file = "metricas_de_paisaje2.csv", row.names = TRUE, sep = ';')


