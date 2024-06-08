# Limpiar entorno --------------------------------------------------------------

rm(list = ls())
options(scipen = 999)

# Cargar paquetes --------------------------------------------------------------

pacman::p_load(tidyverse, sjmisc, sjPlot, ggplot2)

# Cargar datos -----------------------------------------------------------------

data = readRDS("output/data/datos_proc.rds")

# Modelos de Regresión Lineal --------------------------------------------------

modelo0 = lm(edad ~ ingreso,  # Simple
             data = data)

summary(modelo0)

sjPlot::tab_model(modelo0, 
                  show.ci=FALSE,  
                  encoding = "UTF-8")

sjPlot::plot_model(modelo0, 
                   show.p = T,
                   show.values =  T,
                   ci.lvl = c(0.95), 
                   title = "Modelo0", 
                   vline.color = "cyan")

modelo1 = lm(e8t ~ ingreso + edad, # Multiple
             data = data)

summary(modelo1)

sjPlot::tab_model(modelo1, 
                  show.ci=FALSE,  
                  encoding = "UTF-8")

sjPlot::plot_model(modelo1, 
                   show.p = T,
                   show.values =  T,
                   ci.lvl = c(0.95), 
                   title = "Modelo1", 
                   vline.color = "cyan")

