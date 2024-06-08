# Limpiar entorno --------------------------------------------------------------

# rm(list = ls())

# Carga de paquetes ------------------------------------------------------------

pacman::p_load(haven, tidyverse, sjmisc)

# Carga de datos ---------------------------------------------------------------

# data = read_dta("input/data/casen2006.dta")
# saveRDS(data, "input/data/casen2006.rds")

rm(list = ls())
options(scipen = 999)

data = readRDS("input/data/casen2006.rds")

# Exploración ------------------------------------------------------------------

#view(data)
dim(data)
names(data)
head(data)
find_var(data, "ingreso")

frq(data$sexo)
frq(data$e8t)
frq(data$r_15)
frq(data$edad)

# Procesamiento ----------------------------------------------------------------

datos_proc = data%>%
   mutate(sexo = case_when(sexo==1~"Hombre", sexo==2~"Mujer", T~NA_character_),
          edad_tr = case_when(edad>=18 & edad<=39~"Jovenes",
                              edad>=40 & edad<=64~"Adultos",
                              edad>=65~"Adultos mayores",
                              T~NA_character_),
          sexo_edad = case_when(sexo=="Mujer" & edad_tr=="Jovenes"~"Mujer joven",
                                sexo=="Mujer" & edad_tr=="Adultos"~"Mujer adulta",
                                sexo=="Mujer" & edad_tr=="Adultos mayores"~"Mujer mayor",
                                sexo=="Hombre" & edad_tr=="Jovenes"~"Hombre joven",
                                sexo=="Hombre" & edad_tr=="Adultos"~"Hombre adulto",
                                sexo=="Hombre" & edad_tr=="Adultos mayores"~"Hombre mayor",
                                TRUE ~ NA_character_),
          educa = case_when(e8t>=1 & e8t<=4~"Educacion Basica",
                            e8t>=5 & e8t<=8~"Educacion Media",
                            e8t %in% c(9, 11, 13)~"Superior Incompleta",
                            e8t %in% c(10, 12, 14)~"Superior Completa",
                            e8t==15~"Postgrado",
                            T~NA_character_))%>%
  select(sexo, edad, edad_tr, sexo_edad, educa, e8t, region=r_15, ingreso=yoprhaj, 
         estrato, expr, expc, expp)%>%
  filter(region==15 & edad>=18)%>%
  group_by(educa)%>%
  mutate(prom_ing_educ = mean(ingreso, na.rm=T))%>%
  ungroup%>%
  group_by(educa, sexo_edad)%>%
  mutate(prom_ing_sexo = mean(ingreso, na.rm=T))%>%
  ungroup%>%
  mutate(prom_ing_educ = as.integer(prom_ing_educ),
         prom_ing_sexo = as.integer(prom_ing_sexo))

# Revisión de variables --------------------------------------------------------

frq(datos_proc$sexo)
frq(datos_proc$edad)
frq(datos_proc$edad_tr)
frq(datos_proc$sexo_edad)
frq(datos_proc$educa)
frq(datos_proc$region)

# Guardar datos ----------------------------------------------------------------

saveRDS(datos_proc, "output/data/datos_proc.rds")
