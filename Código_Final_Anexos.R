# Libraries ----------------------------------------------------------------
library(tidyverse)
library(GGally)
library(broom)


# Functions ---------------------------------------------------------------

arreglacabecera <- function(df)
{
  # Entra un data.frame df y devuelve el vector names(df) arreglado
  names(df) <- gsub("á","a", names(df))
  names(df) <- gsub("é","e", names(df))
  names(df) <- gsub("í","i", names(df))
  names(df) <- gsub("ó","o", names(df))
  names(df) <- gsub("ú","u", names(df))
  names(df) <- gsub("Á","A", names(df))
  names(df) <- gsub("É","E", names(df))
  names(df) <- gsub("Í","I", names(df))
  names(df) <- gsub("Ó","O", names(df))
  names(df) <- gsub("Ú","U", names(df))
  names(df) <- gsub("ñ","n", names(df))
  names(df) <- gsub("Ñ","N", names(df))
  
  names(df) <- gsub("-","_", names(df))
  
  names(df) <- gsub("#","No", names(df))
  names(df) <- gsub("\\/","", names(df))
  
  names(df) <- gsub("\\.+","_", names(df))
  names(df) <- gsub("\\s+","_", names(df))
  
  names(df)
  
}

portapapeles_salida <- function(df, row_names=FALSE)
{
  df %>% write.table(file="clipboard-165536", 
                     sep="\t", 
                     row.names=row_names, 
                     col.names=TRUE,
                     na = "")
}


# Leer el excel - Resultados - datosanuros_xlsx -------------------------------

cv_inputfilemanual_01 <- "data_in/Mediciones_anuros_V5.xlsx"

datosanuros_leidosxlsx <- readxl::read_xlsx(cv_inputfilemanual_01,
                                            na = "NA",
                                            sheet = "Resultados")

names(datosanuros_leidosxlsx) <- arreglacabecera(datosanuros_leidosxlsx)
glimpse(datosanuros_leidosxlsx)

datosanuros_leidosxlsx %>% filter(is.na(Ano))
datosanuros_leidosxlsx %>% count(Ano) %>% print(n=nrow(.))

datosanuros_xlsx <- datosanuros_leidosxlsx %>%
  mutate(Ano = as.numeric(Ano),  
         LaFecha = as.Date(Ano, origin = "1899-12-30"), 
         Ano = year(LaFecha))  

glimpse(datosanuros_xlsx)


# Gráfico temp-año ------------------------------

datosanuros_temp <- readxl::read_xlsx(cv_inputfilemanual_01,
                                      na = "NA",
                                      sheet = "temperatura")

datosanuros_temp2 <- datosanuros_temp %>% 
  select(Ano = año,
         Temperatura = temperatura)

datosanuros_temp %>% 
  ggplot(aes(x = año, y = temperatura)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Cambio de Temperatura a lo largo del tiempo",
       x = "Año",
       y = "Temperatura (°C)") +
  scale_x_continuous(breaks = seq(1950, 2008, by = 2)) +
  theme_minimal()


# Organizar datos ---------------------------------------------------

datos_gr1 <- datosanuros_xlsx %>% 
  filter(Ano < 2014) %>% 
  group_by(Num_Muestra,
           Ano,
           sexo,
           Altura,
           Temperatura) %>% 
  summarise_at(vars(LRC:LT), ~mean(., na.rm = TRUE)) %>% 
  ungroup()


datos_gr2 <- datos_gr1 %>%
  group_by(Ano,
           Temperatura) %>% 
  summarise_at(vars(LRC:LT), ~mean(., na.rm = TRUE)) %>% 
  ungroup()

# Pivot de datos

datos_gr1 %>% glimpse()

datos_gr1_pivot <- datos_gr1 %>% 
  pivot_longer(cols = LRC:LT,
               names_to = "variable", 
               values_to = "medicion") 

datos_gr1_pivot


datos_gr2_pivot <- datos_gr2 %>% 
  pivot_longer(cols = LRC:LT,
               names_to = "variable", 
               values_to = "medicion") 

datos_gr2_pivot


# Pearson, rasgos funcionales-temp ------------------------------------------------------------

#pearson con datos agrupados por año
datos_gr2_pivot
datos_gr2_pivot %>% count(variable)

datos_gr2_pivot %>% 
  group_by(variable) %>% 
  summarise(tidy(cor.test(Temperatura, medicion, method = "pearson")))

#pearson con todos los datos
datos_gr1_pivot
datos_gr1_pivot %>% count(Ano)
datos_gr1_pivot %>% count(variable)

datos_gr1_pivot %>% 
  group_by(variable) %>% 
  summarise(tidy(cor.test(Temperatura, medicion, method = "pearson")))


# modelo de regresión lineal simple, rasgos funcionales-temp ------------------------------------------------------------

#con todos los datos
datos_gr1_pivot %>% 
  group_by(variable) %>% 
  group_modify(~tidy(lm(medicion ~ Temperatura, data = .x))) %>% 
  ungroup() %>% 
  mutate(term = case_when(term == "Temperatura" ~ "m",
                          term == "(Intercept)" ~ "b")) %>% 
  pivot_wider(names_from = term, 
              names_glue = "{term}_{.value}",
              values_from = 3:6) %>% 
  select(variable, 
         starts_with("m"),
         starts_with("b"))

#con los datos agrupados
modlm_temp <- datos_gr2_pivot %>% 
  group_by(variable) %>% 
  group_modify(~tidy(lm(medicion ~ Temperatura, data = .x))) %>% 
  ungroup() %>% 
  mutate(term = case_when(term == "Temperatura" ~ "m",
                          term == "(Intercept)" ~ "b")) %>% 
  pivot_wider(names_from = term, 
              names_glue = "{term}_{.value}",
              values_from = 3:6) %>% 
  select(variable, 
         starts_with("m"),
         starts_with("b"))
modlm_temp


# Pearson, rasgos funcionales-altura ----------------------------------------

# Si agrupamos hay pocos datos enla mayoría de años, no es pertinente analizarlo así
datos_gr1 %>% group_by(Ano) %>% summarise(alts = n_distinct(Altura))

# Análisis con todos los datos de altura sin agruparse
datos_gr2_altura <- datos_gr1 %>%
  group_by(Altura
  ) %>% 
  summarise_at(vars(LRC:LT), ~mean(., na.rm = TRUE)) %>% 
  ungroup()
datos_gr2_altura


# modelo de regresión lineal simple, rasgos funcionales-altura----------------------

datos_gr1

datos_gr1 %>% count(Altura)
datos_gr1 %>% count(Altura, Ano) %>% print(n=nrow(.))


datos_gr1_pivot %>% 
  group_by(variable) %>% 
  group_modify(~tidy(lm(medicion ~ Altura, data = .x))) %>% 
  ungroup() %>% 
  mutate(term = case_when(term == "Altura" ~ "m",
                          term == "(Intercept)" ~ "b")) %>% 
  pivot_wider(names_from = term, 
              names_glue = "{term}_{.value}",
              values_from = 3:6) %>% 
  select(variable, 
         starts_with("m"),
         starts_with("b"))


# Correlación entre los 6 rasgos funcionales --------------------------------------------------------------

datos_gr1

gr_disp_all <- datos_gr1 %>% 
  select(LRC:LT) %>% 
  ggpairs()
gr_disp_all

gr_disp_sex <- datos_gr1 %>% 
  select(LRC:LT, sexo) %>% 
  ggpairs(columns = c("LRC","LC","ABAC","LA","LF","LT"),
          mapping = aes(colour = sexo, alpha = 3/5))
gr_disp_sex

datos_gr1 %>% 
  select(LRC:LT, sexo) %>% 
  ggpairs(columns = c("LRC","LC","ABAC","LA","LF","LT"),
          mapping = aes(colour = sexo, alpha = 3/5),
          lower = list(continuous = wrap("points"), combo = wrap("dot_no_facet")))


# Arreglos iniciales para las gráficas --------------------------------------------------------

#***** Para Temperatura

# filtrar los años iniciales y finales
df_var_rangoAno <- datos_gr1_pivot %>% 
  group_by(variable) %>% 
  reframe(Ano = range(Ano))

df_var_rangoAno %>% print(n = nrow(.))

# Promedios de medición en los años iniciales y finales
df_extremosAno_prom <- datos_gr1_pivot %>% 
  arrange(Ano) %>% 
  semi_join(df_var_rangoAno) %>% 
  group_by(Ano, variable) %>% 
  summarise(medicion = mean(medicion)) %>% 
  ungroup() %>% 
  mutate(label = str_c(round(medicion, 2), " mm"))

df_extremosAno_prom


# Conteo de ranas por año
df_conteo_individuos <- datos_gr1 %>% 
  group_by(Ano) %>% 
  summarise(conteo = n()) %>% 
  ungroup()

df_conteo_individuos


#***** Para Altura

# Rango de altura
df_var_rangoAltura <- datos_gr1_pivot %>% 
  filter(Altura < 1450) %>% 
  group_by(variable) %>% 
  reframe(Altura = range(Altura))

df_var_rangoAltura %>% print(n = nrow(.))


# Conteo de ranas por altura
df_conteo_individuos_altura <- datos_gr1 %>% 
  filter(Altura < 1450) %>% 
  group_by(Altura) %>% 
  summarise(conteo = n()) %>% 
  ungroup()

df_conteo_individuos_altura


# Promedios de los extremos de altura
df_extremosAltura_prom <- datos_gr1_pivot %>% 
 arrange(Altura) %>% 
 semi_join(df_var_rangoAltura) %>% 
 group_by(Altura, variable) %>% 
 summarise(medicion = mean(medicion)) %>% 
 ungroup() %>% 
 mutate(label = str_c(round(medicion, 2), " mm"))

df_extremosAltura_prom


# Gráficas regresión lineal, Rasgo Funcional-Tiempo -------------------------------------------------------------

#Sólo graficamos con el rasgo funcional LRC
v_iterVar = "LRC"
datos_gr1_pivot
datos_gr1_pivot %>% count(variable)
datos_gr1_pivot %>% count(Ano)

df_var01 <- datos_gr1_pivot %>% 
  filter(variable == v_iterVar)

df_extremosAno_prom_var01 <- df_var01 %>% 
  arrange(Ano) %>% 
  semi_join(df_var_rangoAno, by = join_by(Ano, variable)) %>% 
  group_by(Ano, variable) %>% 
  summarise(medicion = mean(medicion)) %>% 
  ungroup() %>% 
  mutate(label = str_c("E = ", round(medicion, 2), " mm"))

df_extremosAno_prom_var01

y_label <-   max(df_var01$medicion, na.rm = TRUE) + (max(df_var01$medicion, na.rm = TRUE) - min(df_var01$medicion, na.rm = TRUE)) * 0.03
y_conteo <-  max(df_var01$medicion, na.rm = TRUE) + (max(df_var01$medicion, na.rm = TRUE) - min(df_var01$medicion, na.rm = TRUE)) * 0.04

df_var01 %>% 
  filter(variable == "LRC") %>% 
  ggplot(aes(Ano, medicion)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(min(df_var01$Ano), max(df_var01$Ano), by = 4)) +
  geom_text(data = df_conteo_individuos, 
            aes(x = Ano, y = y_conteo, 
                label = conteo),
            inherit.aes = FALSE, color = "black", size = 2) +
  geom_text(data = df_extremosAno_prom_var01, 
            aes(x = Ano, y =  max(df_var01$medicion, na.rm = TRUE) + 2, label = label),
            inherit.aes = FALSE, color = "black", fontface = "bold", size = 2, vjust = 7) +
  labs(title = str_interp("Cambios del ${v_iterVar} a lo largo del tiempo"),
       x = "Año",
       y = "Medición (mm)") +
  theme_minimal()


# Función para graficar los demás Rasgos funcionales --------------------------------------------------------

graficar01 <- function(lavariable, eldf){
  
  df_extremosAno_prom_var01 <- eldf %>% 
    arrange(Ano) %>% 
    semi_join(df_var_rangoAno, by = join_by(Ano, variable)) %>% 
    group_by(Ano, variable) %>% 
    summarise(medicion = mean(medicion)) %>% 
    ungroup() %>% 
    mutate(label = str_c(round(medicion, 2), " mm"))
  
  y_conteo <- max(eldf$medicion) + ((max(eldf$medicion) - min(eldf$medicion)) * 0.15)
  y_label <-  max(eldf$medicion) + ((max(eldf$medicion) - min(eldf$medicion)) * 0.1)
  
  eldf %>% 
    ggplot(aes(Ano, medicion)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    scale_x_continuous(breaks = seq(min(eldf$Ano), max(eldf$Ano), by = 4)) +
    
    geom_text(data = df_conteo_individuos, 
              aes(x = Ano, y = y_conteo, 
                  label = conteo),
              inherit.aes = FALSE, color = "black", size = 3) +
    
    geom_label(data = tibble(Ano = min(eldf$Ano) - 2,
                             conteo = "n ="),
               aes(x = Ano, y = y_conteo,
                   label = conteo),
               inherit.aes = FALSE, color = "black",
               fill = "gray",
               size = 3) +
    
    geom_text(data = df_extremosAno_prom_var01,
              aes(x = Ano, y =  y_label, label = label),
              inherit.aes = FALSE, color = "black",
              size = 2.5) +
    
    geom_label(data = tibble(Ano = min(eldf$Ano) - 2,
                             conteo = "E ="),
               aes(x = Ano, y = y_label,
                   label = conteo),
               inherit.aes = FALSE, color = "black",
               fill = "gray",
               size = 3) +
    
    labs(title = str_interp("Cambios del ${lavariable} a lo largo del tiempo"),
         x = "Año",
         y = "Medición (mm)") +
    theme_minimal()
  
}



df_gr1_variables_plot1 <- datos_gr1_pivot %>% 
  nest_by(variable, .keep = TRUE) %>% 
  mutate(plot = list(graficar01(variable, data)))

df_gr1_variables_plot1 %>% pull(plot)



# Gráfico de doble eje y, cambio del rasgo funcional y la temp. a lo largo del tiempo -------------------------------------------------

graficardobleEje01 <- function(lavariable, eldf){
  
  v_colorTemp <- "#69b3a2"
  v_colorMedi <- rgb(0.2, 0.6, 0.9, 1)
  
  
  x <- eldf %>% pull(Temperatura) %>% range()
  y <- eldf %>% pull(medicion) %>% range()
  m = (y[1] - y[2]) / (x[1] - x[2])
  b = y[1] - m*x[1]
  
  eldf <- eldf %>% mutate(t2 = m * Temperatura + b)
  
  eldf %>% 
    ggplot(aes(x = Ano)) + 
    
    geom_point(aes(y = medicion), 
               color = v_colorMedi,
               position = position_jitter(width = 0.01, height = 0.02),
               size = 4,
               shape = "diamond") + 
    geom_smooth(aes(y = medicion), 
                color = v_colorMedi,
                linetype = "longdash",
                method = "lm",
                se = TRUE,
                alpha = 0.3,
                fill = v_colorMedi) +
    
    geom_point(aes(y = t2), 
               color = v_colorTemp,
               position = position_jitter(width = 0.01, height = 0.02),
               size = 4,
               shape = "circle") + 
    geom_smooth(aes(y = t2), 
                color = v_colorTemp,
                linetype = "longdash",
                method = "lm",
                se = TRUE,
                alpha = 0.3,
                fill = v_colorTemp) +
    
    scale_y_continuous(
      name = str_interp("${lavariable} (mm)"),
      sec.axis = sec_axis(~(.-b)/m,  name="Temperatura (Celsius °)")) +
    labs(x = "Año") +
    theme_minimal() +
    theme(
      axis.title.y = element_text(color = v_colorMedi, size=13),
      axis.title.y.right = element_text(color = v_colorTemp, size=13)
    ) 
}

# grafica con los datos agrupados (gr2)
df_gr2_variables_plotDoble1 <- datos_gr2_pivot %>% 
  nest_by(variable, .keep = TRUE) %>% 
  mutate(plot = list(graficardobleEje01(variable, data)))

df_gr2_variables_plotDoble1 %>% pull(plot)

# gráfica con los datos completos (gr1)
df_gr1_variables_plotDoble1 <- datos_gr1_pivot %>% 
  nest_by(variable, .keep = TRUE) %>% 
  mutate(plot = list(graficardobleEje01(variable, data)))

df_gr1_variables_plotDoble1 %>% pull(plot)


# Quitando 1950
datos_gr2_pivot %>%
  filter(Ano > 1950) %>% 
  nest_by(variable, .keep = TRUE) %>% 
  mutate(plot = list(graficardobleEje01(variable, data))) %>% 
  pull(plot)

datos_gr1_pivot %>% 
  filter(Ano > 1950) %>% 
  nest_by(variable, .keep = TRUE) %>% 
  mutate(plot = list(graficardobleEje01(variable, data))) %>% 
  pull(plot)


# Función para graficar los 6 rasgos funcionales con la Altura --------------------------------------------------------

graficar02 <- function(lavariable, eldf){
  
  df_extremosAno_prom_var01 <- eldf %>% 
    semi_join(df_var_rangoAltura, by = join_by(Altura, variable)) %>% 
    group_by(Altura, variable) %>% 
    summarise(medicion = mean(medicion)) %>% 
    ungroup() %>% 
    mutate(label = str_c(round(medicion, 2), " mm"))
  
  y_conteo <- max(eldf$medicion) + ((max(eldf$medicion) - min(eldf$medicion)) * 0.15)
  y_label <-  max(eldf$medicion) + ((max(eldf$medicion) - min(eldf$medicion)) * 0.1)
  
  eldf %>% 
    ggplot(aes(Altura, medicion)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    scale_x_continuous(breaks = seq(min(eldf$Altura), max(eldf$Altura), by = 20)) +
    
    geom_text(data = df_conteo_individuos_altura, 
              aes(x = Altura, y = y_conteo, 
                  label = conteo),
              inherit.aes = FALSE, color = "black", size = 3) +
    
    geom_label(data = tibble(Altura = min(eldf$Altura) - 20,
                             conteo = "n ="),
               aes(x = Altura, y = y_conteo,
                   label = conteo),
               inherit.aes = FALSE, color = "black",
               fill = "gray",
               size = 3) +
    
    geom_text(data = df_extremosAno_prom_var01,
              aes(x = Altura, y =  y_label, label = label),
              inherit.aes = FALSE, color = "black",
              size = 2.5) +
    
    geom_label(data = tibble(Altura = min(eldf$Altura) - 20,
                             conteo = "E ="),
               aes(x = Altura, y = y_label,
                   label = conteo),
               inherit.aes = FALSE, color = "black",
               fill = "gray",
               size = 3) +
    
    labs(title = str_interp("Cambios del ${lavariable} con respecto a la altura"),
         x = "Altura",
         y = "Medición (mm)") +
    theme_minimal()
  
}


df_gr1_variables_plot2 <- datos_gr1_pivot %>% 
  filter(Altura < 1450) %>% 
  nest_by(variable, .keep = TRUE) %>% 
  mutate(plot = list(graficar02(variable, data)))

df_gr1_variables_plot2 %>% pull(plot)


# Regresiones lineales,rasgos funcionales-Temp separados por sexo--------------------------------------------------------

#LRC
datos_gr1 %>% 
  filter(Ano<2014) %>% 
  pivot_longer(cols = LRC, names_to = "variable", values_to = "medicion") %>% 
  ggplot(aes(Ano, medicion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(variable~sexo,
             scales = "free") +
  labs(title = "Cambios del LRC a lo largo del tiempo",
       x = "Año",
       y = "medición (mm)")

#LC
datos_gr1 %>% 
  filter(Ano<2014) %>% 
  pivot_longer(cols = LC, names_to = "variable", values_to = "medicion") %>% 
  ggplot(aes(Ano, medicion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(variable~sexo,
             scales = "free") +
  labs(title = "Cambios del LC a lo largo del tiempo",
       x = "Año",
       y = "medición (mm)")

#AB/AC
datos_gr1 %>% 
  filter(Ano<2014) %>% 
  pivot_longer(cols = ABAC, names_to = "variable", values_to = "medicion") %>% 
  ggplot(aes(Ano, medicion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(variable~sexo,
             scales = "free") +
  labs(title = "Cambios del AB/AC a lo largo del tiempo",
       x = "Año",
       y = "medición (mm)")

#LA
datos_gr1 %>% 
  filter(Ano<2014) %>% 
  pivot_longer(cols = LA, names_to = "variable", values_to = "medicion") %>% 
  ggplot(aes(Ano, medicion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(variable~sexo,
             scales = "free") +
  labs(title = "Cambios del LA a lo largo del tiempo",
       x = "Año",
       y = "medición (mm)")

#LF
datos_gr1 %>% 
  filter(Ano<2014) %>% 
  pivot_longer(cols = LF, names_to = "variable", values_to = "medicion") %>% 
  ggplot(aes(Ano, medicion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(variable~sexo,
             scales = "free") +
  labs(title = "Cambios del LF a lo largo del tiempo",
       x = "Año",
       y = "medición (mm)")

#LT
datos_gr1 %>% 
  filter(Ano<2014) %>% 
  pivot_longer(cols = LT, names_to = "variable", values_to = "medicion") %>% 
  ggplot(aes(Ano, medicion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(variable~sexo,
             scales = "free") +
  labs(title = "Cambios del LT a lo largo del tiempo",
       x = "Año",
       y = "medición (mm)")


# Regresiones lineales,rasgos funcionales-Altura separados por sexo--------------------------------------------------------

#LRC
datos_gr1 %>% 
  filter(Altura<1450) %>% 
  pivot_longer(cols = LRC, names_to = "variable", values_to = "medicion") %>% 
  ggplot(aes(Altura, medicion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(variable~sexo,
             scales = "free") +
  labs(title = "Cambios del LRC en función de la altitud",
       x = "Año",
       y = "medición (mm)")

#LC
datos_gr1 %>% 
  filter(Altura<1450) %>% 
  pivot_longer(cols = LC, names_to = "variable", values_to = "medicion") %>% 
  ggplot(aes(Altura, medicion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(variable~sexo,
             scales = "free") +
  labs(title = "Cambios del LC en función de la altitud",
       x = "Año",
       y = "medición (mm)")

#AB/AC
datos_gr1 %>% 
  filter(Altura<1450) %>% 
  pivot_longer(cols = ABAC, names_to = "variable", values_to = "medicion") %>% 
  ggplot(aes(Altura, medicion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(variable~sexo,
             scales = "free") +
  labs(title = "Cambios del AB/AC en función de la altitud",
       x = "Año",
       y = "medición (mm)")

#LA
datos_gr1 %>% 
  filter(Altura<1450) %>% 
  pivot_longer(cols = LA, names_to = "variable", values_to = "medicion") %>% 
  ggplot(aes(Altura, medicion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(variable~sexo,
             scales = "free") +
  labs(title = "Cambios del LA en función de la altitud",
       x = "Año",
       y = "medición (mm)")

#LF
datos_gr1 %>% 
  filter(Altura<1450) %>% 
  pivot_longer(cols = LF, names_to = "variable", values_to = "medicion") %>% 
  ggplot(aes(Altura, medicion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(variable~sexo,
             scales = "free") +
  labs(title = "Cambios del LF en función de la altitud",
       x = "Año",
       y = "medición (mm)")

#LT
datos_gr1 %>% 
  filter(Altura<1450) %>% 
  pivot_longer(cols = LT, names_to = "variable", values_to = "medicion") %>% 
  ggplot(aes(Altura, medicion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(variable~sexo,
             scales = "free") +
  labs(title = "Cambios del LT en función de la altitud",
       x = "Año",
       y = "medición (mm)")


# PCA-------------------------------------------------------------------------

# Librerías necesarias

library(stats)
library(FactoMineR)
library(factoextra)

# PCA general

datos_pca <- datos_gr1 %>%
  filter(Ano < 2014) %>%   
  select(Ano, Altura, LRC:LT)             

pca_anuros <- prcomp(datos_pca, scale. = TRUE)

# Resultados del PCA
summary(pca_anuros)               # Resumen del PCA
head(pca_anuros$rotation)[, 1:5]  # Loadings de los primeros 5 componentes
head(pca_anuros$x)[, 1:5]         # Scores de los primeros 5 componentes

###### Visualización del PCA

# Gráfico de individuos (observaciones en los componentes principales)
fviz_pca_ind(pca_anuros, 
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Gráfico de variables (contribución de cada rasgo funcional)
fviz_pca_var(pca_anuros,
             col.var = "contrib",  # Color según la contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)         # Evitar solapamiento de etiquetas

# Screeplot: proporción de varianza explicada por cada componente
fviz_screeplot(pca_anuros, addlabels = TRUE, barfill = "#00AFBB", barcolor = "#00AFBB")

# Contribución de las variables a los componentes principales
fviz_contrib(pca_anuros, choice = "var", axes = 1:2)


# PCA en machos

datos_pca_machos <- datos_gr1 %>%
  filter(sexo == "Macho", Ano < 2014) %>%  
  select(LRC:LT)

pca_machos <- prcomp(datos_pca_machos, scale. = TRUE)

# Resultados y visualización para machos
summary(pca_machos)
head(pca_anuros$rotation)[, 1:5]  # Loadings de los primeros 5 componentes
head(pca_anuros$x)[, 1:5]         # Scores de los primeros 5 componentes



fviz_pca_ind(pca_machos, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_var(pca_machos, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_screeplot(pca_machos, addlabels = TRUE, barfill = "#00AFBB", barcolor = "#00AFBB")
fviz_contrib(pca_machos, choice = "var", axes = 1:2)


# PCA en hembras

datos_pca_hembras <- datos_gr1 %>%
  filter(sexo == "Hembra", Ano < 2014) %>%  
  select(Ano, Altura, LRC:LT)

pca_hembras <- prcomp(datos_pca_hembras, scale. = TRUE)

# Resultados y visualización para machos
summary(pca_hembras)
head(pca_anuros$rotation)[, 1:5]  # Loadings de los primeros 5 componentes
head(pca_anuros$x)[, 1:5]         # Scores de los primeros 5 componentes



fviz_pca_ind(pca_hembras, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_var(pca_hembras, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_screeplot(pca_hembras, addlabels = TRUE, barfill = "#00AFBB", barcolor = "#00AFBB")
fviz_contrib(pca_hembras, choice = "var", axes = 1:2)


