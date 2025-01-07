# Sprint4-LAB-3
# Cargar las librerías necesarias
library(dplyr)
library(tidyr)

# Cargar el dataset mtcars
data(mtcars)
df <- as.data.frame(mtcars)

# Paso 2: Selección de columnas y filtrado de filas
df_selected <- df %>%
  select(mpg, cyl, hp, gear) %>%
  filter(cyl > 4)

# Verificar el dataframe después del filtrado
print("DataFrame después de la selección y filtrado:")
print(df_selected)

# Paso 3: Ordenación y renombrado de columnas
df_sorted <- df_selected %>%
  arrange(desc(hp)) %>%
  rename(consumo = mpg, potencia = hp)

# Verificar el dataframe después de ordenar y renombrar
print("DataFrame después de ordenar y renombrar:")
print(df_sorted)

# Paso 4: Creación de nuevas columnas y agregación de datos
df_mutated <- df_sorted %>%
  mutate(eficiencia = consumo / potencia)

# Agrupación por cyl y cálculo de consumo medio y potencia máxima
df_grouped <- df_mutated %>%
  group_by(cyl) %>%
  summarise(consumo_medio = mean(consumo), potencia_max = max(potencia))

# Verificar el resultado de la agregación
print("DataFrame con agregaciones (consumo medio y potencia máxima por cilindro):")
print(df_grouped)

# Paso 5: Creación del segundo dataframe y unión de dataframes
# Crear el dataframe con gear y tipo_transmision
df_transmision <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

# Realizar un left join
df_joined <- left_join(df_mutated, df_transmision, by = "gear")

# Verificar el dataframe después del left join
print("DataFrame después del left join con tipo_transmision:")
print(df_joined)

# Paso 6: Transformación de formatos
# Convertir el dataframe a formato largo utilizando pivot_longer()
df_long <- df_joined %>%
  pivot_longer(cols = c(consumo, potencia, eficiencia), names_to = "medida", values_to = "valor")

# Verificar el dataframe en formato largo
print("DataFrame en formato largo:")
print(df_long)

# Identificar duplicados antes de la transformación a formato ancho
df_long_grouped <- df_long %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(valor_promedio = mean(valor))

# Verificar el dataframe después de agrupar por las columnas clave
print("DataFrame después de manejar duplicados con agregación (valor_promedio):")
print(df_long_grouped)

# Convertir el dataframe de nuevo a formato ancho utilizando pivot_wider()
df_wide <- df_long_grouped %>%
  pivot_wider(names_from = medida, values_from = valor_promedio)

# Verificar el dataframe en formato ancho
print("DataFrame en formato ancho:")
print(df_wide)
