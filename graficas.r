install.packages("rpart")
install.packages("rpart.plot")
install.packages("ggplot2")
install.packages("tidyverse")


library(ggplot2)
library(rpart)
library(rpart.plot)
library(tidyverse)


# Crear un conjunto de datos de ejemplo
productos <- data.frame(
  Tipo = c("A", "B", "C", "D", "E", "F"),
  Seguidores = c(100, 200, 150, 300, 250, 180),
  Otras_Caracteristicas = c(10, 20, 15, 30, 25, 18)
)

# Ajustar el modelo de árbol de decisión
modelo_arbol <- rpart(Tipo ~ Seguidores + Otras_Caracteristicas, data = productos)

# Visualizar el árbol de decisión
rpart.plot(modelo_arbol, box.palette = "Blues", shadow.col = "gray")

#### ----------------------------Tipos de graficos: 


install.packages("ggplot2")
library(ggplot2)

# Crear un conjunto de datos de ejemplo
datos <- data.frame(
  Categorias = c("A", "B", "C", "D", "E"),
  Valores = c(10, 20, 15, 25, 30)
)

# Crear el gráfico de barras verticales
ggplot(datos, aes(x = Categorias, y = Valores)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Gráfico de Barras Verticales",
       x = "Categorías",
       y = "Valores")

#### ---------------------------- 

# Crear un conjunto de datos de ejemplo con anchos variables
datos <- data.frame(
  Categorias = c("A", "B", "C", "D", "E"),
  Valores = c(10, 20, 15, 25, 30),
  Anchuras = c(0.5, 1, 0.7, 1.2, 0.8) # Anchuras de las barras
)

# Crear el gráfico de barras verticales con anchos variables
ggplot(datos, aes(x = Categorias, y = Valores, width = Anchuras)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Gráfico de Barras Verticales con Anchos Variables",
       x = "Categorías",
       y = "Valores")


### -----------------------------
# Crear un conjunto de datos de ejemplo
datos <- data.frame(
  Categorias = c("A", "B", "C", "D", "E"),
  Valores = c(10, 20, 15, 25, 30)
)

# Crear el gráfico de barras horizontales
ggplot(datos, aes(x = Valores, y = Categorias)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Gráfico de Barras Horizontales",
       x = "Valores",
       y = "Categorías")


### -----------------------------

install.packages("ggplot2")
install.packages("tidyr")
library(ggplot2)
library(tidyr)

# Crear un conjunto de datos de ejemplo para la pirámide de población
datos <- data.frame(
  Edad = c("0-4", "5-9", "10-14", "15-19", "20-24"),
  Hombres = c(100, 120, 110, 90, 80),
  Mujeres = c(95, 110, 105, 85, 75)
)

# Convertir los datos a formato largo
datos_largos <- pivot_longer(datos, cols = c("Hombres", "Mujeres"), 
                             names_to = "Sexo",
                             values_to = "Población")

# Crear la pirámide de población
ggplot(datos_largos, aes(x = Edad, y = Población, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Pirámide de Población",
       x = "Edad",
       y = "Número de Personas",
       fill = "Sexo")

### -----------------------------
# Crear un conjunto de datos de ejemplo
datos <- data.frame(
  Categoria = c("A", "B", "C", "D"),
  Valor_1 = c(10, 15, 20, 25),
  Valor_2 = c(12, 18, 22, 28)
)

# Convertir los datos a formato largo
library(tidyr)
datos_largos <- pivot_longer(datos, cols = c(Valor_1, Valor_2), 
                             names_to = "Variable", values_to = "Valor")

# Crear el gráfico de barras agrupadas
ggplot(datos_largos, aes(x = Categoria, y = Valor, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gráfico de Barras Agrupadas",
       x = "Categoría",
       y = "Valor",
       fill = "Variable")



### -----------------------------
### -----------------------------
### -----------------------------
### -----------------------------
### -----------------------------
### -----------------------------
### -----------------------------
### -----------------------------








### -----------------------------
install.packages("ggplot2")
library(ggplot2)

# Crear un conjunto de datos de ejemplo
datos <- data.frame(
  Categoria = c("A", "B", "C"),
  Valor_1 = c(20, 30, 40),
  Valor_2 = c(30, 40, 30),
  Valor_3 = c(50, 30, 30)
)

# Convertir los datos a formato largo
library(tidyr)
datos_largos <- pivot_longer(datos, cols = c(Valor_1, Valor_2, Valor_3), 
                             names_to = "Variable", values_to = "Valor")

# Calcular los porcentajes acumulados
datos_largos <- transform(datos_largos, 
                          Porcentaje = Valor / ave(Valor, Categoria, FUN = sum) * 100)

# Crear el gráfico de áreas acumuladas al 100%
ggplot(datos_largos, aes(x = Categoria, y = Porcentaje, fill = Variable)) +
  geom_area(position = "fill") +
  labs(title = "Gráfico de Áreas Acumuladas al 100%",
       x = "Categoría",
       y = "Porcentaje",
       fill = "Variable")


### -----------------------------
# Crear un conjunto de datos de ejemplo
datos <- data.frame(
  Categoria = c("A", "B", "C"),
  Valor = c(30, 40, 30)
)

# Crear el gráfico de sectores
ggplot(datos, aes(x = "", y = Valor, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Gráfico de Sectores",
       fill = "Categoría") +
  theme_void()


### -----------------------------
# Crear el gráfico de barras en ejes polares
ggplot(datos, aes(x = Categoria, y = Valor, fill = Categoria)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Gráfico de Barras en Ejes Polares",
       x = "Categoría",
       y = "Valor",
       fill = "Categoría")


### -----------------------------
# Instalar y cargar la biblioteca treemap si aún no está instalada
install.packages("treemap")
library(treemap)

# Crear un conjunto de datos de ejemplo
datos <- data.frame(
  Categoria = c("A", "B", "C"),
  Valor = c(30, 40, 30)
)

# Crear el treemap
treemap(datos, index = "Categoria", vSize = "Valor", draw = TRUE)


### -----------------------------

# Instalar y cargar la biblioteca ggplot2 si aún no está instalada
install.packages("ggplot2")
library(ggplot2)

# Crear un conjunto de datos de ejemplo
datos <- data.frame(
  Categoria = c("A", "B", "C"),
  Valor_1 = c(20, 30, 40),
  Valor_2 = c(30, 40, 30)
)

# Convertir los datos a formato largo
library(tidyr)
datos_largos <- pivot_longer(datos, cols = c(Valor_1, Valor_2),
                             names_to = "Variable", values_to = "Valor")

# Crear el gráfico Mekko o Marimekko
ggplot(datos_largos, aes(x = Categoria, y = Valor, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico Mekko o Marimekko",
       x = "Categoría",
       y = "Valor",
       fill = "Variable")

### -----------------------------
