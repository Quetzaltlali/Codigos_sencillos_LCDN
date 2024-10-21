## González Pinefa Alejandra Quetzal
## Grupo 601
## Visualización de datos 
install.packages("tidyverse", "dslabs")

library(tidyverse)
library(dslabs)
library(gapminder)

gapminder <- as_tibble(gapminder)


# ¿qué países creen que tuvieron las tasas de mortalidad infantil más altas en 2015?
# ¿Qué pares creen que son más similares?
# también es común suponer que los países considerados como parte del mundo en desarrollo: Pakistán, Vietnam, Tailandia y Sudáfrica, tienen tasas de mortalidad igualmente altas

# Turquía tiene la mayor tasa de mortalidad infantil.
gapminder |> 
  filter(year == 2015 & country %in% c("Sri Lanka","Turkey")) |> 
  select(country, infant_mortality)

# primer gráfico es un diagrama de dispersión de la esperanza de vida versus las tasas de fertilidad (número promedio de hijos por mujer). Comenzamos hace unos 50 años, cuando quizás esta visión se consolidó por primera vez en nuestras mentes.

filter(gapminder, year == 1962) |> 
  ggplot(aes(fertility, life_expectancy)) + 
  geom_point()

#La mayoría de puntos se dividen en dos categorías distintas:

# 1.-Esperanza de vida alrededor de 70 años y 3 o menos hijos por familia.
# 2.-Esperanza de vida inferior a 65 años y más de 5 niños por familia.
#Para confirmar que estos países son de las regiones que esperamos,
#podemos usar un color para representar un continente.

filter(gapminder, year == 1962) |> 
  ggplot( aes(fertility, life_expectancy, color = continent)) + 
  geom_point()
#En 1962, la visión del “Oeste versus el mundo en desarrollo” se basaba
#en cierta realidad. ¿sigue siendo así 50 años después?

#####    Separar en facetas #######

# Para separar en facetas, se añade una capa con la función facet_grid,
# que automáticamente separa los gráficos. Esta función les permite
# separar hasta dos variables en facetas usando columnas para representar
# una variable y filas para representar otra. La función espera que
# las variables de fila y de columna estén separadas por un ~. :

filter(gapminder, year %in% c(1962, 2012)) |> 
  ggplot(aes(fertility, life_expectancy, col = continent)) + 
  geom_point() + 
  facet_grid(continent ~ year)

# Este gráfico muestra claramente que la mayoría de los países se han 
# mudado del conjunto mundo en desarrollo al conjunto mundo occidental.
# En 2012, la visión del mundo occidental versus el mundo en desarrollo 
# ya no tiene sentido. Esto es particularmente evidente cuando se compara 
# Europa con Asia, este último ahora con varios países que han realizado 
# grandes mejoras.

filter(gapminder, year %in% c(1962, 2012)) |> 
  ggplot(aes(fertility, life_expectancy, col = continent)) + 
  geom_point() + 
  facet_grid(. ~ year)

# ### facet_wrap ###

# Queremos usar múltiples filas y columnas. La función facet_wrap nos 
# permite hacer esto automáticamente acomodando la serie de gráficos 
# para que cada imagen tenga dimensiones visibles:

# Este gráfico muestra claramente cómo la mayoría de los países asiáticos 
# han mejorado a un ritmo mucho más rápido que los europeos.

years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder |> 
  filter(year %in% years & continent %in% continents) |> 
  ggplot( aes(fertility, life_expectancy, col = continent)) + 
  geom_point() + 
  facet_wrap(~year)

# Escalas fijas para mejores comparaciones ###
#Los gráficos de series de tiempo, tienen tiempo en el eje-x y un resultado o medida de interés en el eje-y. Por ejemplo, Gráfico de la tendencia de las tasas de fertilidad de Estados Unidos:
gapminder |> 
  filter(country == "United States") |> 
  ggplot(aes(year, fertility)) + 
  geom_point()

# Observamos que la tendencia no es lineal en absoluto, sino que durante los años sesenta y setenta se produce una fuerte caída por debajo de 2. Entonces la tendencia vuelve a 2 y se estabiliza durante los años noventa.
# como vemos arriba, creamos una curva que une los puntos 
# con líneas, para transmitir que estos datos provienen de 
# una sola serie, aquí un país. Para hacer esto, usamos la 
# función geom_line en vez de geom_point.

gapminder |> 
  filter(country == "United States") |> 
  ggplot(aes(year, fertility)) + 
  geom_line()

# Esto es particularmente útil cuando comparamos dos países.
# Si creamos un subconjunto de los datos para incluir dos países, 
# uno de Europa y uno de Asia, entonces adaptamos el código 
# anterior:

countries <- c("South Korea", "Germany")

gapminder |> filter(country %in% countries & !is.na(fertility)) |> 
  ggplot(aes(year, fertility, col = country)) + 
  geom_line()

# El gráfico muestra claramente cómo la tasa de fertilidad de 
# Corea del Sur cayó drásticamente durante los años sesenta y 
# setenta, y en 1990 tuvo una tasa similar a la de Alemania.

# Etiquetas en lugar de leyendas ###
# Para los gráficos de tendencias, recomendamos etiquetar las líneas en lugar de usar leyendas, ya que el espectador puede ver rápidamente qué línea representa qué país
# Demostramos cómo hacer esto usando los datos de esperanza de vida. Definimos una tabla de datos con las ubicaciones de las etiquetas y luego usamos una segunda asignación solo para estas etiquetas:
labels <- data.frame(country = countries, x = c(1975,1965), y = c(60,72))
gapminder |> 
  filter(country %in% countries) |> 
  ggplot(aes(year, life_expectancy, col = country)) + 
  geom_line() + 
  geom_text(data = labels, aes(x, y, label = country), size = 5) + 
  theme(legend.position = "none")

# La tabla de datos gapminder incluye una columna con el producto interno bruto de los países (GDP por sus siglas en inglés). El GDP mide el valor de mercado de los bienes y servicios producidos por un país en un año.
gapminder <- gapminder |> mutate(dollars_per_day = gdp/population/365)

# Transformación logarítmica ###
# Abajo tenemos un histograma de ingresos diarios desde 1970:
past_year <- 1970
gapminder |> 
  filter(year == past_year & !is.na(gdp)) |> 
  ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1, color = "black")

# Se utilizo el argumento color = "black" para dibujar un límite y distinguir claramente los compartimientos.

# Cómo visualizar distribuciones multimodales
# Cómo comparar múltiples distribuciones con diagramas de caja y gráficos ridge

# De acuerdo con el histograma, los valores de distribución del ingreso de 1970 muestran una dicotomía. Sin embargo, el histograma no nos muestra si los dos grupos de países están en el oeste o forman parte del mundo en desarrollo.
# Examinando rápidamente los datos por región. Reordenamos las regiones por la mediana y usamos una escala logarítmica.

gapminder |> 
  filter(year == past_year & !is.na(gdp)) |> 
  mutate(region = reorder(region, dollars_per_day, FUN = median)) |> 
  ggplot(aes(dollars_per_day, region)) + 
  geom_point() + 
  scale_x_continuous(trans = "log2")

# Ya podemos ver que efectivamente existe una dicotomía “el Oeste versus el Resto”: hay dos grupos claros, con el grupo rico compuesto por Norteamérica, Europa del Norte y Occidental, Nueva Zelanda y Australia. Definimos grupos basados en esta observación:
gapminder <- gapminder |> 
  mutate(group = case_when(
    region %in% c("Western Europe", "Northern Europe","Southern Europe", "Northern America", 
                  "Australia and New Zealand") ~ "West",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan",
    TRUE ~ "Others"))

# Convertimos esta variable group en un factor para controlar el orden de los niveles:
gapminder <- gapminder |> 
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan", "West")))

# Diagramas de caja
# El anterior análisis exploratorio de datos reveló dos características sobre la distribución de ingreso promedio en 1970. Usando un histograma, encontramos una distribución bimodal con los modos relacionados con los países pobres y ricos. Ahora queremos comparar la distribución entre estos cinco grupos para confirmar la dicotomía “el Oeste versus el Resto”.
# Apilando diagramas de caja uno al lado del otro.
# Tengan en cuenta que añadimos la capa 
# theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
#para que las etiquetas de grupo sean verticales, ya que no encajan si las mostramos horizontalmente, y para quitar la etiqueta del eje a fin de hacer espacio.

p <- gapminder |> 
  filter(year == past_year & !is.na(gdp)) |> 
  ggplot(aes(group, dollars_per_day)) + 
  geom_boxplot() + 
  scale_y_continuous(trans = "log2") + 
  xlab("") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

# Los diagramas de caja tienen la limitación de que al resumir los datos en cinco números, se pueden perder características importantes de los datos. Una forma de evitar esto es mostrando los datos.
p + geom_point(alpha = 0.5)

# Gráficos ridge
# Mostrar cada punto individual no siempre revela características importantes de la distribución. Aunque no es el caso aquí, cuando el número de puntos de datos es demasiado grande acabamos sobregraficando y mostrar los datos puede ser contraproducente. Los diagramas de caja ayudan con esto al proveer un resumen de cinco números, pero esto también tiene limitaciones.
# Los diagramas de caja no revelan distribuciones bimodales. Para ver esto, miren los dos gráficos abajo que resumen el mismo set de datos:

# los dos gráficos abajo que resumen el mismo set de datos:

# En los casos en que nos preocupa que el resumen del diagrama de caja sea demasiado 
# simplista, podemos mostrar densidades suaves o histogramas apilados utilizando 
# gráficos ridge

# El paquete ggridges incluye una función conveniente para hacer esto.
# Abajo vemos los datos de ingresos, que mostramos arriba con diagramas de caja,
# pero ahora visualizados con un gráfico ridge.
install.packages("ggridges")
library(ggridges)

p <- gapminder |> 
  filter(year == past_year & !is.na(dollars_per_day)) |> 
  ggplot(aes(dollars_per_day, group)) + 
  scale_x_continuous(trans = "log2")
p + geom_density_ridges()

# Tengan en cuenta que tenemos que invertir el x e y que se usaron para el diagrama 
# de caja. Un parametro útil de geom_density_ridges es scale, que les permite determinar 
# cuánto superponer; por ejemplo, scale = 1 significa que no hay superposición.
# Valores mayores que 1 resultan en mayor superposición.

# Si el número de puntos de datos es lo suficientemente pequeño, podemos añadirlos 
# al gráfico ridge usando el siguiente código:

# al gráfico ridge usando el siguiente código:

p + geom_density_ridges(jittered_points = TRUE)

# Por defecto, la altura de los puntos está jittered y no se debe interpretar de 
# ninguna manera. Para mostrar puntos de datos, pero sin usar jitter, podemos 
# usar el siguiente código para agregar lo que se conoce como una 
# representación rug de los datos.

p + geom_density_ridges(jittered_points = TRUE, 
                        position = position_points_jitter(height = 0), 
                        point_shape = '|', point_size = 3, 
                        point_alpha = 1, alpha = 0.7)

# distribuciones de ingresos de 1970 versus 2010

# La exploración de datos muestra claramente que en 1970 hubo una dicotomía 
# del “Oeste versus el Resto”. ¿Pero persiste esta dicotomía? Vamos a usar 
# facet_grid para ver cómo han cambiado las distribuciones. 
# Para comenzar, nos enfocamos en dos grupos: el Oeste y el Resto. Hacemos cuatro histogramas.

past_year <- 1970
present_year <- 2010
years <- c(past_year, present_year)
gapminder |> 
  filter(year %in% years & !is.na(gdp)) |> 
  mutate(west = ifelse(group == "West", "West", "Developing")) |> 
  ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1, color = "black") + 
  scale_x_continuous(trans = "log2") + 
  facet_grid(year ~ west)

# Rehacemos los gráficos utilizando solo países con datos disponibles 
# para ambos años. En la parte sobre wrangling de datos de este libro, 
# aprenderemos a usar herramientas de tidyverse que nos permitirá escribir 
# código eficiente para esto, pero aquí podemos usar un código sencillo 
# usando la función intersect:

country_list_1 <- gapminder |> 
  filter(year == past_year & !is.na(dollars_per_day)) |> 
  pull(country)

country_list_2 <- gapminder |> 
  filter(year == present_year & !is.na(dollars_per_day)) |> 
  pull(country)
country_list <- intersect(country_list_1, country_list_2)

# Estos 108 constituyen 86% de la población mundial, por 
# lo que este subconjunto debe ser representativo.

# Ahora vemos que los países ricos se han vuelto un poco más ricos, 
# pero en términos de porcentaje, los países pobres parecen haber 
# mejorado más. En particular, vemos que la proporción de países en 
# desarrollo que ganan más de $16 por día aumentó sustancialmente.

# Para ver qué regiones específicas mejoraron más, podemos rehacer los 
# diagramas de caja que hicimos anteriormente, pero ahora añadimos el 
# año 2010 y luego usamos facet para comparar los dos años.

gapminder |> 
  filter(year %in% years & country %in% country_list) |> 
  ggplot(aes(group, dollars_per_day)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(trans = "log2") + 
  xlab("") + 
  facet_grid(. ~ year)
# Vamos a rehacer el gráfico, pero solo para este subconjunto simplemente 
# agregando country %in% country_list a la función filter:
# Entonces, en lugar de separar en facetas, mantenemos los datos de 
# cada año juntos y pedimos colorearlos (o rellenarlos) según el año. 
# Como el año es un número, lo convertimos en un factor ya que ggplot2 
# asigna automáticamente un color a cada categoría de un factor.
# Recuerden que tenemos que convertir la columna year de numérica a factor.

gapminder |> 
  filter(year %in% years & country %in% country_list) |> 
  mutate(year = factor(year)) |> 
  ggplot(aes(group, dollars_per_day, fill = year)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(trans = "log2") + 
  xlab("")

# Finalmente, señalamos que si lo más que nos interesa es comparar 
# los valores de antes y después, podría tener más sentido graficar 
# los aumentos porcentuales. Todavía no estamos listos para aprender 
# a codificar esto, pero así es como se vería el gráfico:

# Empecemos observando que los gráficos de densidad para la distribución 
# del ingreso en 1970 y 2010 transmiten el mensaje de que la brecha se está cerrando:
# Empecemos observando que los gráficos de densidad para la distribución 
# del ingreso en 1970 y 2010 transmiten el mensaje de que la brecha se está cerrando:

gapminder |> 
  filter(year %in% years & country %in% country_list) |> 
  ggplot(aes(dollars_per_day)) + 
  geom_density(fill = "grey") + 
  scale_x_continuous(trans = "log2") + 
  facet_grid(. ~ year)

# En el gráfico de 1970, vemos dos modas claras: países pobres y ricos. 
# En 2010, parece que algunos de los países pobres se han desplazado hacia 
# la derecha, cerrando la brecha.

# países pobres se hicieron más ricos, en lugar de que algunos países ricos 
# se hicieran más pobres. Para hacer esto, podemos asignar un color a los grupos 
# que identificamos durante la exploración de datos.

# Pero cuando superponemos dos densidades, el comportamiento por defecto es 
# que el área representada por cada distribución sume a 1, independientemente 
# del tamaño de cada grupo:
gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(group = ifelse(group == "West", "West", "Developing")) |>
  ggplot(aes(dollars_per_day, fill = group)) + 
  scale_x_continuous(trans = "log2") + 
  geom_density(alpha = 0.2) + 
  facet_grid(year ~ .)


# Cómo obtener acceso a variables calculadas

# Para que las áreas de estas densidades sean proporcionales al tamaño de 
# los grupos, simplemente multiplicamos los valores del eje-y por el tamaño del grupo.

# En ggplot2, obtenemos acceso a estas variables rodeando el nombre con dos puntos. 
# Por lo tanto, utilizaremos el siguiente mapeo:

aes(x = dollars_per_day, y = ..count..)

# Ahora podemos crear el diagrama deseado simplemente cambiando el mapeo en el 
# fragmento del código anterior. También ampliaremos los límites del eje-x.

p <- gapminder |> 
  filter(year %in% years & country %in% country_list) |> 
  mutate(group = ifelse(group == "West", "West", "Developing")) |> 
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) + 
  scale_x_continuous(trans = "log2", limit = c(0.125, 300))

p + geom_density(alpha = 0.2) + 
  facet_grid(year ~ .)

# Si queremos que las densidades sean más suaves, usamos el argumento bw para 
# que se use el mismo parámetro de suavizado en cada densidad. Seleccionamos 0.75 
# después de probar varios valores.

p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)

# Este gráfico ahora muestra lo que está sucediendo muy claramente. 
# La distribución del mundo en desarrollo está cambiando. Aparece una 
# tercera moda formada por los países que más redujeron la brecha.

# Para visualizar si alguno de los grupos definidos anteriormente son la causa 
# principal de estos cambios, rápidamente podemos hacer un gráfico ridge:

gapminder |> 
  filter(year %in% years & !is.na(dollars_per_day)) |> 
  ggplot(aes(dollars_per_day, group)) + 
  scale_x_continuous(trans = "log2") + 
  geom_density_ridges(adjust = 1.5) + 
  facet_grid(. ~ year)

# Otra forma de lograr esto es apilando las densidades una encima de otra:

gapminder |> 
  filter(year %in% years & country %in% country_list) |> 
  group_by(year) |> 
  mutate(weight = population / sum(population) * 2) |> 
  ungroup() |> 
  ggplot(aes(dollars_per_day, fill = group)) + 
  scale_x_continuous(trans = "log2", limit = c(0.125, 300)) + 
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + 
  facet_grid(year ~ .)

# Creación de diagramas de Venn en R
install.packages(eulerr)
library(eulerr)

# para crear el diagrama de Venn necesitamos un conjunto de datos en el 
# que indicamos el tamaño de los conjuntos y las relaciones. Lo que se hace 
# con listas de pares de clave-valor, en los que la clave es el nombre del 
# conjunto y el valor su tamaño.

# Una vez importados los datos, solamente se tiene que llamar a la función 
# euler() del paquete eulerr para crear el diagrama de Venn.

# Se puede sacar por pantalla con la función plot().

library(eulerr)

data <- c(uno = 100, 
          dos = 220, 
          tres = 150, 
          "uno&dos" = 24, 
          "uno&tres" = 10, 
          "dos&tres" = 22)

venn <- euler(data)

plot(venn)