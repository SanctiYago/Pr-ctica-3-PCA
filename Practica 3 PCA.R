
# Lunes 28 de abril de 2025 Practica 2 ------------------------------------

# Leemos los datos
require(tidyverse)
require(readxl)

datos_generales <- read_xlsx("Covid.xlsm")
getwd()

summary(datos_generales)
view(datos_generales)

# Base de datos 2000
data_2000 <- datos_generales[,c(1,2,3,5,7,9,11,13,15,17,19)]
view(data_2000)





# BASE DE DATOS 2000 ------------------------------------------------------

# Normalizamos los datos
norm_2000 <- scale(data_2000[,-1])
view(norm_2000)

det(cor(norm_2000))
# Determinante que tiende a cero, lo cual nos indica 
# que los datos son optimos para un analisis de componentes principales.

pca_norm_2000 <- princomp(norm_2000)

summary(pca_norm_2000)

# Debemos de quedarnos con 2 componentes, ya que hasta el componente 2 explica 
# el 87.58% de la varianza.

# Realizamos pca
# Calculamos el factor de adecuacion muestral de Kaiser
psych::KMO(norm_2000)

# El valor obtenido al correr KMO de 0.5, no es el valor optimo, ya que 
# el valor optimo seria de 0.6 o mayor, pero es un valor util.

# Revisamos la varianza y eigenvalores
library(factoextra)

# Revisamos la varianza y los eigen valores
# VARIANZA
fviz_eig(pca_norm_2000, choice="variance")
# Con el grafico y el metodo del codo, podemos ver que nos quedamos con 2 componentes principales
# Esto re afirma lo que habiamos dicho anteriormente.


# Realizamos el eigen valor
fviz_eig(pca_norm_2000, choice="eigenvalue")
# Para esto, debemos de ver que extrae solo dos componentes tienen un eigenvalor mayor 
# a la unidad, lo adecuado es extraer unicamente dos factores.

# ANALISIS GRAFICO
# Grafico de las puntuaciones factoriales y su representacion
fviz_pca_ind(pca_norm_2000,
             col.ind = "cos2",
             gradient.cols = c("red", "purple", "green"),
             repel = F)
# Interpretacion:
# Gracias al grafico, podemos ver que los datos que estan de color verde
# son aqullas observaciones que estan bien representadas, mientras que 
# aquellas que estan de color rojo, son aquellas que no estan bien representadas.

# Realizamos otra grafica
fviz_pca_var(pca_norm_2000,
             col.var = "contrib",
             gradient.cols = c("red", "purple", "green"),
             repel = F)
# Interpretacion:
# Las flechas de color rojo pertenecen a la dimension 2, mientras que las
# flechas de color verde representan la dimension 1. Las dos contribuyen 
# de manera distinta.

# Combinamos los graficos
fviz_pca_biplot(pca_norm_2000,
                col.var = "red",
                col.ind = "blue")

covid_2000[8,1]

covid_2000[24,]

covid_2000[14,]

psych::cor.plot(norm_2000)
# En un analisis de PCA, los componentes deben de tener una correlacion alta 
# esto es para que tenga sentido realizar este proceso.

det(cor(norm_2000))

# Rotacion VARIMAX

pca_2000 <- psych::principal(norm_2000, nfactors = 2, residuals = F, rotate = "varimax",
                         scores = T, oblique.scores = F, method = "regression",
                         use = "pairwise", cor = "cor",weight = NULL )
pca_2000

# Accedemos a los datos de este data frame
pca_2000$weights[,1]

# Para el aspecto fisico
pca_2000$weights[,2]

# Nuevas variables obtenidas, cuya principal caracteristica es que son 
# ortogonales, es decir, linealmente independientes.


# Las variables son las siguientes:
pca_2000$scores


# BASE DE DATOS 2001 ------------------------------------------------------

# Base de datos 2001
data_2001 <- datos_generales[,c(1,4,6,8,10,12,14,16,18,20)]
view(data_2001)

# Normalizamos los datos
norm_2001 <- scale(data_2001[,-1])
view(norm_2001)

det(cor(norm_2001))
# Determinante que tiende a cero, lo cual nos indica 
# que nuestros datos son optimos para un analisis de componentes principales.

pca_norm_2001 <- princomp(norm_2001)

summary(pca_norm_2001)

# Debemos de quedarnos con 2 componentes, ya que hasta el componente 2 explica 
# el 89.84% de la varianza.

# Realizamos pca
# Calculamos el factor de adecuacion muestral de Kaiser
psych::KMO(norm_2001)

# El valor obtenido al correr KMO de 0.5, no es un valor optimo, ya que 
# el valor optimo seria de 0.6 o mayor, pero es un valor util.

# Revisamos la varianza y eigenvalores
library(factoextra)

# Revisamos la varianza y los eigen valores
# VARIANZA
fviz_eig(pca_norm_2001, choice="variance")
# Con el grafico y el "metodo del codo", podemos ver que nos quedamos con 2 componentes principales
# Esto re afirma lo que habiamos dicho anteriormente.


# Realizamos el eigen valor
fviz_eig(pca_norm_2001, choice="eigenvalue")
# Para esto, debemos de ver que extrae solo dos componentes tienen un eigenvalor mayor 
# a la unidad, lo adecuado es extraer unicamente dos factores.

# ANALISIS GRAFICO
# Grafico de las puntuaciones factoriales y su representacion
fviz_pca_ind(pca_norm_2001,
             col.ind = "cos2",
             gradient.cols = c("red", "purple", "green"),
             repel = F)
# Interpretacion:
# Gracias al grafico, podemos ver que los datos que estan de color verde
# son aquellas observaciones que estan bien representadas, mientras que 
# aquellas que estan de color rojo, son aquellas que no estan bien representadas.

# Realizamos otra grafica
fviz_pca_var(pca_norm_2001,
             col.var = "contrib",
             gradient.cols = c("red", "purple", "green"),
             repel = F)
# Interpretacion:
# Las flechas de color rojo pertenecen a la dimension 2, mientras que las
# flechas de color verde representan la dimension 1. Las dos contribuyen 
# de manera distinta.

# Combinamos los graficos
fviz_pca_biplot(pca_norm_2001,
                col.var = "red",
                col.ind = "blue")

norm_2001[8,1]

norm_2001[24,]

norm_2001[14,]

psych::cor.plot(norm_2001)
# En un analisis de PCA, los componentes deben de tener una correlacion alta 
# esto es para que tenga sentido realizar este proceso.

det(cor(norm_2001))

# Rotacion VARIMAX

pca_2001 <- psych::principal(norm_2001, nfactors = 2, residuals = F, rotate = "varimax",
                             scores = T, oblique.scores = F, method = "regression",
                             use = "pairwise", cor = "cor",weight = NULL )
pca_2001

# Accedemos a los datos de este data frame
pca_2001$weights[,1]

# Para el aspecto fisico
pca_2001$weights[,2]

# Nuevas variables obtenidas, cuya principal caracteristica es que son 
# ortogonales, es decir, linealmente independientes.


# Las variables son las siguientes:
pca_2001$scores
