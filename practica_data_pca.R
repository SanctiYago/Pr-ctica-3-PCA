
# Lunes 28 de abril de 2025 -----------------------------------------------

# Leemos los datos 

require(tidyverse)

datos <- read.csv2("data_pca2.csv")
getwd()

# Analizamos los datos 

summary(datos)
view(datos)

# Corregimos 

data2 <- scale(datos[,-16])
view(data2)
# Calculamos el determinante de la correlacion 
det(cor(data2))
# Los datos son adecuados para realizar un PCA


# Determinamos los componentes principales
pca_data1 <- princomp(data2)
summary(pca_data1)

# NOTA
# Podemos ver que hasta el componente 6 explica la varianza total 

# Revisamos la varianza y eigenvalores
library(factoextra)

# Grafico de sedimentacion
# Para identificar el numero de clusters que debemos de utilizar
fviz_eig(pca_data1, choice="variance")

# Debemos de ver que, gracias al metodo del codo, debemos de elegir 6 componentes
# principales.

# Realizamos la grafica pero con un eigenvalor

# Calculamos los eigenvalores
fviz_eig(pca_data1, choice="eigenvalue")

# NOTA:
# AL realizar el metodo del codo con el eigen valor, podemos ver que 
# tambien los primeros 6 componentes son los que explican la mayor varianza.
# Podemos notar que es lo mismo que cuando graficamos con la varianza.


# ANALISIS GRAFICO  -------------------------------------------------------

# Grafico de las puntuaciones factoriales y su representacion
fviz_pca_ind(pca_data1,
             col.ind = "cos2",
             gradient.cols = c("red", "purple", "green"),
             repel = F)
# Interpretacion:
# Gracias al grafico, podemos ver que esta tecnica no es la optima 
# para explicar los datos.


# Realizamos otra grafica
fviz_pca_var(pca_data1,
             col.var = "contrib",
             gradient.cols = c("red", "purple", "green"),
             repel = F)
# Interpretacion:
# Las flechas de color rojo pertenecen a la dimension 2, mientras que las
# flechas de color verde representan la dimension 1. Las dos contribuyen 
# de manera distinta.



# Combinamos los graficos
fviz_pca_biplot(pca_data1,
                col.var = "red",
                col.ind = "blue")
data1[8,1]

data1[24,]

data1[14,]


# Clase 25 de abril de 2025 -----------------------------------------------

data1[8,]

# Grafico de correlacion 
x11()


psych::cor.plot(data2)
# En un analisis de PCA, los componentes deben de tener una correlacion alta 
# esto es para que tenga sentido realizar este proceso.

det(cor(data2))

# Rotacion VARIMAX

pca2 <- psych::principal(data2, nfactors = 2, residuals = F, rotate = "varimax",
                         scores = T, oblique.scores = F, method = "regression",
                         use = "pairwise", cor = "cor",weight = NULL )
pca2

# Accedemos a los datos de este data frame
pca2$weights[,1]

# Para el aspecto fisico
pca2$weights[,2]

# Nuevas variables obtenidas, cuya principal caracteristica es que son 
# ortogonales, es decir, linealmente independientes.


# Las variables son las siguientes:
pca2$scores

