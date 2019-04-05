library(mongolite)
library(tm)
library(SnowballC)

c=c=mongo(collection = "EmpresasT" ,db="BDA")

c$count('{}')
alldata <- c$find('{}')

premium_diamonds <- c$find('{"company" : "google", "price" : { "$lt" : 1000 } }')
print(premium_diamonds)

MyData <- read.csv(file="C:/Users/franc/Documents/Gitkraken/Tarea1-BDA/employee_reviews.csv", header=TRUE, sep=",")

# se quitaron los index y links
Datos_columnas_correctas <- read.csv(file="C:/Users/franc/Documents/Gitkraken/Tarea1-BDA/employee_reviews.csv", header=TRUE, sep=",", colClasses=c("NULL", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "NULL"))

#Se hizo todo minuscula
Datos_columnas_correctas[] <- lapply(Datos_columnas_correctas, tolower)
Datos_columnas_correctas[] <- lapply(Datos_columnas_correctas, gsub, pattern='-', replacement='')
Datos_columnas_correctas[] <- lapply(Datos_columnas_correctas, gsub, pattern='*', replacement='')
Datos_columnas_correctas[] <- lapply(Datos_columnas_correctas, gsub, pattern=':', replacement='')
Datos_columnas_correctas[] <- lapply(Datos_columnas_correctas, gsub, pattern='[[:punct:]]', replacement='')
Datos_columnas_correctas[1,]

c$insert(Datos_columnas_correctas)