library(mongolite)

c=c=mongo(collection = "EmpresasT" ,db="BDA")

c$count('{}')
alldata <- c$find('{}')
print(alldata)



premium_diamonds <- c$find('{"company" : "google", "price" : { "$lt" : 1000 } }')
print(premium_diamonds)


MyData <- read.csv(file="D:/Descargas/google-amazon-facebook-employee-reviews/employee_reviews.csv", header=TRUE, sep=",")

print(MyData[17])

c$insert(MyData)


# se quitaron los index y links
Datos_columnas_correctas <- read.csv(file="D:/Descargas/google-amazon-facebook-employee-reviews/employee_reviews.csv", header=TRUE, sep=",", colClasses=c("NULL", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "NULL"))
print(Datos_columnas_correctas[17])
c$insert(Datos_columnas_correctas)

#Se hizo todo minuscula
Datos_columnas_correctas[] <- lapply(Datos_columnas_correctas, tolower)
Datos_columnas_correctas[1,]
