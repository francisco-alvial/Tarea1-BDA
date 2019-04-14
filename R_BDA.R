library(mongolite)
library(tm)
library(SnowballC)
library(jsonlite)

c=mongo(collection = "EmpresasT" ,db="BDA")
c$drop()
c$count('{}')




# se quitaron los index y links
Datos_columnas_correctas <- read.csv(file="C:/Users/franc/Documents/Gitkraken/tarea1BDA/employee_reviews.csv", header=TRUE, sep=",", colClasses=c("NULL", NA, NA, NA, NA, NA, NA, NA, NA, "numeric", NA, NA, NA, NA, NA, NA, "NULL"))


#Se hizo todo minuscula
Datos_columnas_correctas[,1:8] <- lapply(Datos_columnas_correctas[,1:8], tolower)

Datos_columnas_correctas[,4:8] <- lapply(Datos_columnas_correctas[,4:8], gsub, pattern='-', replacement='')
Datos_columnas_correctas[,4:8] <- lapply(Datos_columnas_correctas[,4:8], gsub, pattern='*', replacement='')
Datos_columnas_correctas[,4:8] <- lapply(Datos_columnas_correctas[,4:8], gsub, pattern=':', replacement='')
Datos_columnas_correctas[,4:8] <- lapply(Datos_columnas_correctas[,4:8], gsub, pattern='[[:punct:]]', replacement='')
Datos_columnas_correctas[,4:8] <- lapply(Datos_columnas_correctas[,4:8], gsub, pattern='none', replacement='')
Datos_columnas_correctas[,4:8] <- lapply(Datos_columnas_correctas[,4:8], gsub, pattern='and ', replacement='')
Datos_columnas_correctas[,4:8] <- lapply(Datos_columnas_correctas[,4:8], gsub, pattern='the ', replacement='')

for(i in 1:nrow(Datos_columnas_correctas)) {
  row <- Datos_columnas_correctas[i,]
  mylist <- list(company=row$company,
                 job_title =row$job.title,
                 comentarios=list("summary"=row$summary,
                                  "pros"=row$pros,
                                  "cons"=row$cons,
                                  "advice_to_mgmt"=row$advice.to.mgmt),
                 ratings=list("overall_ratings"=row$overall.ratings,
                              "work_balance_stars"=row$work.balance.stars,
                              "culture_values_stars"=row$culture.values.stars,
                              "carrer_opportunities_stars"=row$carrer.opportunities.stars,
                              "comp_benefit_stars"=row$comp.benefit.stars,
                              "senior_mangemnet_stars"=row$senior.mangemnet.stars)
  )
  
  insertar <- jsonlite::toJSON(mylist, pretty=TRUE, auto_unbox=TRUE)
  c$insert(insertar)
  
}



stats <- c$aggregate(
  '[{"$group":{"_id":"$company", "count": {"$sum":1}, "average":{"$avg": "$ratings.overall_ratings" }}}]',
  options = '{"allowDiskUse":true}'
)
names(stats) <- c("company", "count", "average")
print(stats)



################## PREGUNTA 2 y 3 ########################################
alldata <- c$find( '{}', '{"comentarios.summary" : true}')

corpus<-Corpus(VectorSource(alldata[2]))

# Crear un transformador de contenido llamado toSpace:
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))}) 

corpus <- tm_map(corpus, toSpace, '"') 
corpus <- tm_map(corpus,stemDocument) 
corpus <- tm_map(corpus, removeWords, stopwords("english"))


# Matriz de documentos - términos (MDT)
dtm<- DocumentTermMatrix(corpus)
(w <- with(presidential_debates_2012, q_dtm(corpus, paste(time, tot, sep = "_"))))
# Contar la frecuencia de ocurrencia de cada palabra en el corpus
# .es decir sumar todas las filas y mostrar las sumas de cada columna
freq <- colSums(as.matrix(w))
# Ordenar las frecuencias
ord <- order(freq,decreasing=TRUE)
# Listar los términos más y menos frecuentes
freq[head(ord)]

##################################################################################



######################## Pregunta 4 ##############################################

datos_facebook <- c$find('{"company" : "facebook" }')
datos_google <- c$find('{"company" : "google" }')
datos_apple <- c$find('{"company" : "apple" }')




corpus_facebook<-Corpus(VectorSource(datos_facebook[5]))
corpus_google<-Corpus(VectorSource(datos_google[5]))
corpus_apple<-Corpus(VectorSource(datos_apple[5]))




corpus_facebook <- tm_map(corpus_facebook, toSpace, "-")
corpus_facebook <- tm_map(corpus_facebook, toSpace, ":")
corpus_facebook <- tm_map(corpus_facebook, removePunctuation)
corpus_facebook <- tm_map(corpus_facebook, toSpace, "'")
corpus_facebook <- tm_map(corpus_facebook, toSpace, "'")
corpus_facebook <- tm_map(corpus_facebook, toSpace, " -")
corpus_facebook <- tm_map(corpus_facebook, toSpace, "the ")
corpus_facebook <- tm_map(corpus_facebook, toSpace, "and ")
corpus_facebook <- tm_map(corpus_facebook, toSpace, "none")
corpus_facebook <- tm_map(corpus_facebook, PlainTextDocument)
corpus_facebook<- tm_map(corpus_facebook,removePunctuation)
corpus_facebook <- tm_map(corpus_facebook, removeNumbers)
corpus_facebook <- tm_map(corpus_facebook, removeWords, stopwords("english"))
corpus_facebook <- tm_map(corpus_facebook, stripWhitespace)
corpus_facebook <- tm_map(corpus_facebook,stemDocument)
dtm<- DocumentTermMatrix(corpus_facebook)
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]

corpus_google <- tm_map(corpus_google, toSpace, "-")
corpus_google <- tm_map(corpus_google, toSpace, ":")
corpus_google <- tm_map(corpus_google, removePunctuation)
corpus_google <- tm_map(corpus_google, toSpace, "'")
corpus_google <- tm_map(corpus_google, toSpace, "'")
corpus_google <- tm_map(corpus_google, toSpace, " -")
corpus_google <- tm_map(corpus_google, toSpace, "the ")
corpus_google <- tm_map(corpus_google, toSpace, "and ")
corpus_google <- tm_map(corpus_google, toSpace, "none")
corpus_google <- tm_map(corpus_google, PlainTextDocument)
corpus_google<- tm_map(corpus_google,removePunctuation)
corpus_google <- tm_map(corpus_google, removeNumbers)
corpus_google <- tm_map(corpus_google, removeWords, stopwords("english"))
corpus_google <- tm_map(corpus_google, stripWhitespace)
corpus_google <- tm_map(corpus_google,stemDocument)
dtm<- DocumentTermMatrix(corpus_google)
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]

corpus_apple <- tm_map(corpus_apple, toSpace, "-")
corpus_apple <- tm_map(corpus_apple, toSpace, ":")
corpus_apple <- tm_map(corpus_apple, removePunctuation)
corpus_apple <- tm_map(corpus_apple, toSpace, "'")
corpus_apple <- tm_map(corpus_apple, toSpace, "'")
corpus_apple <- tm_map(corpus_apple, toSpace, " -")
corpus_apple <- tm_map(corpus_apple, toSpace, "the ")
corpus_apple <- tm_map(corpus_apple, toSpace, "and ")
corpus_apple <- tm_map(corpus_apple, toSpace, "none")
corpus_apple <- tm_map(corpus_apple, PlainTextDocument)
corpus_apple<- tm_map(corpus_apple,removePunctuation)
corpus_apple <- tm_map(corpus_apple, removeNumbers)
corpus_apple <- tm_map(corpus_apple, removeWords, stopwords("english"))
corpus_apple <- tm_map(corpus_apple, stripWhitespace)
corpus_apple <- tm_map(corpus_apple,stemDocument)
dtm<- DocumentTermMatrix(corpus_apple)
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
###############################################################

################### PREGUNTA 5 ################################
