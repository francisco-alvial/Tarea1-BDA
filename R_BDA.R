library(mongolite)
library(tm)
library(SnowballC)
library(jsonlite)
library(stringi)
library(ggplot2)
library(Rcpp) 
library(wordcloud)
library(RColorBrewer) 
library(cluster) 
library(fpc)
library(rmarkdown)

```{r}

render("C:/Users/franc/Documents/Gitkraken/tarea1BDA/R_BDA.R", output_format = "pdf_document")

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
Comentarios <- c$find( '{}', '{"comentarios" : true}')

corpus<-VCorpus(VectorSource(Comentarios[2]))

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

corpus <- tm_map(corpus, toSpace, '"')
# Remover stopwords usando la lista estandar de tm
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus,stemDocument)

# Matriz de documentos - t�rminos (MDT)
dtm<- DocumentTermMatrix(corpus)
dtm
# Contar la frecuencia de ocurrencia de cada palabra en el corpus
# .es decir sumar todas las filas y mostrar las sumas de cada columna
freq <- colSums(as.matrix(dtm))
# Ordenar las frecuencias
ord <- order(freq,decreasing=TRUE)
# Listar los t�rminos m�s y menos frecuentes
freq[head(ord)]
inspect(dtm)
findAssocs(dtm,"difficult",0.5)
findAssocs(dtm,"compani",0.6)
findAssocs(dtm,"place",0.6)
findAssocs(dtm,c("work", "great"),0.6)

##################################################################################



######################## Pregunta 4 ##############################################

datos_facebook <- c$find('{"company" : "facebook" }', '{"comentarios.summary" : true}')
datos_google <- c$find('{"company" : "google" }', '{"comentarios.summary" : true}')
datos_apple <- c$find('{"company" : "apple" }', '{"comentarios.summary" : true}')

corpus_facebook<-VCorpus(VectorSource(datos_facebook[2]))
corpus_google<-VCorpus(VectorSource(datos_google[2]))
corpus_apple<-VCorpus(VectorSource(datos_apple[2]))

corpus_facebook <- tm_map(corpus_facebook, toSpace, '"')
# Remover stopwords usando la lista estandar de tm
corpus_facebook <- tm_map(corpus_facebook, removeWords, stopwords("english"))
corpus_facebook <- tm_map(corpus_facebook,stemDocument)

dtm<- DocumentTermMatrix(corpus_facebook)
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]

corpus_google <- tm_map(corpus_google, toSpace, '"')
# Remover stopwords usando la lista estandar de tm
corpus_google <- tm_map(corpus_google, removeWords, stopwords("english"))
corpus_google <- tm_map(corpus_google,stemDocument)

dtm<- DocumentTermMatrix(corpus_google)
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]

corpus_apple <- tm_map(corpus_apple, toSpace, '"')
# Remover stopwords usando la lista estandar de tm
corpus_apple <- tm_map(corpus_apple, removeWords, stopwords("english"))
corpus_apple <- tm_map(corpus_apple,stemDocument)

dtm<- DocumentTermMatrix(corpus_apple)
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
###############################################################

################### PREGUNTA 5 ################################

Comentarios_p_c <- c$find( '{}', '{"comentarios.pros" : true, "comentarios.cons" : true}')

cuerpo_pros_contra<-VCorpus(VectorSource(Comentarios_p_c[2]))

cuerpo_pros_contra <- tm_map(cuerpo_pros_contra, toSpace, '"')
# Remover stopwords usando la lista estandar de tm
cuerpo_pros_contra <- tm_map(cuerpo_pros_contra, removeWords, stopwords("english"))
cuerpo_pros_contra <- tm_map(cuerpo_pros_contra,stemDocument)

# Matriz de documentos - t�rminos (MDT)
dtm<- DocumentTermMatrix(cuerpo_pros_contra)
dtm
# Contar la frecuencia de ocurrencia de cada palabra en el corpus
# .es decir sumar todas las filas y mostrar las sumas de cada columna
freq <- colSums(as.matrix(dtm))
# Ordenar las frecuencias
ord <- order(freq,decreasing=TRUE)
# Listar los t�rminos m�s y menos frecuentes
freq[head(ord)]
###############################################################

################### PREGUNTA 6 ################################
# Setear un valor semilla
set.seed(42)
# Nube de palabras en colores; palabras con frecuencia m�nima de 70
wordcloud(names(freq), freq,min.freq=5000,colors=brewer.pal(6,"Dark2")) 

###############################################################

################### PREGUNTA 7 ################################
dtm <- dtm[, findFreqTerms(dtm, lowfreq = 10000)]
dtmr2 <- removeSparseTerms(dtm, sparse = 0.15)
distMatrix <- dist(t(dtm), method= "maximum")# probar con otros m�todos
fit <- hclust(distMatrix, method = "complete")# probar con otros m�todos
# Dendrograma
plot(fit, cex=0.9, hang=-1, main = "Dendrograma de Clusters de Palabras")
rect.hclust(fit, k = 4, border="green") 

# De los clusters se ven muchos tipos de relaciones, por ejemplo se identifica uno
# que relaciona "compa�ia" con "bienes", como tambien "empleado" con "equipo" y asi
# otros mas. Tomando los recuadros verdes se observa que los cluster reseltan una
# gran relacion a lo que se refieren las personas al hablar sobre compa�ias y empresas.


###############################################################


################# PREGUNTA EXTRA ##############################
# cuales son los terminos mas comunes de los comentarios negativos 
# de la compa�ia peor evaluada y encontrar una relacion en base a los 
# clusters generados por los terminos mas frecuentes.

datos_netflix <- c$find('{"company" : "netflix" }', '{"comentarios.cons" : true}')


corpus_netflix<-VCorpus(VectorSource(datos_netflix[2]))


corpus_netflix <- tm_map(corpus_netflix, toSpace, '"')
# Remover stopwords usando la lista estandar de tm
corpus_netflix <- tm_map(corpus_netflix, removeWords, stopwords("english"))
corpus_netflix <- tm_map(corpus_netflix,stemDocument)

dtm<- DocumentTermMatrix(corpus_netflix)
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]

dtm <- dtm[, findFreqTerms(dtm, lowfreq = 100)]
dtmr2 <- removeSparseTerms(dtm, sparse = 0.15)
distMatrix <- dist(t(dtm), method= "maximum")# probar con otros m�todos
fit <- hclust(distMatrix, method = "complete")# probar con otros m�todos
# Dendrograma
plot(fit, cex=0.9, hang=-1, main = "Dendrograma de Clusters de Palabras")
rect.hclust(fit, k = 4, border="green") 
##############################################################
```