library(ggpubr)
library(ggplot2)
library(dplyr)
library(FSelector)
library(NoiseFiltersR)
library("cowplot")
library("arulesViz")
library("C50")
library("caret")

set.seed(4)
# Se carga la cabecera de la base de datos.
head <- c("animal name","hair",
          "feathers","eggs","milk","airborne","aquatic","predator",
          "toothed","backbone","breathes","venomous","fins","legs",
          "tail","domestic","catsize","type")

# Se cargan los datos
data <- read.table("zoo.data",header = FALSE, sep = ",")

# Se agrega cabecera a la base de datos
colnames(data) <- head

# se generan valores de info_gain y ratio_gain
atr.info <- information.gain(type~., data)
atr.ratio <- gain.ratio(type~., data)

# creación de data frame que contiene los valores de info gain y ratio gain
peso.atr <- data.frame(
  head[head!="type"],
  atr.info$attr_importance,
  atr.ratio$attr_importance
)

colnames(peso.atr) <- c("atributo", "info_gain", "ratio_gain")

#Eliminacion de atributos
data <- subset( data, select = -c(aquatic, venomous, predator, domestic, catsize) )
data <- data[-26, ]

tabla <- table(data$type)
prob <- prop.table(tabla)

#############################################

num.wide.data <- data[,-1] # se usa cuando se necesiten los valores numericos
data$type <- factor(data$type)
data.wide <- data[,-1]

# Para hacer un resumen de las variables adecuado se transforman las columnas con valores 1 y 0 a booleanos

data.wide[["hair"]] <- factor(data.wide[["hair"]])
data.wide[["feathers"]] <- factor(data.wide[["feathers"]])
data.wide[["eggs"]] <- factor(data.wide[["eggs"]])
data.wide[["milk"]] <- factor(data.wide[["milk"]])
data.wide[["airborne"]] <- factor(data.wide[["airborne"]])
data.wide[["toothed"]] <- factor(data.wide[["toothed"]])
data.wide[["backbone"]] <- factor(data.wide[["backbone"]])
data.wide[["breathes"]] <- factor(data.wide[["breathes"]])
data.wide[["fins"]] <- factor(data.wide[["fins"]])
data.wide[["tail"]] <- factor(data.wide[["tail"]])
data.wide[["legs"]] <- factor(data.wide[["legs"]])

summary(data.wide)


# se agrupan las clases 3 5 y 7, en la clase 3
data.wide <- mutate(data.wide, type = case_when(type == 1 ~ "mamifero",
                                                type == 2 ~ "ave",
                                                type == 3 ~ "otro",
                                                type == 4 ~ "pez",
                                                type == 5 ~ "otro",
                                                type == 6 ~ "insecto",
                                                type == 7 ~ "otro"))
data.wide$type <- factor(data.wide$type)

training.index <- createDataPartition(data.wide$type, p=0.7)$Resample1
training.set <- data.wide[training.index, ]
test.set <- data.wide[-training.index, ]

tree <- C5.0(type ~ ., training.set) 

tree.rules <- C5.0(x = training.set[, -12], y = training.set$type, rules = T)

tree.pred.class <- predict(tree, test.set[,-12], type = "class")

tree.pred.prob <- predict(tree, test.set[,-12], type = "prob")

plot(tree)

summary(tree)

summary(tree.rules)


conf.matrix.tree <- confusionMatrix(table(test.set$type, tree.pred.class))
print(conf.matrix.tree)




