install.packages("randomForest")
library(randomForest)

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

hogar <- read.csv('C:/Users/orodas/Desktop/ax/mineria/db_csv_/MIGRACION_BDP.csv', sep=',')
hogar <- na.omit(hogar)

hogar <- hogar[, c("DEPARTAMENTO", "PCH1", "PCH2","PCH3","PCH4","PCH5", "PCH8", "PCH9_A",
                   "PCH9_B","PCH9_C","PCH9_D","PCH9_E","PCH9_F","PCH9_G","PCH9_H")]

hogar$DEPARTAMENTO <- as.factor(hogar$DEPARTAMENTO)

set.seed(100)
hogar <- hogar[sample(1:nrow(hogar)),]

index <-sample(1:nrow(hogar), 0.8*nrow(hogar))

train <- hogar[index,]
test <- hogar[-index,]

bosque <- randomForest(DEPARTAMENTO ~ PEI3 + PEI4 + PEI5,
                       data = train,
                       ntree = 1000,
                       mtry = 10
)

entreno <- predict(bosque, test)

entreno

dato_nuevo <- data.frame(
  PEI3=3,
  PEI4=1,
  PEI5=1
)

dato_nuevo <- data.frame(
  PEI3=1,
  PEI4=2,
  PEI5=2
)

prediccion <- predict(bosque, dato_nuevo)
prediccion

