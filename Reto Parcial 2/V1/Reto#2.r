library(pracma)
library(readxl)

arch1 = read_excel("D:/Juanpa/U/Semestre 8/Analisis Numerico/Analisis_Numerico_1057_2130/Reto Parcial 2/DatosReto2.xls", sheet = "Itatira")
arch2 = read_excel("D:/Juanpa/U/Semestre 8/Analisis Numerico/Analisis_Numerico_1057_2130/Reto Parcial 2/DatosReto2.xls", sheet = "Santa Quitéria")

tempInteranArch1 = arch1$`Temp. Interna (ºC)`
diasArch1 = arch1$`Dia Juliano`
horasArch1 = arch1$Hora

tempInteranArch2 = arch2$`Temp. Interna (ºC)`
diasArch2 = arch2$`Dia Juliano`
horasArch2 = arch2$Hora

tamArch1 = length(tempInteranArch1)
tamArch2 = length(tempInteranArch2)

x = seq(from = 1, to = 720, by = 1)
y = tempInteranArch1

# Seleccionar un 70% aleatorio de los datos para el entrenamiento 
sample_i = sample(720, round(720*0.7))

# XTrain
xTrain = x[sample_i]
xTrain = append(xTrain, x[0], 0) # Agregar el primer valor de x (al inicio)
xTrain = append(xTrain, x[length(x)]) # Agregar el ultimo valor de x (al final)

yTrain = y[sample_i]
yTrain = append(yTrain, y[0], 0) # Agregar el primer valor de y (al inicio)
yTrain = append(yTrain, y[length(y)]) # Agregar el ultimo valor de y (al final)

# Graficar todos los datos y también la interpolación de los datos de entrenamiento
plot(x, y, type='l', ylab = "Temperatura interna", xlab = "Indice", col= "black")

lines(spline(xTrain, yTrain, n=tamArch1), col= "purple")

interpolacion = splinefun(xTrain, yTrain)

# Calcular el error absoluto de la interpolación
error_absoluto = sum(abs(interpolacion(x) - y))
print(error_absoluto)

# Calcular el error relativo de la interpolación
error_relativo = error_absoluto/sum(abs(y))
print(error_relativo)

indicesBajoCondicionesIguales = c()

for(i in 1:tamArch2)
{
    for(j in 1:tamArch1)
    {
        if((diasArch1[j] == diasArch2[i]) && (horasArch1[j] == horasArch2[i]))
        {
            indicesBajoCondicionesIguales = c(indicesBajoCondicionesIguales, j)
        }
    }
}

#error_absoluto_test = c()

xTest = indicesBajoCondicionesIguales
yTest = c()
# Interpolación de los datos de xTest
for(i in indicesBajoCondicionesIguales)
{
    yTest = c(yTest, interpolacion(i))
    #error_absoluto_test = c(error_absoluto_test, abs(yTest[i] - tempInteranArch2[i]))
}

plot(xTest, tempInteranArch2, ylab = "Temperatura interna", xlab = "Indice", type = 'l', col= "red")

lines(xTest, yTest, col= "blue")

# Calcular el error máximo de la interpolación
error_maximo = max(abs(yTest - tempInteranArch2))
print(error_maximo)
# Calcular el error mínimo de la interpolación
error_minimo = min(abs(yTest - tempInteranArch2))
print(error_minimo)
# Calcular el error medio de la interpolación
error_medio = mean(abs(yTest - tempInteranArch2))
print(error_medio)
# Calcular el error absoluto de la interpolación
error_absoluto = sum(abs(yTest - tempInteranArch2))
print(error_absoluto)
# Calcular el error relativo de la interpolación
error_relativo = error_absoluto/sum(abs(tempInteranArch2))
print(error_relativo)
# Calcular el error Cuadrático Medio de la interpolación
error_cuadratico_medio = sqrt(sum((yTest - tempInteranArch2)^2)/length(yTest))
print(error_cuadratico_medio)