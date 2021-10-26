# --------------------------------------------------
# ---------------- Instalar paquetes ---------------
# --------------------------------------------------

# install.packages("pracma")
# install.packages("readxl")
# install.packages("Metrics")
# install.packages('philentropy')

# --------------------------------------------------
# -------------- Importar librerías ----------------

library(pracma)
library(readxl)
library(Metrics)
library(philentropy)

# --------------------------------------------------
# --------------- Variables Globales ---------------
# --------------------------------------------------

# --------------------------------------------------
# ---------------- Funciones -----------------------
# --------------------------------------------------

funInterpGenerator <- function(pp, xs) {
    ppfun <- function(xs) ppval(pp, xs)
    return(ppfun)
}

errores <- function(nombre, actual, predicted) {
    print(paste0("-------- Error de la interpolación ", nombre, "--------"))

    # Error Absoluto
    errorAbsoluto = ae(actual, predicted)
    #print("Error Absoluto: ")
    #print(errorAbsoluto)

    # Error Media
    errorMedia = round(mae(actual, predicted), 2)
    print(paste0("Error Media: ", errorMedia, "ºC"))

    # Error Máximo
    maxError = round(max(errorAbsoluto), 2)
    print(paste0("Error Máximo: ", maxError, "ºC"))

    # Error Mínimo
    minError = round(min(errorAbsoluto), 2)
    print(paste0("Error Mínimo: ", minError, "ºC"))

    # Error medio cuadrático (EMC)
    EMC = round(mse(actual, predicted), 2)
    print(paste0("Error medio cuadrático: ", EMC, "ºC"))

    # Indice de Jaccard
    indiceJaccard = jaccard(actual, predicted, testNA = FALSE)
    print(paste0("Indice de Jaccard: ", indiceJaccard))
}

interpolaciones <- function(x, y, xTrain, yTrain, xTest, yTest, tamArch1, tamArch2, diasArch1, diasArch2, horasArch1, horasArch2, estacion1, estacion2) {


}

# cicloDeUnaEstacion() <- function() {}

# --------------------------------------------------
# ---- Función carga de datos de dos Estaciones ----
# --------------------------------------------------

procesamientoDosEstaciones <- function(estacion1, estacion2) {

    arch1 = read_excel("D:/Juanpa/U/Semestre 8/Analisis Numerico/Analisis_Numerico_1057_2130/Reto Parcial 2/DatosReto2.xls", sheet = estacion1)
    arch2 = read_excel("D:/Juanpa/U/Semestre 8/Analisis Numerico/Analisis_Numerico_1057_2130/Reto Parcial 2/DatosReto2.xls", sheet = estacion2)

    tempInteranArch1 = arch1$`Temp. Interna (ºC)`
    diasArch1 = arch1$`Dia Juliano`
    horasArch1 = arch1$Hora

    tempInteranArch2 = arch2$`Temp. Interna (ºC)`
    diasArch2 = arch2$`Dia Juliano`
    horasArch2 = arch2$Hora

    tamArch1 = length(tempInteranArch1)
    tamArch2 = length(tempInteranArch2)

    x = seq(from = 1, to = tamArch1, by = 1)
    y = tempInteranArch1

    # Seleccionar un 70% aleatorio de los datos para el entrenamiento 
    #sample_i = sample(tamArch1, round(tamArch1 * 0.7))
    sample_i = sample(2, tamArch1, replace = TRUE, prob = c(0.7, 0.3))

    # XTrain y YTrain el 70% de los datos
    # Se toma el primero y el último valor de X y Y para manejar una correcta interpolación
    xTrain = x[sample_i == 1]
    xTrain = append(xTrain, x[0], 0) # Agregar el primer valor de x (al inicio)
    xTrain = append(xTrain, x[length(x)]) # Agregar el ultimo valor de x (al final)

    yTrain = y[sample_i == 1]
    yTrain = append(yTrain, y[0], 0) # Agregar el primer valor de y (al inicio)
    yTrain = append(yTrain, y[length(y)]) # Agregar el ultimo valor de y (al final)

    # XTest y YTest el 30% de los datos
    # Se toma el primero y el último valor de X y Y para manejar una correcta interpolación
    xTest = x[sample_i == 2]
    xTest = append(xTest, x[0], 0) # Agregar el primer valor de x (al inicio)
    xTest = append(xTest, x[length(x)]) # Agregar el ultimo valor de x (al final)

    yTest = y[sample_i == 2]
    yTest = append(yTest, y[0], 0) # Agregar el primer valor de y (al inicio)
    yTest = append(yTest, y[length(y)]) # Agregar el ultimo valor de y (al final)

    #print(tamArch1)
    
    #print(length(xTrain))
    #print(xTrain)
    #print(length(yTrain))
    #print(yTrain)

    #print(length(xTest))
    #print(xTest)
    #print(length(yTest))
    #print(yTest)

    # -----------------------------------------------------------
    # ---------------------- Entrenamiento ---------------------- 
    # -----------------------------------------------------------

    # ----------- Interpolación lineal -----------
    # (Los approx tienen la función approxfun())

    # Función de la interpolación
    funInterp1 = approxfun(xTrain, yTrain)

    # Datos de entrenamiento
    lineInterp1 = approx(xTrain, yTrain)

    # Datos de test
    yPredicted1 = funInterp1(xTest)
    error1 = errores(paste0("Lineal - Test Data Vs Predicted Data - Estación Base: ", estacion1), yTest, yPredicted1)
    # --------------------------------------------
    
    # -------- Interpolación Método FMM ----------
    # Método FMM (fast marching method) (método de marcha rápida)
    # Este método utiliza Forsythe, Malcolm y Moler. Se ajusta un un cúbico exacto

    # Función de la interpolación
    funInterp2 = splinefun(xTrain, yTrain, method = "fmm")
    
    # Datos de entrenamiento
    lineInterp2 = spline(xTrain, yTrain, method = "fmm")

    # Datos de test
    yPredicted2 = funInterp2(xTest)
    error2 = errores(paste0("FMM - Test Data Vs Predicted Data - Estación Base: ", estacion1), yTest, yPredicted2)
    # --------------------------------------------
    
    # ------- Interpolación Método Natural -------
    # Método Spline Natural

    # Función de la interpolación
    funInterp3 = splinefun(xTrain, yTrain, method = "natural")

    # Datos de entrenamiento
    lineInterp3 = spline(xTrain, yTrain, method = "natural")

    # Datos de test
    yPredicted3 = funInterp3(xTest)
    error3 = errores(paste0("Natural - Test Data Vs Predicted Data - Estación Base: ", estacion1), yTest, yPredicted3)
    # --------------------------------------------

    # Plot de los resultados de la interpolación de los datos de entrenamiento
    plot(xTrain, yTrain, type = "p", col = "black", lwd = 1,
            ylab = "Temperatura interna (Entrenamiento)", xlab = "Indice (Entrenamiento)", 
            main = paste0("(Train Data) Estación: ", estacion1))
    lines(xTrain, yTrain, col = "black", lwd = 0.5)
    lines(lineInterp1, col = "blue", lwd = 1, type = "l", pch = 10, lty = 1)
    lines(lineInterp2, col = "red", lwd = 3, type = "l", pch = 10, lty = 1)    
    lines(lineInterp3, col = "green", lwd = 1, type = "l", pch = 10, lty = 1)

    # Plot de los resultados de la interpolación de los datos de test
    plot(xTest, yTest, type = "p", col = "black", lwd = 1,
            ylab = "Temperatura interna (Test)", xlab = "Indice (Test)",
            main = paste0("(Test Data) Estación: ", estacion1))
    lines(xTest, yTest, col = "black", lwd = 0.5)
    lines(xTest, yPredicted1, col = "blue", lwd = 1, type = "l", pch = 10, lty = 1)
    lines(xTest, yPredicted2, col = "red", lwd = 3, type = "l", pch = 10, lty = 1)
    lines(xTest, yPredicted3, col = "green", lwd = 1, type = "l", pch = 10, lty = 1)

    # -----------------------------------------------------------
    # ------------ Predicción de datos Ciudad cercana ----------- 
    # -----------------------------------------------------------

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

    yPredicted1 = funInterp1(indicesBajoCondicionesIguales)
    error1 = errores(paste0("(Interp Lineal) Estimación de ", estacion2, " a partir de ", estacion1), tempInteranArch2, yPredicted1)
    yPredicted2 = funInterp2(indicesBajoCondicionesIguales)
    error2 = errores(paste0("(Interp FMM) Estimación de ", estacion2, " a partir de ", estacion1), tempInteranArch2, yPredicted2)
    yPredicted3 = funInterp3(indicesBajoCondicionesIguales)
    error3 = errores(paste0("(Interp Natural) Estimación de ", estacion2, " a partir de ", estacion1), tempInteranArch2, yPredicted3)

    plot(indicesBajoCondicionesIguales, tempInteranArch2, type = "p", col = "black", lwd = 1,
        ylab = "Temperatura interna", xlab = "Indice",
        main = paste0("Estimación de Temperatura interna de ", estacion2, " a partir de ", estacion1))
    
    lines(indicesBajoCondicionesIguales, tempInteranArch2, col = "black", lwd = 0.5)
    lines(indicesBajoCondicionesIguales, yPredicted1, col = "blue", lwd = 1, type = "l", pch = 10, lty = 1)
    lines(indicesBajoCondicionesIguales, yPredicted2, col = "red", lwd = 3, type = "l", pch = 10, lty = 1)
    lines(indicesBajoCondicionesIguales, yPredicted3, col = "green", lwd = 1, type = "l", pch = 10, lty = 1)

}

# --------------------------------------------------
# ---------------------- Main ----------------------
# --------------------------------------------------

procesamientoDosEstaciones("Itatira", "Santa Quitéria")
procesamientoDosEstaciones("Araripe", "Santana do Cariri")
procesamientoDosEstaciones("Quixadá", "quixeramobim")