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

errores() <- function(actual, predicted) {
    # El calculo del error se hace con base al 30% de los datos y de estos es los datos reales y los datos predecidos
    # actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
    # predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
    
    # Error Absoluto
    errorAbsoluto = ae(actual, predicted)
    print(errorAbsoluto)

    # Error Media
    ErrorMedia = mae(actual, predicted)
    print(ErrorMedia)

    # Error Máximo
    maxError = max(errorAbsoluto)
    print(maxError)

    # Error Mínimo
    minError = min(errorAbsoluto)
    print(minError)

    # Error medio cuadrado (EMC)
    EMC = mse(actual, predicted)
    print(EMC)

    # Indice de Jaccard
    IndiceJaccard = jaccard(actual, predicted, testNA = FALSE)
}

interpolaciones <- function(x, y, xTrain, yTrain) {
    plot(x, y, ylab = "Temperatura interna", xlab = "Indice", col= "black") 

    # Impresión de los datos originales}
    lines(x, y, col = "black", lwd = 2)

    # ----------- Interpolación lineal -----------
    # (Los approx tienen la función approxfun())
    line0 = approx(xTrain, yTrain)
    #lines(line0, col = "purple")
    # --------------------------------------------
    
    # (Los spline tienen los splinefun())
    
    # -------- Interpolación Método FMM ----------
    # Método FMM (fast marching method) (método de marcha rápida)
    # Este método utiliza Forsythe, Malcolm y Moler. Se ajusta un un cúbico exacto
    line1 = spline(xTrain, yTrain, method = "fmm")
    lines(line1, col = "red")
    # --------------------------------------------
    
    # ------ Interpolación Método Periodic -------
    # Método Spline Periodic
    # TODO - Creo que no se puede porque no es periodico los datos
    #line2 = spline(xTrain, yTrain, method = "periodic")
    #lines(line2, col = "blue")
    # --------------------------------------------
    
    # ------- Interpolación Método Natural -------
    # Método Spline Natural
    line3 = spline(xTrain, yTrain, method = "natural")
    lines(line3, col = "green")
    # --------------------------------------------
    
    # ------ Interpolación Método MonoH.FC -------
    # Método Spline MonoH.FC
    # Calcula un spline de Hermite monotónico (creciente o decreciente) según el método de Fritsh y Carlson.
    #line4 = spline(xTrain, yTrain, method = "monoH.FC")
    ###lines(line4, col = "yellow")
    # --------------------------------------------
    
    # -------- Interpolación Método Hyman --------
    # Método Spline Hyman
    # Calcula un spline cúbico monótono usando el filtrado de Hyman.
    #line5 = spline(xTrain, yTrain, method = "hyman")
    ###lines(line5, col = "orange")
    # --------------------------------------------

    # ---- Interpolación Método Barycentrico -----
    # Método Barycentrico (barycentric lagrange) de interpolación
    #line6 = barylag(xTrain, yTrain, xTrain)
    ##lines(line6, col = "purple")
    # --------------------------------------------
    
    # ------- Interpolación Método Cúbico --------
    # Método spline cúbico de interpolación natural.
    #line7 = cubicspline(xTrain, yTrain, xTrain)
    ###lines(line7, col = "brown")
    # --------------------------------------------

    # ------ Interpolación Método Lagrange -------
    # Método Lagrange de interpolación
    line8 = lagrangeInterp(xTrain, yTrain, x)
    lines(line8, col = "purple", lwd = 3)
    # --------------------------------------------
    
    # ------- Interpolación Método Newton --------
    # Método de interpolación de Newton
    #line9 = newtonInterp(xTrain, yTrain, x)
    ##lines(line9, col = "pink")
    # --------------------------------------------
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

    # Seleccionar un 70% aleatorio de los datos para el entrenamiento y el 30% para el test
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

    interpolaciones(x, y, xTrain, yTrain)
}

# --------------------------------------------------
# ---------------------- Main ----------------------
# --------------------------------------------------

procesamientoDosEstaciones("Itatira", "Santa Quitéria")
#procesamientoDosEstaciones("São Gonçalo do Amarante", "Pentecoste")
#procesamientoDosEstaciones("Quixadá", "quixeramobim")