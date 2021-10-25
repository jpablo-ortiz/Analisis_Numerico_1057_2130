# Todos los metodos de interpolación


# Interpolación lineal
# (Los approx tienen la función approxfun())
approx(x, y)

# Interpolación Polinomial
# (Los spline tienen los splinefun())

# Método FMM (fast marching method) (método de marcha rápida)
# Este método utiliza Forsythe, Malcolm y Moler. Se ajústa un un cúbico exacto
spline(x, y, method = "fmm")

# Método Spline Periodic
spline(x, y, method = "periodic")

# Método Spline Natural
spline(x, y, method = "natural")

# Método Spline MonoH.FC
# Calcula un spline de Hermite monotónico (creciente o decreciente) según el método de Fritsh y Carlson.
spline(x, y, method = "monoH.FC")

# Método Spline Hyman
# Calcula un spline cúbico monótono usando el filtrado de Hyman.
spline(x, y, method = "hyman")

# Método Barycentrico (barycentric lagrange) de interpolación
barylag(x, y)

# Método spline cúbico de interpolación natural.
cubicspline(x, y)

# Método Lagrange de interpolación
lagrangeInterp(x, y)

# Método de interpolación de Newton
newtonInterp(x, y)
