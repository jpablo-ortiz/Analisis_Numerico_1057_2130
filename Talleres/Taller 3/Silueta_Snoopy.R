#############
#Variables
#############


x = c(00.00, 01.00, 02.00, 06.00, 07.00, 08.00, 10.00, 13.00, 17.00, 20.00, 23.00, 24.00, 25.00, 27.00, 27.70, 28.00, 29.00, 30.00)
y = c(03.00, 03.70, 03.90, 04.20, 05.70, 06.60, 07.10, 06.70, 04.50, 07.00, 06.10, 05.60, 05.80, 05.20, 04.10, 04.30, 04.10, 03.00)
#Procedimiento
plot(x,y,main = "silueta")
vx1 = c(x[1:9])
vy1 = c(y[1:9])
splines = splinefun(vx1,vy1, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx1[1], to = vx1[length(vx1)])
vx2 = c(x[9:15])
vy2 = c(y[9:15])
splines = splinefun(vx2,vy2, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx2[1], to = vx2[length(vx2)])
vx3 = c(x[15:18])
vy3 = c(y[15:18])
splines = splinefun(vx3,vy3, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx3[1], to = vx3[length(vx3)])