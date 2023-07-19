library(minpack.lm)

# Datos de las posiciones de los corredores
df <- read.table("Berlin2009.txt", header = TRUE, sep="\t")

# Posiciones de los corredores para ajustar: cada 20 metros
r <- seq(0,100,20)

########
# BOLT #
########
# Tiempo de Bolt (corredor #1) cada 20 metros
t_bolt <- as.vector(t(df[1,2:7]))

# Parámetros de inicio
a = 110
b = 12.1
k = 0.8

# Posiciones estimadas según artículo
(a/k)*log((a+b*exp(-k*t_bolt))/(a+b))+(b/k)*log((a*exp(k*t_bolt)+b)/(a+b))

# Ajuste
ajuste_bolt <- nlsLM(r ~ (a/k)*log((a+b*exp(-k*t_bolt))/(a+b))+(b/k)*log((a*exp(k*t_bolt)+b)/(a+b)), start = list(a = 110, b = 12.1, k = 0.8))

res.ajuste_bolt <- summary(ajuste_bolt)
a_hat <- res.ajuste_bolt$parameters[1,1]
b_hat <- res.ajuste_bolt$parameters[2,1]
k_hat <- res.ajuste_bolt$parameters[3,1]

bolt <- c(a_hat, b_hat, k_hat)

plot(t_bolt,r, col="blue",xlim=c(0,11),ylim=c(0,110), pch=19, xlab="Tiempo (s)",ylab="Posición (m)", main="Bolt")
points(t_bolt,predict(ajuste_bolt,t_bolt), pch=4, lwd=2,col="red")

nombre="estimaciones-bolt.pdf"
pdf(nombre, bg='transparent', width = 6, height = 4.5)
plot(t_bolt,r, col="blue",xlim=c(0,11),ylim=c(0,110), pch=19, xlab="Tiempo (s)",ylab="Posición (m)", main="Bolt")
points(t_bolt,(a/k)*log((a+b*exp(-k*t_bolt))/(a+b))+(b/k)*log((a*exp(k*t_bolt)+b)/(a+b))
       , pch=4, lwd=2,col="red")
points(t_bolt,predict(ajuste_bolt,t_bolt), pch=4, lwd=2,col="orange")
legend(7,40, c("real","estimada (Gómez, et al.)","estimada"), col=c("blue", "red", "orange"), 
       pch=c(19,4,4), cex=0.8,bty="n")
dev.off()

########
# GAY  #
########
# Tiempo de Gay (corredor #2) cada 20 metros
t_gay <- as.vector(t(df[2,2:7]))

# Ajuste
ajuste_gay <- nlsLM(r ~ (a/k)*log((a+b*exp(-k*t_gay))/(a+b))+(b/k)*log((a*exp(k*t_gay)+b)/(a+b)), start = list(a = 50, b = 12, k = 0.8))

res.ajuste_gay <- summary(ajuste_gay)
a_hat <- res.ajuste_gay$parameters[1,1]
b_hat <- res.ajuste_gay$parameters[2,1]
k_hat <- res.ajuste_gay$parameters[3,1]

gay <- c(a_hat, b_hat, k_hat)

plot(t_gay,r, col="blue",xlim=c(0,11),ylim=c(0,110), pch=19, xlab="Tiempo (s)",ylab="Posición (m)", main="Gay")
points(t_gay,predict(ajuste_gay,t_gay), pch=4, lwd=2,col="red")


nombre="estimaciones-gay.pdf"
pdf(nombre, bg='transparent', width = 6, height = 4.5)
plot(t_gay,r, col="blue",xlim=c(0,11),ylim=c(0,110), pch=19, xlab="Tiempo (s)",ylab="Posición (m)", main="Gay")
points(t_gay,predict(ajuste_gay,t_gay), pch=4, lwd=2,col="orange")
legend(7,40, c("real","estimada"), col=c("blue", "orange"), 
       pch=c(19,4,4), cex=0.8,bty="n")
dev.off()

##########
# POWELL #
##########
# Tiempo de Powell (corredor #3) cada 20 metros
t_powell <- as.vector(t(df[3,2:7]))

# Ajuste
ajuste_powell <- nlsLM(r ~ (a/k)*log((a+b*exp(-k*t_powell))/(a+b))+(b/k)*log((a*exp(k*t_powell)+b)/(a+b)), start = list(a = 50, b = 12, k = 0.8))

res.ajuste_powell <- summary(ajuste_powell)
a_hat <- res.ajuste_powell$parameters[1,1]
b_hat <- res.ajuste_powell$parameters[2,1]
k_hat <- res.ajuste_powell$parameters[3,1]

powell <- c(a_hat, b_hat, k_hat)

plot(t_powell,r, col="blue",xlim=c(0,11),ylim=c(0,110), pch=19, xlab="Tiempo (s)",ylab="Posición (m)", main="Powell")
points(t_powell,predict(ajuste_powell,t_powell), pch=4, lwd=2,col="red")

nombre="estimaciones-powell.pdf"
pdf(nombre, bg='transparent', width = 6, height = 4.5)
plot(t_powell,r, col="blue",xlim=c(0,11),ylim=c(0,110), pch=19, xlab="Tiempo (s)",ylab="Posición (m)", main="Powell")
points(t_powell,predict(ajuste_powell,t_powell), pch=4, lwd=2,col="orange")
legend(7,40, c("real","estimada"), col=c("blue", "orange"), 
       pch=c(19,4,4), cex=0.8,bty="n")
dev.off()

##########
# PATTON #
##########
# Tiempo de Patton (corredor #8) cada 20 metros
t_patton <- as.vector(t(df[8,2:7]))

# Ajuste
ajuste_patton <- nlsLM(r ~ (a/k)*log((a+b*exp(-k*t_patton))/(a+b))+(b/k)*log((a*exp(k*t_patton)+b)/(a+b)), start = list(a = 50, b = 12, k = 0.8))

res.ajuste_patton <- summary(ajuste_patton)
a_hat <- res.ajuste_patton$parameters[1,1]
b_hat <- res.ajuste_patton$parameters[2,1]
k_hat <- res.ajuste_patton$parameters[3,1]

patton <- c(a_hat, b_hat, k_hat)
a_hat* b_hat

round(patton,2)
plot(t_patton,r, col="blue",xlim=c(0,11),ylim=c(0,110), pch=19, xlab="Tiempo (s)",ylab="Posición (m)", main="Patton")
points(t_patton,predict(ajuste_patton,t_patton), pch=4, lwd=2,col="red")


nombre="estimaciones-patton.pdf"
pdf(nombre, bg='transparent', width = 6, height = 4.5)
plot(t_patton,r, col="blue",xlim=c(0,11),ylim=c(0,110), pch=19, xlab="Tiempo (s)",ylab="Posición (m)", main="Patton")
points(t_patton,predict(ajuste_patton,t_patton), pch=4, lwd=2,col="orange")
legend(7,40, c("real","estimada"), col=c("blue", "orange"), 
       pch=c(19,4,4), cex=0.8,bty="n")
dev.off()


coeficientes <- rbind(bolt, gay, powell, patton)
colnames(coeficientes) <- c("a","b","k")
coeficientes

nombre="predicciones-vs-reales-corredores.pdf"
pdf(nombre, bg='transparent', width = 10, height = 9)
par(mfrow=c(2,2))
plot(t_bolt,r, col="grey",xlim=c(0,11),ylim=c(0,110), pch=19, xlab="Tiempo (s)",
     ylab="Posición (m)", main="Bolt",cex.axis=0.8,family = "NimbusSan")
points(t_bolt,predict(ajuste_bolt,t_bolt), pch=4, lwd=2,col="blue")

plot(t_gay,r, col="grey",xlim=c(0,11),ylim=c(0,110), pch=19, xlab="Tiempo (s)",
     ylab="Posición (m)", main="Gay",cex.axis=0.8,family = "NimbusSan")
points(t_gay,predict(ajuste_gay,t_gay), pch=4, lwd=2,col="red")

plot(t_powell,r, col="grey",xlim=c(0,11),ylim=c(0,110), pch=19, xlab="Tiempo (s)",
     ylab="Posición (m)", main="Powell",cex.axis=0.8,family = "NimbusSan")
points(t_powell,predict(ajuste_powell,t_powell), pch=4, lwd=2,col="darkgreen")

plot(t_patton,r, col="grey",xlim=c(0,11),ylim=c(0,110), pch=19, xlab="Tiempo (s)",
     ylab="Posición (m)", main="Patton",cex.axis=0.8,family = "NimbusSan")
points(t_patton,predict(ajuste_patton,t_patton), pch=4, lwd=2,col="orange")
par(mfrow=c(1,1))
dev.off()

nombre="estimaciones-corredores.pdf"
pdf(nombre, bg='transparent', width = 10, height = 4.5)
plot(t_bolt,predict(ajuste_bolt,t_bolt), col="blue",xlim=c(0,11),ylim=c(0,110), 
     pch=1, lwd=2, xlab="Tiempo (s)",ylab="Posición (m)", main="Estimaciones",cex.axis=0.8,
     family = "NimbusSan")
points(t_gay,predict(ajuste_gay,t_gay), pch=1, lwd=2,col="red")
points(t_powell,predict(ajuste_powell,t_powell), pch=1, lwd=2,col="darkgreen")
points(t_patton,predict(ajuste_patton,t_patton), pch=1, lwd=2,col="orange")
abline(h=0, lwd=1, col="grey", lty="dashed")
abline(h=20, lwd=1, col="grey", lty="dashed")
abline(h=40, lwd=1, col="grey", lty="dashed")
abline(h=60, lwd=1, col="grey", lty="dashed")
abline(h=80, lwd=1, col="grey", lty="dashed")
abline(h=100, lwd=1, col="grey", lty="dashed")
legend(6,16, c("Bolt","Gay","Powell","Patton"), col=c("blue", "red", "darkgreen", "orange"), 
       pch=1, cex=0.8, horiz=TRUE, bty="n")
dev.off()

coeficientes
