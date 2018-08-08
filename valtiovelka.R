## Periodien määrä
T <- 100

## BKT:n kasvuvauhti
g <- 0.03

## Reaalikorko
r <- 0.02 

## BKT
Y <- rep(NA, T)
### BKT periodilla 1
Y[1] <- 200 

## Velka / BKT
d <- rep(NA, T)
### Velka periodilla 1
d[1] <- 0.7

## Perusjäämä / BKT
pb <- rep(-0.03, T) 

## BKT:n kehitys
for(i in 2:T)
    Y[i] <- (1+g)*Y[i-1]

## Velan kehitys
for(i in 2:T)
    d[i] <- d[i-1]*(1+r)/(1+g) - pb[i]

## Velkamäärä
D <- d*Y

## Perusjäämä
PB <- pb*Y

## Velka ja BKT tasoina
plot(D, type = "l", col = "red", xlab = "Aika", ylab = "Miljoonaa euroa")
lines(Y, col = "blue")
legend(1, 8000,  legend = c("Velka", "BKT"), col = c("red", "blue"), lty = 1)

## Velkasuhde
plot(100*d, type = "l", col = "red", xlab = "Aika", ylab = "Velkasuhde, %")
