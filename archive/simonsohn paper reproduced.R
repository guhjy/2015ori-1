m2 <- c(11.36, 6.77, 8.74)
m3 <- c(39.74, 85.74, 65.73)
m4 <- c(32.93, 20.60, 23.66)

x2 <- c(2.82, 2.75, 2.96)
x3 <- c(25.09, 24.58, 25.65)
x4 <- c(9.24, 9.54, 9.82)

n2 <- c(20, 20, 20)
n3 <- c(15, 15, 15)
n4 <- c(15, 15, 15)

se2 <- mean(x2) / sqrt(2 * n2)[1]
se3 <- mean(x3) / sqrt(2 * n3)[1]
se4 <- mean(x4) / sqrt(2 * n4)[1]

psi2 <- sd(x2) / se2
psi3 <- sd(x3) / se3
psi4 <- sd(x4) / se4

psi <- mean(c(psi2, psi3, psi4))

n.iter <- 100000
spsi <- NULL

for(i in 1:n.iter){
  sx2 <- c(sd(rnorm(n2[1], m2[1], mean(x2))),
           sd(rnorm(n2[2], m2[2], mean(x2))),
           sd(rnorm(n2[3], m2[3], mean(x2))))
  
  sx3 <- c(sd(rnorm(n3[1], m3[1], mean(x3))),
           sd(rnorm(n3[2], m3[2], mean(x3))),
           sd(rnorm(n3[3], m3[3], mean(x3))))
  
  sx4 <- c(sd(rnorm(n4[1], m4[1], mean(x4))),
           sd(rnorm(n4[2], m4[2], mean(x4))),
           sd(rnorm(n4[3], m4[3], mean(x4))))
  
  se2 <- mean(sx2) / sqrt(2 * n2)[1]
  se3 <- mean(sx3) / sqrt(2 * n3)[1]
  se4 <- mean(sx4) / sqrt(2 * n4)[1]
  
  spsi2 <- sd(sx2) / se2
  spsi3 <- sd(sx3) / se3
  spsi4 <- sd(sx4) / se4
  
  spsi[i] <- mean(c(spsi2, spsi3, spsi4))}

sum(spsi <= psi) / n.iter