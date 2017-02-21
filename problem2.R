library(boot)
library(lpSolve)
library(lpSolveAPI)
library(ggplot2)

#2. primer
#funkcija, ki sprejme linearni program in vrne rešitev s pomočjo simpleksne metode
problem2 <- function(a11 = 3, a12 = 1, a13 = 1, a14 = 4,
                     a21 = 1, a22 = -3, a23 = 2, a24 = 3,
                     a31 = 2, a32 = 1, a33 = 3, a34 = -1,
                     b1 = 12, b2 = 7, b3 = 10,
                     c1 = 2, c2 = 4, c3 = 3, c4 = 1) {
  sol <- simplex(a = c(c1, c2, c3, c4), A1 = matrix(c(a11, a21, a31, a12, a22, a32, a13, a23, a33, a14, a24, a34), ncol = 4),
                 b1 = c(b1, b2, b3), maxi = TRUE)
  c(value = sol$value, sol$soln) #zanima nas samo optimalna vrednost funkcije in rešitve
}

r <- problem2() 


#sprememba koeficienta b1 = 12

rb1 <- seq(0, 30, 0.01) 
sb1 <- sapply(rb1, function(x) problem2(b1 = x))

db1 = data.frame(rb1, sb1["x1",], sb1["x2",], sb1["x3",], sb1["x4",], sb1["value.b",]) #tabela vrednosti, ki bo v pomoc za risanje grafov

#Grafi za resitve x,y,z in w
ggplot() + 
  geom_point(data = db1, aes(x = rb1, y = sb1["x1",]), colour = "coral1")+
  ggtitle("Vpliv koeficienta b1 na rešitev x")+
  xlab("Koeficient b1") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = db1, aes(x = rb1, y = sb1["x2",]), colour = "deepskyblue3")+
  ggtitle("Vpliv koeficienta b1 na rešitev y")+
  xlab("Koeficient b1") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = db1, aes(x = rb1, y = sb1["x3",]), colour = "darkolivegreen2")+
  ggtitle("Vpliv koeficienta b1 na rešitev z")+
  xlab("Koeficient b1") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = db1, aes(x = rb1, y = sb1["x4",]), colour = "darkorchid3")+
  ggtitle("Vpliv koeficienta b1 na rešitev w")+
  xlab("Koeficient b1") + ylab("Nova rešitev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = db1, aes(x = rb1, y = sb1["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta b1 na optimalno vrednost")+
  xlab("Koeficient b1") + ylab("Optimalna vrednost")




#sprememba koeficienta b2 = 7

rb2 <- seq(0, 15, 0.01) 
sb2 <- sapply(rb2, function(x) problem2(b2 = x))

db2 = data.frame(rb2, sb2["x1",], sb2["x2",], sb2["x3",], sb2["x4",], sb2["value.b",]) #tabela vrednosti, ki bo v pomoc za risanje grafov

#Grafi za resitve x,y,z in w
ggplot() + 
  geom_point(data = db2, aes(x = rb2, y = sb2["x1",]), colour = "coral1")+
  ggtitle("Vpliv koeficienta b2 na rešitev x")+
  xlab("Koeficient b2") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = db2, aes(x = rb2, y = sb2["x2",]), colour = "deepskyblue3")+
  ggtitle("Vpliv koeficienta b2 na rešitev y")+
  xlab("Koeficient b2") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = db2, aes(x = rb2, y = sb2["x3",]), colour = "darkolivegreen2")+
  ggtitle("Vpliv koeficienta b2 na rešitev z")+
  xlab("Koeficient b2") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = db2, aes(x = rb2, y = sb2["x4",]), colour = "darkorchid3")+
  ggtitle("Vpliv koeficienta b2 na rešitev w")+
  xlab("Koeficient b2") + ylab("Nova rešitev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = db2, aes(x = rb2, y = sb2["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta b2 na optimalno vrednost")+
  xlab("Koeficient b2") + ylab("Optimalna vrednost")

#sprememba koeficienta b3 = 10

rb3 <- seq(7, 15, 0.01) 
sb3 <- sapply(rb3, function(x) problem2(b3 = x))

db3 = data.frame(rb3, sb3["x1",], sb3["x2",], sb3["x3",], sb3["x4",], sb3["value.b",]) #tabela vrednosti, ki bo v pomoc za risanje grafov

#Graf za resitve x,y,z in 

ggplot() + 
  geom_point(data = db3, aes(x = rb3, y = sb3["x1",]), colour = "coral1")+
  ggtitle("Vpliv koeficienta b3 na rešitev x")+
  xlab("Koeficient b3") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = db3, aes(x = rb3, y = sb3["x2",]), colour = "deepskyblue3")+
  ggtitle("Vpliv koeficienta b3 na rešitev y")+
  xlab("Koeficient b3") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = db3, aes(x = rb3, y = sb3["x3",]), colour = "darkolivegreen2")+
  ggtitle("Vpliv koeficienta b3 na rešitev z")+
  xlab("Koeficient b3") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = db3, aes(x = rb3, y = sb3["x4",]), colour = "darkorchid3")+
  ggtitle("Vpliv koeficienta b3 na rešitev w")+
  xlab("Koeficient b3") + ylab("Nova rešitev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = db3, aes(x = rb3, y = sb3["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta b3 na optimalno vrednost")+
  xlab("Koeficient b3") + ylab("Optimalna vrednost")

#sprememba koeficienta c1 = 2

rc1 <- seq(0, 15, 0.01) 
sc1 <- sapply(rc1, function(x) problem2(c1 = x))

dc1 = data.frame(rc1, sc1["x1",], sc1["x2",], sc1["x3",], sc1["x4",], sc1["value.b",]) #tabela vrednosti, ki bo v pomoc za risanje grafov

#Graf za resitve x,y,z in w
ggplot() + 
  geom_point(data = dc1, aes(x = rc1, y = sc1["x1",]), colour = "coral1")+
  ggtitle("Vpliv koeficienta c1 na rešitev x")+
  xlab("Koeficient c1") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = dc1, aes(x = rc1, y = sc1["x2",]), colour = "deepskyblue3")+
  ggtitle("Vpliv koeficienta c1 na rešitev y")+
  xlab("Koeficient c1") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = dc1, aes(x = rc1, y = sc1["x3",]), colour = "darkolivegreen2")+
  ggtitle("Vpliv koeficienta c1 na rešitev z")+
  xlab("Koeficient c1") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = dc1, aes(x = rc1, y = sc1["x4",]), colour = "darkorchid3")+
  ggtitle("Vpliv koeficienta c1 na rešitev w")+
  xlab("Koeficient c1") + ylab("Nova rešitev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = dc1, aes(x = rc1, y = sc1["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c1 na optimalno vrednost")+
  xlab("Koeficient c1") + ylab("Optimalna vrednost")


#sprememba koeficienta c2 = 4

rc2 <- seq(0, 5, 0.01) 
sc2 <- sapply(rc2, function(x) problem2(c2 = x))

dc2 = data.frame(rc2, sc2["x1",], sc2["x2",], sc2["x3",], sc2["x4",], sc2["value.b",]) #tabela vrednosti, ki bo v pomoc za risanje grafov

#Graf za resitve x,y,z in w
ggplot() + 
  geom_point(data = dc2, aes(x = rc2, y = sc2["x1",]), colour = "coral1")+
  ggtitle("Vpliv koeficienta c2 na rešitev x")+
  xlab("Koeficient c2") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = dc2, aes(x = rc2, y = sc2["x2",]), colour = "deepskyblue3")+
  ggtitle("Vpliv koeficienta c2 na rešitev y")+
  xlab("Koeficient c2") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = dc2, aes(x = rc2, y = sc2["x3",]), colour = "darkolivegreen2")+
  ggtitle("Vpliv koeficienta c2 na rešitev z")+
  xlab("Koeficient c2") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = dc2, aes(x = rc2, y = sc2["x4",]), colour = "darkorchid3")+
  ggtitle("Vpliv koeficienta c2 na rešitev w")+
  xlab("Koeficient c2") + ylab("Nova rešitev")


#Graf za optimalno vrednost

ggplot() + 
  geom_point(data = dc2, aes(x = rc2, y = sc2["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c2 na optimalno vrednost")+
  xlab("Koeficient c2") + ylab("Optimalna vrednost")


#sprememba koeficienta c3 = 3

rc3 <- seq(0, 20, 0.01) 
sc3 <- sapply(rc3, function(x) problem2(c3 = x))

dc3 = data.frame(rc3, sc3["x1",], sc3["x2",], sc3["x3",], sc3["x4",], sc3["value.b",]) #tabela vrednosti, ki bo v pomoc za risanje grafov

#Graf za resitve x,y,z in w
ggplot() + 
  geom_point(data = dc3, aes(x = rc3, y = sc3["x1",]), colour = "coral1")+
  ggtitle("Vpliv koeficienta c3 na rešitev x")+
  xlab("Koeficient c3") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = dc3, aes(x = rc3, y = sc3["x2",]), colour = "deepskyblue3")+
  ggtitle("Vpliv koeficienta c3 na rešitev y")+
  xlab("Koeficient c3") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = dc3, aes(x = rc3, y = sc3["x3",]), colour = "darkolivegreen2")+
  ggtitle("Vpliv koeficienta c3 na rešitev z")+
  xlab("Koeficient c3") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = dc3, aes(x = rc3, y = sc3["x4",]), colour = "darkorchid3")+
  ggtitle("Vpliv koeficienta c3 na rešitev w")+
  xlab("Koeficient c3") + ylab("Nova rešitev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = dc3, aes(x = rc3, y = sc3["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c3 na optimalno vrednost")+
  xlab("Koeficient c3") + ylab("Optimalna vrednost")


#sprememba koeficienta c4 = 1

rc4 <- seq(0, 20, 0.01) 
sc4 <- sapply(rc4, function(x) problem2(c4 = x))

dc4 = data.frame(rc4, sc4["x1",], sc4["x2",], sc4["x3",], sc4["x4",], sc4["value.b",]) #tabela vrednosti, ki bo v pomoc za risanje grafov

#Graf za resitve x,y,z in w
ggplot() + 
  geom_point(data = dc4, aes(x = rc4, y = sc4["x1",]), colour = "coral1")+
  ggtitle("Vpliv koeficienta c4 na rešitev x")+
  xlab("Koeficient c4") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = dc4, aes(x = rc4, y = sc4["x2",]), colour = "deepskyblue3")+
  ggtitle("Vpliv koeficienta c4 na rešitev y")+
  xlab("Koeficient c4") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = dc4, aes(x = rc4, y = sc4["x3",]), colour = "darkolivegreen2")+
  ggtitle("Vpliv koeficienta c4 na rešitev z")+
  xlab("Koeficient c4") + ylab("Nova rešitev")

ggplot() + 
  geom_point(data = dc4, aes(x = rc4, y = sc4["x4",]), colour = "darkorchid3")+
  ggtitle("Vpliv koeficienta c4 na rešitev w")+
  xlab("Koeficient c4") + ylab("Nova rešitev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = dc4, aes(x = rc4, y = sc4["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c4 na optimalno vrednost")+
  xlab("Koeficient c4") + ylab("Optimalna vrednost")
