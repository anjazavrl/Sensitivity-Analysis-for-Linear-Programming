library(boot)
library(lpSolve)
library(lpSolveAPI)
library(ggplot2)

#1. primer
#funkcija, ki sprejme linearni program in vrne rešitev s pomocjo simpleksne metode
problem1 <- function(a11 = 1, a12 = 1, a21 = 2, a22 = 1,
                    b1 = 8, b2 = 10, c1 = 5000, c2 = 3000) {
  sol <- simplex(a = c(c1, c2), A1 = matrix(c(a11, a21, a12, a22), ncol = 2),
                 b1 = c(b1, b2), maxi = TRUE)
  c(value = sol$value, sol$soln) #zanima nas samo optimalna vrednost funkcije in rešitve
}

r <- problem1() 




#sprememba koeficienta b1 = 8

rb1 <- seq(4.5, 10.5, 0.01) #koeficient b1 spreminjamo na intervalu [4.5, 10.5] s korakom 0.05
sb1 <- sapply(rb1, function(x) problem1(b1 = x)) #zaženemo funkcijo problem1, kjer se nam koeficient b1 spreminja

db1 = data.frame(rb1, sb1["x1",], sb1["x2",], sb1["value.b",])

#Graf za rešitvi x1 in x2 
grafb1xy <- ggplot() + 
  geom_point(data = db1, aes(x = rb1, y = sb1["x1",]), colour = "steelblue3")+
  geom_point(data = db1, aes(x = rb1, y = sb1["x2",]), colour = "deeppink1")+
  ggtitle("Vpliv koeficienta b1 na rešitvi x in y")+
  xlab("Koeficient b1") + ylab("Nova rešitev")

#Graf za optimalno vrednost
grafb1opt <- ggplot() + 
  geom_point(data = db1, aes(x = rb1, y = sb1["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta b1 na optimalno vrednost")+
  xlab("Koeficient b1") + ylab("Optimalna vrednost")

ggsave(filename="grafb1xy.pdf", plot=grafb1xy)

ggsave(filename="grafb1opt.pdf", plot=grafb1opt)


#sprememba koeficienta b2 = 10

rb2 <- seq(7, 15, 0.01) 
sb2 <- sapply(rb2, function(x) problem1(b2 = x))

db2 = data.frame(rb2, sb2["x1",], sb2["x2",], sb2["value.b",])

#Graf za rešitvi x1 in x2 
grafb2xy <- ggplot() + 
  geom_point(data = db2, aes(x = rb2, y = sb2["x1",]), colour = "steelblue3")+
  geom_point(data = db2, aes(x = rb2, y = sb2["x2",]), colour = "deeppink1")+
  ggtitle("Vpliv koeficienta b2 na rešitvi x in y")+
  xlab("Koeficient b2") + ylab("Nova rešitev")

#Graf za optimalno vrednost
grafb2opt <- ggplot() + 
  geom_point(data = db2, aes(x = rb2, y = sb2["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta b2 na optimalno vrednost")+
  xlab("Koeficient b2") + ylab("Optimalna vrednost")



ggsave(filename="grafb2xy.pdf", plot=grafb2xy)

ggsave(filename="grafb2opt.pdf", plot=grafb2opt)



#sprememba koeficienta c1 = 5000

rc1 <- seq(2500, 6500, 10) 
sc1 <- sapply(rc1, function(x) problem1(c1 = x))

dc1 = data.frame(rc1, sc1["x1",], sc1["x2",], sc1["value.b",])

#Graf za rešitvi x in y 
grafc1xy <- ggplot() + 
  geom_point(data = dc1, aes(x = rc1, y = sc1["x1",]), colour = "steelblue3")+
  geom_point(data = dc1, aes(x = rc1, y = sc1["x2",]), colour = "deeppink1")+
  ggtitle("Vpliv koeficienta c1 na rešitvi x in y")+
  xlab("Koeficient c1") + ylab("Nova rešitev")

#Graf za optimalno vrednost
grafc1opt <- ggplot() + 
  geom_point(data = dc1, aes(x = rc1, y = sc1["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c1 na optimalno vrednost")+
  xlab("Koeficient c1") + ylab("Optimalna vrednost")


ggsave(filename="grafc1xy.pdf", plot=grafc1xy)

ggsave(filename="grafc1opt.pdf", plot=grafc1opt)


#sprememba koeficienta c2 = 3000

rc2 <- seq(2000, 6500, 10) 
sc2 <- sapply(rc2, function(x) problem1(c2 = x))

dc2 = data.frame(rc2, sc2["x1",], sc2["x2",], sc2["value.b",])

#Graf za rešitvi x in y
grafc2xy <- ggplot() + 
  geom_point(data = dc2, aes(x = rc2, y = sc2["x1",]), colour = "steelblue3")+
  geom_point(data = dc2, aes(x = rc2, y = sc2["x2",]), colour = "deeppink1")+
  ggtitle("Vpliv koeficienta c2 na rešitvi x in y")+
  xlab("Koeficient c2") + ylab("Nova rešitev")

#Graf za optimalno vrednost
grafc2opt <- ggplot() + 
  geom_point(data = dc2, aes(x = rc2, y = sc2["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c2 na optimalno vrednost")+
  xlab("Koeficient c2") + ylab("Optimalna vrednost")


ggsave(filename="grafc2xy.pdf", plot=grafc2xy)

ggsave(filename="grafc2opt.pdf", plot=grafc2opt)



