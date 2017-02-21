library(boot)
library(lpSolve)
library(lpSolveAPI)
library(ggplot2)

#1. primer
#funkcija, ki sprejme linearni program in vrne re?itev s pomo?jo simpleksne metode
problem1 <- function(a11 = 1, a12 = 1, a21 = 2, a22 = 1,
                    b1 = 8, b2 = 10, c1 = 5000, c2 = 3000) {
  sol <- simplex(a = c(c1, c2), A1 = matrix(c(a11, a21, a12, a22), ncol = 2),
                 b1 = c(b1, b2), maxi = TRUE)
  c(value = sol$value, sol$soln) #zanima nas samo optimalna vrednost funkcije in re?itve
}

r <- problem1() 

#sprememba koeficienta a11 = 1


ra11 <- seq(0, 2, 0.01) #koeficient a11 spreminjamo na intervalu [0,2] s korakom 0.01, dobimo 201 razlicnih vrednosti
sa11 <- sapply(ra11, function(x) problem1(a11 = x)) #za?enemo funkcijo problem1, kjer se nam koeficient a11 spreminja

d11 = data.frame(ra11, sa11["x1",], sa11["x2",], sa11["value.b",]) #tabela vrednosti, ki bo v pomo? za risanje grafov

#Graf za re?itvi x1 in x2 
ggplot() + 
  geom_point(data = d11, aes(x = ra11, y = sa11["x1",]), colour = "steelblue3")+
  geom_point(data = d11, aes(x = ra11, y = sa11["x2",]), colour = "deeppink1")+
  ggtitle("Vpliv koeficienta a11 na re?itvi x in y")+
  xlab("Koeficient a11") + ylab("Nova re?itev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = d11, aes(x = ra11, y = sa11["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta a11 na optimalno vrednost")+
  xlab("Koeficient a11") + ylab("Optimalna vrednost")


#sprememba koeficienta a12 = 1


ra12 <- seq(0.5, 3, 0.01)
sa12 <- sapply(ra12, function(x) problem1(a12 = x)) 


d12 = data.frame(ra12, sa12["x1",], sa12["x2",], sa12["value.b",])

#Graf za re?itvi x1 in x2 
ggplot() + 
  geom_point(data = d12, aes(x = ra12, y = sa12["x1",]), colour = "steelblue3")+
  geom_point(data = d12, aes(x = ra12, y = sa12["x2",]), colour = "deeppink1")+
  ggtitle("Vpliv koeficienta a12 na re?itvi x in y")+
  xlab("Koeficient a12") + ylab("Nova re?itev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = d12, aes(x = ra12, y = sa12["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta a12 na optimalno vrednost")+
  xlab("Koeficient a12") + ylab("Optimalna vrednost")


#sprememba koeficienta a21 = 2


ra21 <- seq(1, 3, 0.01)
sa21 <- sapply(ra21, function(x) problem1(a21 = x)) 

d21 = data.frame(ra21, sa21["x1",], sa21["x2",], sa21["value.b",])

#Graf za re?itvi x1 in x2 
ggplot() + 
  geom_point(data = d21, aes(x = ra21, y = sa21["x1",]), colour = "steelblue3")+
  geom_point(data = d21, aes(x = ra21, y = sa21["x2",]), colour = "deeppink1")+
  ggtitle("Vpliv koeficienta a21 na re?itvi x in y")+
  xlab("Koeficient a21") + ylab("Nova re?itev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = d21, aes(x = ra21, y = sa21["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta a21 na optimalno vrednost")+
  xlab("Koeficient a21") + ylab("Optimalna vrednost")



#sprememba koeficienta a22 = 1


ra22 <- seq(0, 2, 0.01)
sa22 <- sapply(ra22, function(x) problem1(a22 = x)) 


d22 = data.frame(ra22, sa22["x1",], sa22["x2",], sa22["value.b",])

#Graf za re?itvi x1 in x2 
ggplot() + 
  geom_point(data = d22, aes(x = ra22, y = sa22["x1",]), colour = "steelblue3")+
  geom_point(data = d22, aes(x = ra22, y = sa22["x2",]), colour = "deeppink1")+
  ggtitle("Vpliv koeficienta a22 na re?itvi x in y")+
  xlab("Koeficient a22") + ylab("Nova re?itev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = d22, aes(x = ra22, y = sa22["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta a22 na optimalno vrednost")+
  xlab("Koeficient a22") + ylab("Optimalna vrednost")



#sprememba koeficienta b1 = 8

rb1 <- seq(4.5, 10.5, 0.01) #koeficient b1 spreminjamo na intervalu [4.5, 10.5] s korakom 0.05
sb1 <- sapply(rb1, function(x) problem1(b1 = x)) #za?enemo funkcijo problem1, kjer se nam koeficient b1 spreminja

db1 = data.frame(rb1, sb1["x1",], sb1["x2",], sb1["value.b",])

#Graf za re?itvi x1 in x2 
ggplot() + 
  geom_point(data = db1, aes(x = rb1, y = sb1["x1",]), colour = "steelblue3")+
  geom_point(data = db1, aes(x = rb1, y = sb1["x2",]), colour = "deeppink1")+
  ggtitle("Vpliv koeficienta b1 na re?itvi x in y")+
  xlab("Koeficient b1") + ylab("Nova re?itev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = db1, aes(x = rb1, y = sb1["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta b1 na optimalno vrednost")+
  xlab("Koeficient b1") + ylab("Optimalna vrednost")



#sprememba koeficienta b2 = 10

rb2 <- seq(7, 15, 0.01) 
sb2 <- sapply(rb2, function(x) problem1(b2 = x))

db2 = data.frame(rb2, sb2["x1",], sb2["x2",], sb2["value.b",])

#Graf za re?itvi x1 in x2 
ggplot() + 
  geom_point(data = db2, aes(x = rb2, y = sb2["x1",]), colour = "steelblue3")+
  geom_point(data = db2, aes(x = rb2, y = sb2["x2",]), colour = "deeppink1")+
  ggtitle("Vpliv koeficienta b2 na re?itvi x in y")+
  xlab("Koeficient b2") + ylab("Nova re?itev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = db2, aes(x = rb2, y = sb2["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta b2 na optimalno vrednost")+
  xlab("Koeficient b2") + ylab("Optimalna vrednost")


#sprememba koeficienta c1 = 5000

rc1 <- seq(2500, 6500, 10) 
sc1 <- sapply(rc1, function(x) problem1(c1 = x))

dc1 = data.frame(rc1, sc1["x1",], sc1["x2",], sc1["value.b",])

#Graf za re?itvi x1 in x2 
ggplot() + 
  geom_point(data = dc1, aes(x = rc1, y = sc1["x1",]), colour = "steelblue3")+
  geom_point(data = dc1, aes(x = rc1, y = sc1["x2",]), colour = "deeppink1")+
  ggtitle("Vpliv koeficienta c1 na re?itvi x in y")+
  xlab("Koeficient c1") + ylab("Nova re?itev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = dc1, aes(x = rc1, y = sc1["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c1 na optimalno vrednost")+
  xlab("Koeficient c1") + ylab("Optimalna vrednost")

#sprememba koeficienta c2 = 3000

rc2 <- seq(2000, 6500, 10) 
sc2 <- sapply(rc2, function(x) problem1(c2 = x))

dc2 = data.frame(rc2, sc2["x1",], sc2["x2",], sc2["value.b",])

#Graf za re?itvi x1 in x2 
ggplot() + 
  geom_point(data = dc2, aes(x = rc2, y = sc2["x1",]), colour = "steelblue3")+
  geom_point(data = dc2, aes(x = rc2, y = sc2["x2",]), colour = "deeppink1")+
  ggtitle("Vpliv koeficienta c2 na re?itvi x in y")+
  xlab("Koeficient c2") + ylab("Nova re?itev")

#Graf za optimalno vrednost
ggplot() + 
  geom_point(data = dc2, aes(x = rc2, y = sc2["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c2 na optimalno vrednost")+
  xlab("Koeficient c2") + ylab("Optimalna vrednost")





