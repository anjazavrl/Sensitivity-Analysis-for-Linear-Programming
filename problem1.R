library(boot)
library(lpSolve)
library(lpSolveAPI)
library(ggplot2)

#1. primer
#funkcija, ki sprejme linearni program in vrne resitev s pomocjo simpleksne metode
problem1 <- function(a11 = 1, a12 = 1, a21 = 2, a22 = 1,
                    b1 = 8, b2 = 10, c1 = 5000, c2 = 3000) {
  sol <- simplex(a = c(c1, c2), A1 = matrix(c(a11, a21, a12, a22), ncol = 2),
                 b1 = c(b1, b2), maxi = TRUE)
  c(value = sol$value, sol$soln) #zanima nas samo optimalna vrednost funkcije in resitve
}

r <- problem1() 



#sprememba koeficienta b1 = 8

rb1 <- seq(0, 10.5, 0.01) #koeficient b1 spreminjamo na intervalu [0, 10.5] s korakom 0.01
sb1 <- sapply(rb1, function(x) problem1(b1 = x)) #zazenemo funkcijo problem1, kjer se nam koeficient b1 spreminja

db11 = rbind(data.frame(b1 = rb1, var = "x1", val = sb1["x1",]),
            data.frame(b1 = rb1, var = "x2", val = sb1["x2",])) #definirana tabela, da se na grafu lahko loci resitve
db12 = data.frame(rb1, sb1["value.b",]) #tabela za graf optimalne vrednosti

#Graf za resitvi x in y
grafb1xy <- ggplot() + 
  geom_point(data = db11, aes(x = b1, y = val, colour = var))+
  ggtitle("Vpliv koeficienta b1 na rešitvi x in y")+
  xlab("Koeficient b1") + ylab("Nova rešitev") +
  scale_colour_discrete(name  = "Rešitvi",
                       breaks = c("x1", "x2"),
                       labels = c("x", "y"))


#Graf za optimalno vrednost
grafb1opt <- ggplot() + 
  geom_point(data = db12, aes(x = rb1, y = sb1["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta b1 na optimalno vrednost")+
  xlab("Koeficient b1") + ylab("Optimalna vrednost")

ggsave(filename = "grafb1xy.pdf", plot = grafb1xy)

ggsave(filename = "grafb1opt.pdf", plot = grafb1opt)


#sprememba koeficienta b2 = 10

rb2 <- seq(0, 20, 0.01) 
sb2 <- sapply(rb2, function(x) problem1(b2 = x))

db21 = rbind(data.frame(b2 = rb2, var = "x1", val = sb2["x1",]),
            data.frame(b2 = rb2, var = "x2", val = sb2["x2",]))
db22 = data.frame(rb2, sb2["value.b",])

#Graf za resitvi x in y
grafb2xy <- ggplot() + 
  geom_point(data = db21, aes(x = b2, y = val, colour = var))+
  ggtitle("Vpliv koeficienta b2 na rešitvi x in y")+
  xlab("Koeficient b2") + ylab("Nova rešitev") +
  scale_colour_discrete(name  = "Rešitvi",
                        breaks = c("x1", "x2"),
                        labels = c("x", "y"))

#Graf za optimalno vrednost
grafb2opt <- ggplot() + 
  geom_point(data = db22, aes(x = rb2, y = sb2["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta b2 na optimalno vrednost")+
  xlab("Koeficient b2") + ylab("Optimalna vrednost")



ggsave(filename="grafb2xy.pdf", plot=grafb2xy)

ggsave(filename="grafb2opt.pdf", plot=grafb2opt)



#sprememba koeficienta c1 = 5000

rc1 <- seq(2500, 6500, 10) 
sc1 <- sapply(rc1, function(x) problem1(c1 = x))


dc11 = rbind(data.frame(c1 = rc1, var = "x1", val = sc1["x1",]),
             data.frame(c1 = rc1, var = "x2", val = sc1["x2",]))
dc12 = data.frame(rc1, sc1["value.b",])

#Graf za resitvi x in y 
grafc1xy <- ggplot() + 
  geom_point(data = dc11, aes(x = c1, y = val, colour = var))+
  ggtitle("Vpliv koeficienta c1 na rešitvi x in y")+
  xlab("Koeficient c1") + ylab("Nova rešitev") +
  scale_colour_discrete(name  = "Rešitvi",
                        breaks = c("x1", "x2"),
                        labels = c("x", "y"))

#Graf za optimalno vrednost
grafc1opt <- ggplot() + 
  geom_point(data = dc12, aes(x = rc1, y = sc1["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c1 na optimalno vrednost")+
  xlab("Koeficient c1") + ylab("Optimalna vrednost")


ggsave(filename="grafc1xy.pdf", plot=grafc1xy)

ggsave(filename="grafc1opt.pdf", plot=grafc1opt)


#sprememba koeficienta c2 = 3000

rc2 <- seq(2000, 6500, 10) 
sc2 <- sapply(rc2, function(x) problem1(c2 = x))

dc21 = rbind(data.frame(c2 = rc2, var = "x1", val = sc2["x1",]),
             data.frame(c2 = rc2, var = "x2", val = sc2["x2",]))
dc22 = data.frame(rc2, sc2["value.b",])

#Graf za resitvi x in y
grafc2xy <- ggplot() + 
  geom_point(data = dc21, aes(x = c2, y = val, colour = var))+
  ggtitle("Vpliv koeficienta c2 na rešitvi x in y")+
  xlab("Koeficient c2") + ylab("Nova rešitev") +
  scale_colour_discrete(name  = "Rešitvi",
                        breaks = c("x1", "x2"),
                        labels = c("x", "y"))

#Graf za optimalno vrednost
grafc2opt <- ggplot() + 
  geom_point(data = dc22, aes(x = rc2, y = sc2["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c2 na optimalno vrednost")+
  xlab("Koeficient c2") + ylab("Optimalna vrednost")


ggsave(filename="grafc2xy.pdf", plot=grafc2xy)

ggsave(filename="grafc2opt.pdf", plot=grafc2opt)




#dodajanje novega pogoja 5x + 3y <= 20

novpogoj <- function(a11 = 1, a12 = 1, a21 = 2, a22 = 1, a31 = 5, a32 = 3,
                     b1 = 8, b2 = 10, b3 = 20, c1 = 5000, c2 = 3000) {
  sol <- simplex(a = c(c1, c2), A1 = matrix(c(a11, a21, a12, a22, a31, a32), nrow = 3, byrow = TRUE),
                 b1 = c(b1, b2, b3), maxi = TRUE)
  c(value = sol$value, sol$soln) #zanima nas samo optimalna vrednost funkcije in resitve
}

np <- novpogoj() 


#rešitev se spremeni iz x = 6, y = 2, p = 28000 na x = 4, y = 0, p = 20000
