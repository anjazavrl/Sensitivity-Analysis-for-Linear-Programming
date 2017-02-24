library(boot)
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

rb1 <- seq(0, 30, 0.05) 
sb1 <- sapply(rb1, function(x) problem2(b1 = x))

db11 = rbind(data.frame(b1 = rb1, var = "x1", val = sb1["x1",]),
             data.frame(b1 = rb1, var = "x2", val = sb1["x2",]),
             data.frame(b1 = rb1, var = "x3", val = sb1["x3",]),
             data.frame(b1 = rb1, var = "x4", val = sb1["x4",])) #tabela vrednosti, ki bo v pomoc za risanje grafov

db12 = data.frame(rb1, sb1["value.b",]) #tabela za graf optimalne vrednosti

#Grafi za resitve x,y,z in w
grafb1xyzw <- ggplot() + 
  geom_point(data = db11, aes(x = b1, y = val, colour = var))+
  ggtitle("Vpliv koeficienta b1 na rešitve x, y, z in w")+
  xlab("Koeficient b1") + ylab("Nova rešitev") +
  scale_colour_discrete(name  = "Rešitvi",
                        breaks = c("x1", "x2", "x3", "x4"),
                        labels = c("x", "y", "z", "w"))

ggsave(filename="grafb1xyzw.pdf", plot=grafb1xyzw)

#Graf za optimalno vrednost
grafb1opti <- ggplot() + 
  geom_point(data = db12, aes(x = rb1, y = sb1["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta b1 na optimalno vrednost")+
  xlab("Koeficient b1") + ylab("Optimalna vrednost")

ggsave(filename="grafb1opti.pdf", plot=grafb1opti)

#sprememba koeficienta b2 = 7

rb2 <- seq(0, 15, 0.05) 
sb2 <- sapply(rb2, function(x) problem2(b2 = x))

db21 = rbind(data.frame(b2 = rb2, var = "x1", val = sb2["x1",]),
             data.frame(b2 = rb2, var = "x2", val = sb2["x2",]),
             data.frame(b2 = rb2, var = "x3", val = sb2["x3",]),
             data.frame(b2 = rb2, var = "x4", val = sb2["x4",]))
db22 = data.frame(rb2, sb2["value.b",])

#Grafi za resitve x,y,z in w
grafb2xyzw <- ggplot() + 
  geom_point(data = db21, aes(x = b2, y = val, colour = var))+
  ggtitle("Vpliv koeficienta b2 na rešitve x, y, z in w")+
  xlab("Koeficient b2") + ylab("Nova rešitev") +
  scale_colour_discrete(name  = "Rešitvi",
                        breaks = c("x1", "x2", "x3", "x4"),
                        labels = c("x", "y", "z", "w"))

ggsave(filename="grafb2xyzw.pdf", plot=grafb2xyzw)

#Graf za optimalno vrednost
grafb2opti <- ggplot() + 
  geom_point(data = db22, aes(x = rb2, y = sb2["value.b",]), size=2, colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta b2 na optimalno vrednost")+
  xlab("Koeficient b2") + ylab("Optimalna vrednost")

ggsave(filename="grafb2opti.pdf", plot=grafb2opti)

#sprememba koeficienta b3 = 10

rb3 <- seq(7, 15, 0.05) 
sb3 <- sapply(rb3, function(x) problem2(b3 = x))

db31 = rbind(data.frame(b3 = rb3, var = "x1", val = sb3["x1",]),
             data.frame(b3 = rb3, var = "x2", val = sb3["x2",]),
             data.frame(b3 = rb3, var = "x3", val = sb3["x3",]),
             data.frame(b3 = rb3, var = "x4", val = sb3["x4",]))
db32 = data.frame(rb3, sb3["value.b",])

#Graf za resitve x,y,z in w

grafb3xyzw <- ggplot() + 
  geom_point(data = db31, aes(x = b3, y = val, colour = var))+
  ggtitle("Vpliv koeficienta b3 na rešitve x, y, z in w")+
  xlab("Koeficient b3") + ylab("Nova rešitev") +
  scale_colour_discrete(name  = "Rešitvi",
                        breaks = c("x1", "x2", "x3", "x4"),
                        labels = c("x", "y", "z", "w"))

ggsave(filename="grafb3xyzw.pdf", plot=grafb3xyzw)

#Graf za optimalno vrednost
grafb3opti <- ggplot() + 
  geom_point(data = db32, aes(x = rb3, y = sb3["value.b",]), size=2, colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta b3 na optimalno vrednost")+
  xlab("Koeficient b3") + ylab("Optimalna vrednost")

ggsave(filename="grafb3opti.pdf", plot=grafb3opti)

#sprememba koeficienta c1 = 2

rc1 <- seq(0, 15, 0.05) 
sc1 <- sapply(rc1, function(x) problem2(c1 = x))

dc11 = rbind(data.frame(c1 = rc1, var = "x1", val = sc1["x1",]),
             data.frame(c1 = rc1, var = "x2", val = sc1["x2",]),
             data.frame(c1 = rc1, var = "x3", val = sc1["x3",]),
             data.frame(c1 = rc1, var = "x4", val = sc1["x4",]))
dc12 = data.frame(rc1, sc1["value.b",])

#Graf za resitve x,y,z in w
grafc1xyzw <- ggplot() + 
  geom_point(data = dc11, aes(x = c1, y = val, colour = var))+
  ggtitle("Vpliv koeficienta c1 na rešitve x, y, z in w")+
  xlab("Koeficient c1") + ylab("Nova rešitev") +
  scale_colour_discrete(name  = "Rešitvi",
                        breaks = c("x1", "x2", "x3", "x4"),
                        labels = c("x", "y", "z", "w"))

ggsave(filename="grafc1xyzw.pdf", plot=grafc1xyzw)

#Graf za optimalno vrednost
grafc1opti <- ggplot() + 
  geom_point(data = dc12, aes(x = rc1, y = sc1["value.b",]), size=2, colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c1 na optimalno vrednost")+
  xlab("Koeficient c1") + ylab("Optimalna vrednost")

ggsave(filename="grafc1opti.pdf", plot=grafc1opti)

#sprememba koeficienta c2 = 4

rc2 <- seq(0, 5, 0.05) 
sc2 <- sapply(rc2, function(x) problem2(c2 = x))

dc21 = rbind(data.frame(c2 = rc2, var = "x1", val = sc2["x1",]),
             data.frame(c2 = rc2, var = "x2", val = sc2["x2",]),
             data.frame(c2 = rc2, var = "x3", val = sc2["x3",]),
             data.frame(c2 = rc2, var = "x4", val = sc2["x4",]))
dc22 = data.frame(rc2, sc2["value.b",])

#Graf za resitve x,y,z in w
grafc2xyzw <- ggplot() + 
  geom_point(data = dc21, aes(x = c2, y = val, colour = var))+
  ggtitle("Vpliv koeficienta c2 na rešitve x, y, z in w")+
  xlab("Koeficient c2") + ylab("Nova rešitev") +
  scale_colour_discrete(name  = "Rešitvi",
                        breaks = c("x1", "x2", "x3", "x4"),
                        labels = c("x", "y", "z", "w"))

ggsave(filename="grafc2xyzw.pdf", plot=grafc2xyzw)

#Graf za optimalno vrednost

grafc2opti <- ggplot() + 
  geom_point(data = dc22, aes(x = rc2, y = sc2["value.b",]), colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c2 na optimalno vrednost")+
  xlab("Koeficient c2") + ylab("Optimalna vrednost")

ggsave(filename="grafc2opti.pdf", plot=grafc2opti)

#sprememba koeficienta c3 = 3

rc3 <- seq(0, 20, 0.05) 
sc3 <- sapply(rc3, function(x) problem2(c3 = x))

dc31 = rbind(data.frame(c3 = rc3, var = "x1", val = sc3["x1",]),
             data.frame(c3 = rc3, var = "x2", val = sc3["x2",]),
             data.frame(c3 = rc3, var = "x3", val = sc3["x3",]),
             data.frame(c3 = rc3, var = "x4", val = sc3["x4",]))
dc32 = data.frame(rc3, sc3["value.b",])

#Graf za resitve x,y,z in w
grafc3xyzw <- ggplot() + 
  geom_point(data = dc31, aes(x = c3, y = val, colour = var))+
  ggtitle("Vpliv koeficienta c3 na rešitev x, y, z in w")+
  xlab("Koeficient c3") + ylab("Nova rešitev") +
  scale_colour_discrete(name  = "Rešitvi",
                        breaks = c("x1", "x2", "x3", "x4"),
                        labels = c("x", "y", "z", "w"))

ggsave(filename="grafc3xyzw.pdf", plot=grafc3xyzw)

#Graf za optimalno vrednost
grafc3opti <- ggplot() + 
  geom_point(data = dc32, aes(x = rc3, y = sc3["value.b",]), size = 2, colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c3 na optimalno vrednost")+
  xlab("Koeficient c3") + ylab("Optimalna vrednost")

ggsave(filename="grafc3opti.pdf", plot=grafc3opti)

#sprememba koeficienta c4 = 1

rc4 <- seq(0, 20, 0.05) 
sc4 <- sapply(rc4, function(x) problem2(c4 = x))

dc41 = rbind(data.frame(c4 = rc4, var = "x1", val = sc4["x1",]),
             data.frame(c4 = rc4, var = "x2", val = sc4["x2",]),
             data.frame(c4 = rc4, var = "x3", val = sc4["x3",]),
             data.frame(c4 = rc4, var = "x4", val = sc4["x4",]))
dc42 = data.frame(rc4, sc4["value.b",])

#Graf za resitve x,y,z in w
grafc4xyzw <- ggplot() + 
  geom_point(data = dc41, aes(x = c4, y = val, colour = var))+
  ggtitle("Vpliv koeficienta c4 na rešitve x, y, z in w")+
  xlab("Koeficient c4") + ylab("Nova rešitev") +
  scale_colour_discrete(name  = "Rešitvi",
                        breaks = c("x1", "x2", "x3", "x4"),
                        labels = c("x", "y", "z", "w"))

ggsave(filename="grafc4xyzw.pdf", plot=grafc4xyzw) 

#Graf za optimalno vrednost
grafc4opti <- ggplot() + 
  geom_point(data = dc42, aes(x = rc4, y = sc4["value.b",]), size = 2, colour = "mediumpurple1")+
  ggtitle("Vpliv koeficienta c4 na optimalno vrednost")+
  xlab("Koeficient c4") + ylab("Optimalna vrednost")

ggsave(filename="grafc4opti.pdf", plot=grafc4opti)

