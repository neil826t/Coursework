Orange - 12
Brown - 2
Blue - 6
Green - 8
Yellow - 1
Red - 1

Neighbor - 2/27
Me - 2/30

beta(0.835, 4.165)

a1p <- 6.835
b1p <- 28.165
a2p <- 2.835
b2p <- 32.165

d.draw <- rbeta(1000, a1p, b1p) - rbeta(1000, a2p, b2p)
hist(d.draw)
plot(density(d.draw))


curve(dgamma(x, 1, 2), xlim = c(0, 10))
curve(dgamma(x, 2, 2), add = TRUE)
curve(dgamma(x, 3, 2), add = TRUE)
curve(dgamma(x, 4, 2),  add = TRUE)
curve(dgamma(x, 5, 2),  add = TRUE)
curve(dgamma(x, 6, 2),  add = TRUE)
curve(dgamma(x, 7, 2),  add = TRUE)
curve(dgamma(x, 8, 2),  add = TRUE)
curve(dgamma(x, 0.5, 0.1), xlim = c(0, 100))
for (i in 1:10) {
  curve(dgamma(x, i, 0.1), add = TRUE)
}
