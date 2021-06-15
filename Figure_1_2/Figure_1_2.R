rm(list=ls())

d <- read.csv("../Figure_1_2/fig1a2.csv")
d1 <- read.csv("../Figure_1_2/fig1b2.csv")
d2 <- read.csv("../Figure_1_2/fig1c2.csv")


pdf("../Figure_1_2/Figure_1.pdf", width = 10, height = 5)
par(mar=c(5,5,3,2))
plot(x=NULL, y=NULL, xlim = c(0,150), ylim = c(0,100), cex.axis=1.5,
     xaxt='n', xlab="Date", ylab="GT Index", bty='l', cex.lab=1.5,
     main="Searches from April 20-27, 2015")
lines(d2$protest, lwd=3, col="blue")
axis(1, at=c(12,60, 104, 145), 
     labels = c("Apr 21", "Apr 23", "Apr 25", "Apr 27"),
     cex.axis=1.5)
dev.off()


pdf("../Figure_1_2/Figure_2.pdf", width = 7.5, height = 10)
par(mfrow=c(3,1), oma=c(2,2,2,2), mar=c(5,5,5,5))
plot(x=NULL, y=NULL, xlim = c(0,170), ylim = c(0,100), cex.axis=1.5,
     xaxt='n', xlab="Date", ylab="GT Index", bty='l', cex.lab=1.5, 
     main="(a) Searches from April 13-21, 2015")
lines(d$protest, lwd=2, col="blue")
axis(1, at=c(1,40, 80, 120, 160), 
     labels = c("Apr 13", "Apr 15", "Apr 17", "Apr 19", "Apr 21"), 
     cex.axis=1.5)

plot(x=NULL, y=NULL, xlim = c(0,145), ylim = c(0,100), cex.axis=1.5,
     xaxt='n', xlab="Date", ylab="GT Index", bty='l', cex.lab=1.5,
     main="(b) Searches from April 17-24, 2015")
lines(d1$protest, lwd=2, col="blue")
axis(1, at=c(12,60, 104, 145), 
     labels = c("Apr 18", "Apr 20", "Apr 22", "Apr 24"),
     cex.axis=1.5)

plot(x=NULL, y=NULL, xlim = c(0,150), ylim = c(0,100), cex.axis=1.5,
     xaxt='n', xlab="Date", ylab="GT Index", bty='l', cex.lab=1.5,
     main="(c) Searches from April 20-27, 2015")
lines(d2$protest, lwd=2, col="blue")
axis(1, at=c(12,60, 104, 145), 
     labels = c("Apr 21", "Apr 23", "Apr 25", "Apr 27"),
     cex.axis=1.5)
dev.off()








