rm(list=ls())

library(plyr)
library(ggplot2)
library(viridis) 

d <- read.csv("../Data/trends_data_Mar_to_May.csv")
d <- d[-nrow(d),] # delete last row

hourly_means <- apply(d[2:ncol(d)], 1, mean, na.rm=TRUE)
groups <- data.frame(cbind(rep(seq(1:(nrow(d)/24)), each=24), hourly_means))
mean <- ddply(groups, ~ groups[,1], summarize, mean(hourly_means, na.rm=T))
var <- ddply(groups, ~ groups[,1], summarize, sd(hourly_means, na.rm=T))
groups$mean <- rep(mean[,2], each=24)
groups$var <- rep(var[,2], each=24)
groups$id <- seq(1:nrow(groups))
day <- c(seq(1:30), seq(1:5))
coef_var <- var/mean*100


pdf("../Figure_3_4/Figure_3.pdf", width=12, height=7)
plot(groups$hourly_means, type='l', bty='l', col=magma(10)[9], lwd=0.75, 
     xaxt='n', xlab='Date', ylab='Mean Trends Index', cex.axis=1.5, cex.lab=1.5)
lines(groups$mean, type='l', bty='l', col=magma(10)[2], lwd=1.5)
lines(groups$var, type='l', bty='l', col=magma(10)[6], lwd=1.5)
axis(1, at=seq(0,2160, 200), labels = c("Mar. 1", "Mar. 9", "Mar. 17", "Mar. 26", "Apr. 4", 
                                        "Apr. 11", "Apr. 18", "Apr. 26", "May 4", "May 13", "May 22"), 
          cex.axis=1.25)
legend(1350, 106, legend=c("Hourly average", "Daily mean", "Daily St. Dev."),
       col=c(magma(10)[9], magma(10)[2], magma(10)[6]),
       cex=1, bty="n", lty=1, ncol=1)
dev.off()

# Coef Variance
data <- data.frame(unique(groups[,1]),coef_var[,2])
colnames(data) <- c("groups", "var")
ggplot(aes(x=groups, y=var), data=data) +
  geom_point(aes(y=var), colour="blue", size=3, alpha = 0.5) +
  geom_smooth(aes(y=var), se=F, colour="blue", size=0.7)+
  theme_classic() + labs(title="",x="24-hour intervals (Mar 1 - May 30, 2015)", y="Coef. of the Variance") +
  scale_x_discrete(limits=c(0, 25, 50, 75), labels=c("Mar 1", "Mar 25", "Apr 19", "May 5")) +
  theme(plot.title=element_text(size=14, face="bold", hjust=0.5, lineheight=1.2),
        axis.text=element_text(size=12), 
        axis.title.y=element_text(size=14,face = "bold", margin = margin(r = 10)),
        axis.title.x=element_text(size=12,face = "bold", margin = margin(t = 10)))

ggsave("../Figure_3_4/Figure_4.pdf")

