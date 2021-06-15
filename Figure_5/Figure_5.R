
rm(list=ls())

library(ggplot2)
library(readstata13)

protest <- read.dta13("../Data/pred_prot.dta")
no_protest <- read.dta13("../Data/pred_noprot.dta")
protest <- protest[,-1]
no_protest <- no_protest[,-1]

protest <- apply(protest, 2, as.numeric)
no_protest <- apply(no_protest, 2, as.numeric)

protest_means <- apply(protest, 1, mean, na.rm=T)
no_protest_means <- apply(no_protest, 1, mean, na.rm=T)

plot(protest_means, type="l", col="blue", bty="l")
lines(no_protest_means, col="red")

pred_data = data.frame(cbind(protest_means,no_protest_means, seq.int(1:length(protest_means))))
pred_data = pred_data[168:1750,]
colnames(pred_data) = c("preds_prot", "preds_noprot", "hour")

ggplot(aes(x=hour), data=pred_data) +
  geom_smooth(aes(y=preds_prot, colour="Protest (130 events)"), se=T, size=0.5) +
  geom_smooth(aes(y=preds_noprot, colour="No Protest (133 non-events)"), se=T, size=0.5) +
  theme_classic() + geom_vline(xintercept=1440, linetype="dashed", color = "maroon") +
  geom_vline(xintercept=1272, linetype="dashed", color = "grey") +
  annotate("text", x=1540, y=0.012, label=expression(italic("Day of protest")), size=3, color="maroon") +
  annotate("text", x=1330, y=0.012, label=expression(italic("1 week")), size=3, color="grey") +
  labs(title="",x="2.5-month period (in days)", y="Probability of Protest", color="") +
  scale_x_discrete(limits=c(169, 720, 1440, 2160), labels=c("0", "30", "60", "90")) +
  theme(axis.text=element_text(size=12),
        axis.title.y=element_text(size=12,face = "bold", margin = margin(r = 10)),
        axis.title.x=element_text(size=12,face = "bold", margin = margin(t = 10))) +
  theme(plot.title = element_text(size=18, face="bold"),
        legend.position = c(0.25, 0.95)) +
  scale_color_manual(values=c("Red", "Blue")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))

ggsave("../Figure_5/Figure_5.pdf")



