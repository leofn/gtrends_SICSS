rm(list=ls())
library(plyr)
library(DataCombine)
library(tidyr)


# The code below reads in all .CSV files, names them, generates a type and puts them into a list.

# l <- list.files(getwd())
# l <- l[grepl("_noprotest", l) | grepl("_protest", l)]
# noprotest <- l[grepl("_noprotest", l)]
# protest <- l[grepl("_protest", l)]
# names <- NULL
# type <- NULL
# all_prot <- list()
# for (i in 1:length(l)) {
#   d <- read.csv(l[i])
#   d <- d[-nrow(d),]
#   d$type <- c(type, ifelse(grepl("_protest", l[i]), 1, 0))
#   all_prot[[i]] <- d
#   names <- c(names, paste(strsplit(l[i], "_")[[1]][1:4], collapse=""))
# }
# names(all_prot) <- names
# save(all_prot, file="../Data/all_prot.RData")

load("./Data/all_prot.RData")

# Get means by hour
means_list <- lapply(all_prot, function(x) apply(x[,2:ncol(x)], 1, mean, na.rm=T))

all_means_wide <- data.frame(cbind(sapply(means_list, '[', c(1:2160))))
# All in one dataset WIDE

# How do we get the DV onto a dataset with different dates?
# All protests start on day 60, so just add that?
data <- gather(all_means_wide) # from tidyr
colnames(data) <- c("city", "gt")

table(is.na(data$gt))
#There are NAs but don't remove yet

# Now let's add protest date to long
# All protests happened 60 days into the collection
# That's hour 1440
seq <- rep(c(rep(0,1440), rep(1, 24), rep(0,(nrow(all_means_wide)-1464))), length(unique(data$city)))
data$protest <- seq

data$t <- ave(data$gt, data$city, FUN = seq_along)
data$h12 <- rep(1:(nrow(all_means_wide)/12), each=12)

# Add type
types <- sapply(sapply(all_prot, "[[", "type"), unique)
data$type <- rep(types, each=nrow(all_means_wide))


library(DataCombine)
data <- slide(data, Var = "gt", GroupVar="city", slideBy = -1, NewVar="gt1")
data <- slide(data, Var = "gt", GroupVar="city", slideBy = -2, NewVar="gt2")
data <- slide(data, Var = "gt", GroupVar="city", slideBy = -3, NewVar="gt3")
data <- slide(data, Var = "gt", GroupVar="city", slideBy = -4, NewVar="gt4")
data <- slide(data, Var = "gt", GroupVar="city", slideBy = -5, NewVar="gt5")
data <- slide(data, Var = "gt", GroupVar="city", slideBy = -24, NewVar="gt24")
data <- slide(data, Var = "gt", GroupVar="city", slideBy = -48, NewVar="gt48")
data <- slide(data, Var = "gt", GroupVar="city", slideBy = -72, NewVar="gt72")
data <- slide(data, Var = "gt", GroupVar="city", slideBy = -96, NewVar="gt96")
data <- slide(data, Var = "gt", GroupVar="city", slideBy = -120, NewVar="gt120")
data <- slide(data, Var = "gt", GroupVar="city", slideBy = -144, NewVar="gt144")
data <- slide(data, Var = "gt", GroupVar="city", slideBy = -168, NewVar="gt168")

# Now let's compute the other stuff, CofV and lags

# 12h variance
data$var <- rep(ddply(data, .(city, h12), summarize, sd(gt, na.rm=T))[,3], each=12)
data$coef_var <- data$var/data$gt
table(is.infinite(data$coef_var))
data$coef_var[is.infinite(data$coef_var)] <- NA

data <- slide(data, Var = "var", GroupVar="city", slideBy = -1, NewVar="var1")
data <- slide(data, Var = "var", GroupVar="city", slideBy = -2, NewVar="var2")
data <- slide(data, Var = "var", GroupVar="city", slideBy = -3, NewVar="var3")

data <- slide(data, Var = "coef_var", GroupVar="city", slideBy = -1, NewVar="coef_var1")
data <- slide(data, Var = "coef_var", GroupVar="city", slideBy = -2, NewVar="coef_var2")
data <- slide(data, Var = "coef_var", GroupVar="city", slideBy = -3, NewVar="coef_var3")
data <- slide(data, Var = "coef_var", GroupVar="city", slideBy = -4, NewVar="coef_var4")
data <- slide(data, Var = "coef_var", GroupVar="city", slideBy = -5, NewVar="coef_var5")
data <- slide(data, Var = "coef_var", GroupVar="city", slideBy = -24, NewVar="coef_var24")
data <- slide(data, Var = "coef_var", GroupVar="city", slideBy = -48, NewVar="coef_var48")
data <- slide(data, Var = "coef_var", GroupVar="city", slideBy = -72, NewVar="coef_var72")
data <- slide(data, Var = "coef_var", GroupVar="city", slideBy = -96, NewVar="coef_var96")
data <- slide(data, Var = "coef_var", GroupVar="city", slideBy = -120, NewVar="coef_var120")
data <- slide(data, Var = "coef_var", GroupVar="city", slideBy = -144, NewVar="coef_var144")
data <- slide(data, Var = "coef_var", GroupVar="city", slideBy = -168, NewVar="coef_var168")

# Log main variable
data$lcoef_var = log(data$coef_var)
data$lcoef_var[is.infinite(data$lcoef_var)] <- NA
data$lcoef_var1 = log(data$coef_var1)
data$lcoef_var1[is.infinite(data$lcoef_var1)] <- NA
data$lcoef_var2 = log(data$coef_var2)
data$lcoef_var2[is.infinite(data$lcoef_var2)] <- NA
data$lcoef_var3 = log(data$coef_var3)
data$lcoef_var3[is.infinite(data$lcoef_var3)] <- NA
data$lcoef_var4 = log(data$coef_var4)
data$lcoef_var4[is.infinite(data$lcoef_var4)] <- NA
data$lcoef_var5 = log(data$coef_var5)
data$lcoef_var5[is.infinite(data$lcoef_var5)] <- NA
data$lcoef_var24 = log(data$coef_var24)
data$lcoef_var24[is.infinite(data$lcoef_var24)] <- NA
data$lcoef_var48 = log(data$coef_var48)
data$lcoef_var48[is.infinite(data$lcoef_var48)] <- NA
data$lcoef_var72 = log(data$coef_var72)
data$lcoef_var72[is.infinite(data$lcoef_var72)] <- NA
data$lcoef_var96 = log(data$coef_var96)
data$lcoef_var96[is.infinite(data$lcoef_var96)] <- NA
data$lcoef_var120 = log(data$coef_var120)
data$lcoef_var120[is.infinite(data$lcoef_var120)] <- NA
data$lcoef_var144 = log(data$coef_var144)
data$lcoef_var144[is.infinite(data$lcoef_var144)] <- NA
data$lcoef_var168 = log(data$coef_var168)
data$lcoef_var168[is.infinite(data$lcoef_var168)] <- NA


library(readstata13)
#save.dta13(data, "all_protests.dta")
#save(data, file = "all_protests.RData")


