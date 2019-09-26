##############################################################
##  Figure //.  Lake WA Buoy 0852 Profiles Redfield Ratios  ##
##############################################################

rm(list=ls())

setwd("~/Desktop")

NUTS <- read.csv("LakeWA_Nutrients.csv", header=TRUE)
colnames(NUTS)
NUTS$JULIAN
# View(NUTS)
xmin <- min(as.numeric(NUTS$JULIAN))-2
xmin # 43561
xmax <- max(as.numeric(NUTS$JULIAN))+2
xmax # 43768
min(na.omit(NUTS$RATIO)) #0
max(na.omit(NUTS$RATIO)) #448
colnames(NUTS)

D1Y13 <- subset(NUTS, YEAR=="2013" & DEPTH=="1")
D1Y13
D5Y13 <- subset(NUTS, YEAR=="2013" & DEPTH=="5")
D10Y13 <- subset(NUTS, YEAR=="2013" & DEPTH=="10")
D15Y13 <- subset(NUTS, YEAR=="2013" & DEPTH=="15")

D1Y14 <- subset(NUTS, YEAR=="2014" & DEPTH=="1")
D5Y14 <- subset(NUTS, YEAR=="2014" & DEPTH=="5")
D10Y14 <- subset(NUTS, YEAR=="2014" & DEPTH=="10")
D15Y14 <- subset(NUTS, YEAR=="2014" & DEPTH=="15")

D1Y15 <- subset(NUTS, YEAR=="2015" & DEPTH=="1")
D5Y15 <- subset(NUTS, YEAR=="2015" & DEPTH=="5")
D10Y15 <- subset(NUTS, YEAR=="2015" & DEPTH=="10")
D15Y15 <- subset(NUTS, YEAR=="2015" & DEPTH=="15")

D1Y16 <- subset(NUTS, YEAR=="2016" & DEPTH=="1")
D5Y16 <- subset(NUTS, YEAR=="2016" & DEPTH=="5")
D10Y16 <- subset(NUTS, YEAR=="2016" & DEPTH=="10")
D15Y16 <- subset(NUTS, YEAR=="2016" & DEPTH=="15")

D1Y17 <- subset(NUTS, YEAR=="2017" & DEPTH=="1")
D5Y17 <- subset(NUTS, YEAR=="2017" & DEPTH=="5")
D10Y17 <- subset(NUTS, YEAR=="2017" & DEPTH=="10")
D15Y17 <- subset(NUTS, YEAR=="2017" & DEPTH=="15")

D1Y18 <- subset(NUTS, YEAR=="2018" & DEPTH=="1")
D5Y18 <- subset(NUTS, YEAR=="2018" & DEPTH=="5")
D10Y18 <- subset(NUTS, YEAR=="2018" & DEPTH=="10")
D15Y18 <- subset(NUTS, YEAR=="2018" & DEPTH=="15")

# Tile 1 # 2013

par(mfrow=c(3,2))

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
par(oma=c(5, 5, 3, 3))

plot(D1Y13$JULIAN, D1Y13$RATIO, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
axis(2, ylim=c(0, 500), col="black", las=1, cex=1.6, cex.axis=1.6, cex.lab=1.6, font=1)  #las=1 makes horizontal labels
polygon(x=c(xmin, xmax, xmax, xmin), y=c(0, 0, 22, 22), col="gray", border=NA)
par(new=TRUE)
plot(D1Y13$JULIAN, D1Y13$RATIO, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
par(new=TRUE)
plot(D5Y13$JULIAN, D5Y13$RATIO, type="b", pch=16, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
par(new=TRUE)
plot(D10Y13$JULIAN, D10Y13$RATIO, type="b", pch=2, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="red")
par(new=TRUE)
plot(D15Y13$JULIAN, D15Y13$RATIO, type="b", pch=17, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="red")
legend("topright", legend="2013", text.col="black", cex=2.6, bty="n", text.font=2)
legend("topleft", legend=c("Epilimnion", "Hypolimnion", "N-limitation"), text.col=c("black", "red", "dimgray"), 
       text.font=2, bty="", cex=c(1.6, 1.6, 1.6))

# Tile 2 # 2014

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
plot(D1Y14$JULIAN, D1Y14$RATIO, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
polygon(x=c(xmin, xmax, xmax, xmin), y=c(0, 0, 22, 22), col="gray", border=NA)
par(new=TRUE)
plot(D1Y14$JULIAN, D1Y14$RATIO, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
par(new=TRUE)
plot(D5Y14$JULIAN, D5Y14$RATIO, type="b", pch=16, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
par(new=TRUE)
plot(D10Y14$JULIAN, D10Y14$RATIO, type="b", pch=2, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="red")
par(new=TRUE)
plot(D15Y14$JULIAN, D15Y14$RATIO, type="b", pch=17, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="red")
legend("topleft", legend="2014", text.col="black", cex=2.6, bty="n", text.font=2)
legend("topright", legend=c("1 m", "5 m", "10 m", "15 m"), text.col=c("black", "black", "red", "red"), 
       text.font=2, bty="", pch=c(1, 16, 2, 17), cex=c(1.6, 1.6, 1.6, 1.6), col=c("black", "black", "red", "red"))

# Tile 3 # 2015

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
plot(D1Y15$JULIAN, D1Y15$RATIO, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
mtext(expression("Nitrate" ~ ":" ~ "Phosphate"), side=2, line=4, cex=1.6, font=1)
axis(2, ylim=c(0, 500), col="black", las=1, cex=1.6, cex.axis=1.6, cex.lab=1.6, font=1)  #las=1 makes horizontal labels
polygon(x=c(xmin, xmax, xmax, xmin), y=c(0, 0, 22, 22), col="gray", border=NA)
par(new=TRUE)
plot(D1Y15$JULIAN, D1Y15$RATIO, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
par(new=TRUE)
plot(D5Y15$JULIAN, D5Y15$RATIO, type="b", pch=16, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
par(new=TRUE)
plot(D10Y15$JULIAN, D10Y15$RATIO, type="b", pch=2, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="red")
par(new=TRUE)
plot(D15Y15$JULIAN, D15Y15$RATIO, type="b", pch=17, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="red")
legend("topright", legend="2015", text.col="black", cex=2.6, bty="n", text.font=2)

# Tile 4 # 2016

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
plot(D1Y16$JULIAN, D1Y16$RATIO, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
polygon(x=c(xmin, xmax, xmax, xmin), y=c(0, 0, 22, 22), col="gray", border=NA)
par(new=TRUE)
plot(D1Y16$JULIAN, D1Y16$RATIO, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
par(new=TRUE)
plot(D5Y16$JULIAN, D5Y16$RATIO, type="b", pch=16, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
par(new=TRUE)
plot(D10Y16$JULIAN, D10Y16$RATIO, type="b", pch=2, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="red")
par(new=TRUE)
plot(D15Y16$JULIAN, D15Y16$RATIO, type="b", pch=17, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="red")
legend("topleft", legend="2016", text.col="black", cex=2.6, bty="n", text.font=2)

# Tile 5 # 2017

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
plot(D1Y17$JULIAN, D1Y17$RATIO, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
mtext(expression("Date"), side=1, line=4, cex=1.6, font=2)
axis(2, ylim=c(0, 500), col="black", las=1, cex=1.6, cex.axis=1.6, cex.lab=1.6, font=1)  #las=1 makes horizontal labels
axis(1, xlim=c(xmin, xmax), at=c(43563, 43598, 43627, 43655, 43690, 43718, 43752), labels=c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"), col="black", las=1, cex=1.6, cex.axis=1.6, cex.lab=1.6, font=1)  #las=1 makes horizontal labels
polygon(x=c(xmin, xmax, xmax, xmin), y=c(0, 0, 22, 22), col="gray", border=NA)
par(new=TRUE)
plot(D1Y17$JULIAN, D1Y17$RATIO, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
par(new=TRUE)
plot(D5Y17$JULIAN, D5Y17$RATIO, type="b", pch=16, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
par(new=TRUE)
plot(D10Y17$JULIAN, D10Y17$RATIO, type="b", pch=2, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="red")
par(new=TRUE)
plot(D15Y17$JULIAN, D15Y17$RATIO, type="b", pch=17, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="red")
legend("topright", legend="2017", text.col="black", cex=2.6, bty="n", text.font=2)

# Tile 6 # 2018

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
plot(D1Y18$JULIAN, D1Y18$RATIO, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
mtext(expression("Date"), side=1, line=4, cex=1.6, font=2)
axis(1, xlim=c(xmin, xmax), at=c(43563, 43598, 43627, 43655, 43690, 43718, 43752), labels=c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"), col="black", las=1, cex=1.6, cex.axis=1.6, cex.lab=1.6, font=1)  #las=1 makes horizontal labels
polygon(x=c(xmin, xmax, xmax, xmin), y=c(0, 0, 22, 22), col="gray", border=NA)
par(new=TRUE)
plot(D1Y18$JULIAN, D1Y18$RATIO, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
par(new=TRUE)
plot(D5Y18$JULIAN, D5Y18$RATIO, type="b", pch=16, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="black")
par(new=TRUE)
plot(D10Y18$JULIAN, D10Y18$RATIO, type="b", pch=2, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="red")
par(new=TRUE)
plot(D15Y18$JULIAN, D15Y18$RATIO, type="b", pch=17, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", font=2, las=1, cex=1.5, col="red")
legend("topleft", legend="2018", text.col="black", cex=2.6, bty="n", text.font=2)


#####################################################################
##  Figure //.  Lake WA Buoy 0852 Profiles Redfield Ratios at 1 m  ##
#####################################################################

rm(list=ls())

setwd("~/Desktop")

NUTS <- read.csv("LakeWA_Nutrients.csv", header=TRUE)
colnames(NUTS)
NUTS$JULIAN
# View(NUTS)
xmin <- min(as.numeric(NUTS$JULIAN))-2
xmin # 43561
xmax <- max(as.numeric(NUTS$JULIAN))+2
xmax # 43768
min(na.omit(NUTS$RATIO)) #0
max(na.omit(NUTS$RATIO)) #448
colnames(NUTS)

D1Y13 <- subset(NUTS, YEAR=="2013" & DEPTH=="1")
D1Y13
D5Y13 <- subset(NUTS, YEAR=="2013" & DEPTH=="5")
D10Y13 <- subset(NUTS, YEAR=="2013" & DEPTH=="10")
D15Y13 <- subset(NUTS, YEAR=="2013" & DEPTH=="15")

D1Y14 <- subset(NUTS, YEAR=="2014" & DEPTH=="1")
D5Y14 <- subset(NUTS, YEAR=="2014" & DEPTH=="5")
D10Y14 <- subset(NUTS, YEAR=="2014" & DEPTH=="10")
D15Y14 <- subset(NUTS, YEAR=="2014" & DEPTH=="15")

D1Y15 <- subset(NUTS, YEAR=="2015" & DEPTH=="1")
D5Y15 <- subset(NUTS, YEAR=="2015" & DEPTH=="5")
D10Y15 <- subset(NUTS, YEAR=="2015" & DEPTH=="10")
D15Y15 <- subset(NUTS, YEAR=="2015" & DEPTH=="15")

D1Y16 <- subset(NUTS, YEAR=="2016" & DEPTH=="1")
D5Y16 <- subset(NUTS, YEAR=="2016" & DEPTH=="5")
D10Y16 <- subset(NUTS, YEAR=="2016" & DEPTH=="10")
D15Y16 <- subset(NUTS, YEAR=="2016" & DEPTH=="15")

D1Y17 <- subset(NUTS, YEAR=="2017" & DEPTH=="1")
D5Y17 <- subset(NUTS, YEAR=="2017" & DEPTH=="5")
D10Y17 <- subset(NUTS, YEAR=="2017" & DEPTH=="10")
D15Y17 <- subset(NUTS, YEAR=="2017" & DEPTH=="15")

D1Y18 <- subset(NUTS, YEAR=="2018" & DEPTH=="1")
D5Y18 <- subset(NUTS, YEAR=="2018" & DEPTH=="5")
D10Y18 <- subset(NUTS, YEAR=="2018" & DEPTH=="10")
D15Y18 <- subset(NUTS, YEAR=="2018" & DEPTH=="15")

par(mar=c(8, 8, 1, 1)) #bottom, left, top, right

plot(D1Y13$JULIAN, D1Y13$RATIO, pch=16, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", type="b", font=2, las=1, cex=1.5, col="blue3")
mtext(expression("Epilimnetic" ~ "Nitrate" ~ ":" ~ "Phosphate"), side=2, line=4, cex=1.6, font=1)
mtext(expression("Date"), side=1, line=4, cex=1.6, font=1)
axis(2, ylim=c(0, 500), col="black", las=1, cex=1.6, cex.axis=1.6, cex.lab=1.6, font=1)  #las=1 makes horizontal labels
axis(1, xlim=c(xmin, xmax), at=c(43563, 43598, 43627, 43655, 43690, 43718, 43752), 
     labels=c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), col="black", las=1, cex=1.6, cex.axis=1.6, cex.lab=1.6, font=1)  #las=1 makes horizontal labels
polygon(x=c(xmin, xmax, xmax, xmin), y=c(0, 0, 22, 22), col="gray", border=NA)
par(new=TRUE)
plot(D1Y13$JULIAN, D1Y13$RATIO, pch=16, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", type="b", font=2, las=1, cex=1.5, col="blue3")
par(new=TRUE)
plot(D1Y14$JULIAN, D1Y14$RATIO, pch=16, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", type="b", font=2, las=1, cex=1.5, col="blueviolet")
par(new=TRUE)
plot(D1Y15$JULIAN, D1Y15$RATIO, pch=16, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", type="b", font=2, las=1, cex=1.5, col="cadetblue3")
par(new=TRUE)
plot(D1Y16$JULIAN, D1Y16$RATIO, pch=16, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", type="b", font=2, las=1, cex=1.5, col="chartreuse1")
par(new=TRUE)
plot(D1Y17$JULIAN, D1Y17$RATIO, pch=16, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", type="b", font=2, las=1, cex=1.5, col="coral1")
par(new=TRUE)
plot(D1Y18$JULIAN, D1Y18$RATIO, pch=16, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 500), xlab="", ylab="", type="b", font=2, las=1, cex=1.5, col="cyan1")
legend("topleft", legend="N-limitation", text.col="dimgray", text.font=2, bty="n", cex=1.6)
legend("topright", legend=c("2013", "2014", "2015", "2016", "2017", "2018"), text.col=c("blue3", "blueviolet", "cadetblue3", "chartreuse1", "coral1", "cyan1"), 
       text.font=2, bty="n", pch=c(16, 16, 16, 16, 16, 16), cex=c(1.6, 1.6, 1.6, 1.6, 1.6, 1.6), col=c("blue3", "blueviolet", "cadetblue3", "chartreuse1", "coral1", "cyan1"))


############################################################
##  Figure //.  Lake WA Buoy 0852 Profiles Cholorphyll-a  ##
############################################################

colnames(NUTS)
NUTS$JULIAN
# View(NUTS)
min(na.omit(NUTS$CHL)) #0.5
max(na.omit(NUTS$CHL)) #18.4

# Tile 1 # 2013

par(mfrow=c(3,2))

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
par(oma=c(5, 5, 3, 3))

plot(D1Y13$JULIAN, D1Y13$CHL, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="chartreuse3")
axis(2, ylim=c(0, 500), col="black", las=1, cex=1.6, cex.axis=1.6, cex.lab=1.6, font=1)  #las=1 makes horizontal labels
par(new=TRUE)
plot(D5Y13$JULIAN, D5Y13$CHL, type="b", pch=16, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="chartreuse3")
par(new=TRUE)
plot(D10Y13$JULIAN, D10Y13$CHL, type="b", pch=2, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="darkgreen")
par(new=TRUE)
plot(D15Y13$JULIAN, D15Y13$CHL, type="b", pch=17, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="darkgreen")
legend("topright", legend="2013", text.col="black", cex=2.6, bty="n", text.font=2)
legend("topleft", legend=c("Epilimnion", "Hypolimnion"), text.col=c("chartreuse3", "darkgreen"), 
       text.font=2, bty="", cex=c(1.6, 1.6))

# Tile 2 # 2014

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
plot(D1Y14$JULIAN, D1Y14$CHL, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="chartreuse3")
par(new=TRUE)
plot(D5Y14$JULIAN, D5Y14$CHL, type="b", pch=16, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="chartreuse3")
par(new=TRUE)
plot(D10Y14$JULIAN, D10Y14$CHL, type="b", pch=2, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="darkgreen")
par(new=TRUE)
plot(D15Y14$JULIAN, D15Y14$CHL, type="b", pch=17, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="darkgreen")
legend("topleft", legend="2014", text.col="black", cex=2.6, bty="n", text.font=2)
legend("topright", legend=c("1 m", "5 m", "10 m", "15 m"), text.col=c("chartreuse3", "chartreuse3", "darkgreen", "darkgreen"), 
       text.font=2, bty="", pch=c(1, 16, 2, 17), cex=c(1.6, 1.6, 1.6, 1.6), col=c("chartreuse3", "chartreuse3", "darkgreen", "darkgreen"))

# Tile 3 # 2015

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
plot(D1Y15$JULIAN, D1Y15$CHL, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="chartreuse3")
mtext(expression("Chlorophyll-a" ~ (mu*g ~ L^{-1})), side=2, line=4, cex=1.6, font=1)
axis(2, ylim=c(0, 500), col="black", las=1, cex=1.6, cex.axis=1.6, cex.lab=1.6, font=1)  #las=1 makes horizontal labels
par(new=TRUE)
plot(D5Y15$JULIAN, D5Y15$CHL, type="b", pch=16, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="chartreuse3")
par(new=TRUE)
plot(D10Y15$JULIAN, D10Y15$CHL, type="b", pch=2, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="darkgreen")
par(new=TRUE)
plot(D15Y15$JULIAN, D15Y15$CHL, type="b", pch=17, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="darkgreen")
legend("topright", legend="2015", text.col="black", cex=2.6, bty="n", text.font=2)

# Tile 4 # 2016

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
plot(D1Y16$JULIAN, D1Y16$CHL, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="chartreuse3")
par(new=TRUE)
plot(D5Y16$JULIAN, D5Y16$CHL, type="b", pch=16, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="chartreuse3")
par(new=TRUE)
plot(D10Y16$JULIAN, D10Y16$CHL, type="b", pch=2, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="darkgreen")
par(new=TRUE)
plot(D15Y16$JULIAN, D15Y16$CHL, type="b", pch=17, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="darkgreen")
legend("topleft", legend="2016", text.col="black", cex=2.6, bty="n", text.font=2)

# Tile 5 # 2017

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
plot(D1Y17$JULIAN, D1Y17$CHL, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="chartreuse3")
mtext(expression("Date"), side=1, line=4, cex=1.6, font=2)
axis(2, ylim=c(0, 20), col="black", las=1, cex=1.6, cex.axis=1.6, cex.lab=1.6, font=1)  #las=1 makes horizontal labels
axis(1, xlim=c(xmin, xmax), at=c(43563, 43598, 43627, 43655, 43690, 43718, 43752), labels=c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"), col="black", las=1, cex=1.6, cex.axis=1.6, cex.lab=1.6, font=1)  #las=1 makes horizontal labels
par(new=TRUE)
plot(D5Y17$JULIAN, D5Y17$CHL, type="b", pch=16, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="chartreuse3")
par(new=TRUE)
plot(D10Y17$JULIAN, D10Y17$CHL, type="b", pch=2, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="darkgreen")
par(new=TRUE)
plot(D15Y17$JULIAN, D15Y17$CHL, type="b", pch=17, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="darkgreen")
legend("topright", legend="2017", text.col="black", cex=2.6, bty="n", text.font=2)

# Tile 6 # 2018

par(mar=c(2, 2, 0, 0)) # Bottom, left, top, right
plot(D1Y18$JULIAN, D1Y18$CHL, type="b", pch=1, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="chartreuse3")
mtext(expression("Date"), side=1, line=4, cex=1.6, font=2)
axis(1, xlim=c(xmin, xmax), at=c(43563, 43598, 43627, 43655, 43690, 43718, 43752), labels=c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"), col="black", las=1, cex=1.6, cex.axis=1.6, cex.lab=1.6, font=1)  #las=1 makes horizontal labels
par(new=TRUE)
plot(D5Y18$JULIAN, D5Y18$CHL, type="b", pch=16, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="chartreuse3")
par(new=TRUE)
plot(D10Y18$JULIAN, D10Y18$CHL, type="b", pch=2, lty=3, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="darkgreen")
par(new=TRUE)
plot(D15Y18$JULIAN, D15Y18$CHL, type="b", pch=17, lty=1, lwd=2, axes=FALSE, xlim=c(xmin, xmax), ylim=c(0, 20), xlab="", ylab="", font=2, las=1, cex=1.5, col="darkgreen")
legend("topleft", legend="2018", text.col="black", cex=2.6, bty="n", text.font=2)

