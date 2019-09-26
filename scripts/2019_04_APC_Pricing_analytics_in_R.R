## ---- include=FALSE------------------------------------------------------
# overall knitr options
knitr::opts_chunk$set(cache = FALSE, echo = TRUE, warning = FALSE, message = FALSE)

## ------------------------------------------------------------------------
path <- file.path('C:/Users/u0043788/Dropbox/APC Module Data Science/computer labs/pricing analytics')

## ---- eval=TRUE----------------------------------------------------------
packages <- c("data.table", "dplyr", "mgcv", "evtree", "classInt", "rgdal", "RColorBrewer", "ggplot2", "ggmap", "grid", "gridExtra")
suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))

## ------------------------------------------------------------------------
path.MTPL <- file.path(path, "P&Cdata.txt")
path.MTPL
DT <- read.table(path.MTPL, header = TRUE)
DT <- as.data.frame(DT)

## ---- eval=FALSE---------------------------------------------------------
## str(DT)
## names(DT)
## summary(DT)
## head(DT)

## ------------------------------------------------------------------------
DT.sev <- DT[DT$AMOUNT>0 & DT$AVG<=81000, ]

## ------------------------------------------------------------------------
mean(DT$NCLAIMS)
sum(DT$NCLAIMS)/sum(DT$EXP)
# using the pipe operator
library(dplyr)
DT %>% summarize(tot_claims = sum(NCLAIMS)) 
DT %>% summarize(emp_freq = sum(NCLAIMS) / sum(EXP)) 

## ------------------------------------------------------------------------
m <- sum(DT$NCLAIMS)/sum(DT$EXP)
m
var <- sum((DT$NCLAIMS - m*DT$EXP)^2)/sum(DT$EXP)
var

## ------------------------------------------------------------------------
table(DT$NCLAIMS)
prop.table(table(DT$NCLAIMS))

## ------------------------------------------------------------------------
library("colorspace")
library("ggplot2")
g <- ggplot(data=DT, aes(NCLAIMS)) + theme_bw()
g + geom_bar()

## ------------------------------------------------------------------------
g <- ggplot(data=DT, aes(NCLAIMS)) + theme_bw()
g + geom_bar(aes(weight = EXP))

## ------------------------------------------------------------------------
g <- ggplot(data=DT, aes(NCLAIMS)) + theme_bw()
g + geom_bar(aes(y = (..count..)/sum(..count..))) + labs(y="Relative frequency")

## ------------------------------------------------------------------------
col <- "#003366"
fill <- "#99CCFF"
ylab <- "Relative frequency"

## ------------------------------------------------------------------------
g <- ggplot(data=DT, aes(NCLAIMS)) + theme_bw()
g + geom_bar(col=col, fill=fill)

## ------------------------------------------------------------------------
g <- ggplot(data=DT, aes(AGEPH)) + theme_bw()
g + geom_histogram(binwidth=2, col=col, fill=fill)

## ------------------------------------------------------------------------
g <- ggplot(data = DT, aes(AGEPH)) + theme_bw()
g + geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth=2, col=col, fill=fill) + labs(y="Relative frequency")

## ------------------------------------------------------------------------
ggplot.bar <- function(DT,variable,xlab){
  ggplot(data=DT, aes(as.factor(variable)), environment = environment()) + theme_bw() + 
    geom_bar(aes(y = (..count..)/sum(..count..)), col = col, fill = fill) + labs(x = xlab, y = ylab)
}

ggplot.hist <- function(DT,variable,xlab,binwidth){
  ggplot(data=DT, aes(variable), environment=environment()) + theme_bw() + 
    geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = binwidth, col = col, fill = fill) + 
    labs(x = xlab, y = ylab)
}

## ------------------------------------------------------------------------
# Frequency, exposure and total severity
plot.eda.nclaims <- ggplot.bar(DT, variable = DT$NCLAIMS, "nclaims")
plot.eda.exp <- ggplot.hist(DT, DT$EXP, "exp", 0.05)
plot.eda.amount <- ggplot(data = DT.sev, aes(AMOUNT)) + geom_density(adjust = 3, col = col, fill = fill) + xlim(0,1e4) + ylab(ylab) + xlab("amount") + theme_bw()

# Bar plots of factor variables
plot.eda.coverage <- ggplot.bar(DT, DT$COVERAGE, "coverage")
plot.eda.fuel <- ggplot.bar(DT, DT$FUEL, "fuel")
plot.eda.sex <- ggplot.bar(DT, DT$SEX, "sex")
plot.eda.use <- ggplot.bar(DT, DT$USE, "use")
plot.eda.fleet <- ggplot.bar(DT, DT$FLEET, "fleet")

# Histograms of continuous variables
plot.eda.ageph <- ggplot.hist(DT, DT$AGEPH, "ageph", 2)
plot.eda.agec <- ggplot.hist(DT, DT$AGEC, "agec", 1)
plot.eda.bm <- ggplot.bar(DT, DT$BM, "bm")
plot.eda.power <- ggplot.hist(DT, DT$POWER, "power", 10)

# Putting these together
library(grid)
library(gridExtra)
grid.arrange(plot.eda.nclaims, plot.eda.exp, plot.eda.amount, plot.eda.coverage, plot.eda.fuel, plot.eda.sex, plot.eda.use, plot.eda.fleet, plot.eda.ageph, plot.eda.power, plot.eda.agec, plot.eda.bm, ncol = 4)

## ------------------------------------------------------------------------
plot.eda.agephpower <- ggplot(DT, aes(x = AGEPH, y = POWER)) + stat_density2d(aes(fill = ..level..), geom="polygon") 
plot.eda.agephpower <- plot.eda.agephpower + scale_fill_gradient("Density", low="#99CCFF", high="#003366") 
plot.eda.agephpower <- plot.eda.agephpower + theme_bw() + xlab("ageph") + ylab("power")
plot.eda.agephpower

## ------------------------------------------------------------------------
DT_PC <- aggregate(EXP ~ PC, data=DT, sum)
DT_PC$N <- DT_PC$EXP
head(DT_PC)

## ------------------------------------------------------------------------
library(rgdal)
setwd('C://Users/u0043788/Dropbox/APC Module Data Science/computer labs/pricing analytics')
readShapefile = function(){
  belgium_shape <- readOGR(dsn = path.expand(paste(getwd(),"/Shape file Belgie postcodes",sep="")), layer = "npc96_region_Project1")
  belgium_shape <- spTransform(belgium_shape, CRS("+proj=longlat +datum=WGS84"))
  belgium_shape$id <- row.names(belgium_shape)
  return(belgium_shape)
}

belgium_shape = readShapefile()
str(belgium_shape@data)

## ------------------------------------------------------------------------
belgium_shape@data <- merge(belgium_shape@data, DT_PC, by.x = "POSTCODE", by.y = "PC", all.x = TRUE)
belgium_shape@data$freq <- belgium_shape@data$N/belgium_shape@data$Shape_Area
belgium_shape@data$freq_class <- cut(belgium_shape@data$freq, breaks = quantile(belgium_shape@data$freq, c(0,0.2,0.8,1), na.rm=TRUE), right = FALSE, include.lowest = TRUE, labels = c("low","average","high")) 
belgium_shape_f <- fortify(belgium_shape)
belgium_shape_f <- merge(belgium_shape_f, belgium_shape@data, all.x = TRUE)

## ------------------------------------------------------------------------
library(ggmap)
plot.eda.map <- ggplot(belgium_shape_f, aes(long, lat, group = group)) + geom_polygon(aes(fill = belgium_shape_f$freq_class), colour = "black", size=0.1)
plot.eda.map <- plot.eda.map + theme_bw() + labs(fill = "Relative\nfrequency") + scale_fill_brewer(palette = "Blues", na.value = "white")
plot.eda.map

## ------------------------------------------------------------------------
DT %>% group_by(AGEPH) %>% 
  summarize(emp_freq = sum(NCLAIMS) / sum(EXP)) 

## ------------------------------------------------------------------------
DT %>% group_by(AGEPH) %>% 
  summarize(emp_freq = sum(NCLAIMS) / sum(EXP)) %>% 
  ggplot(aes(x = AGEPH, y = emp_freq)) + theme_bw() +
  geom_point(color = "#003366")

## ------------------------------------------------------------------------
glm.freq1 <- glm(NCLAIMS ~ FUEL, offset = log(EXP), data = DT, family = poisson(link = "log"))
summary(glm.freq1)

## ------------------------------------------------------------------------
library(mgcv)
gam.freq1 <- gam(NCLAIMS ~ FUEL, offset = log(EXP), data = DT, family = poisson(link = "log"))
summary(gam.freq1)

## ------------------------------------------------------------------------
gam.freq2 <- gam(NCLAIMS ~ s(AGEPH), offset = log(EXP), data = DT, family = poisson(link = "log"))
summary(gam.freq2)

## ------------------------------------------------------------------------
pred <- predict(gam.freq2, type = "terms", se = TRUE)
str(pred)

## ------------------------------------------------------------------------
b <- pred$fit[,1]
l <- pred$fit[,1] - qnorm(0.975) * pred$se.fit[,1]
u <- pred$fit[,1] + qnorm(0.975) * pred$se.fit[,1]
x <- DT$AGEPH
df <- unique(data.frame(x, b, l, u))

## ------------------------------------------------------------------------
p <- ggplot(df, aes(x = x))
p <- p + geom_line(aes(y = b), size = 1, col="#003366")
p <- p + geom_line(aes(y = l), size = 0.5, linetype = 2, col="#99CCFF")
p <- p + geom_line(aes(y = u), size = 0.5, linetype = 2, col="#99CCFF")
p <- p + xlab("ageph") + ylab(expression(hat(f)(ageph))) + theme_bw()
p

## ------------------------------------------------------------------------
ggplot.gam <- function(model,variable,gam_term,xlabel,ylabel){
  pred <- predict(model, type = "terms", se = TRUE)
  col_index <- which(colnames(pred$fit)==gam_term)
  x <- variable
  b <- pred$fit[, col_index]
  l <- pred$fit[, col_index] - qnorm(0.975) * pred$se.fit[, col_index]
  u <- pred$fit[, col_index] + qnorm(0.975) * pred$se.fit[, col_index]
  df <- unique(data.frame(x, b, l, u))
  p <- ggplot(df, aes(x = x))
  p <- p + geom_line(aes(y = b), size = 1,col="#003366")
  p <- p + geom_line(aes(y = l), size = 0.5, linetype = 2,col="#99CCFF")
  p <- p + geom_line(aes(y = u), size = 0.5, linetype = 2,col="#99CCFF")
  p <- p + xlab(xlabel) + ylab(ylabel) + theme_bw()
  p
}

plot.gam.freq.ageph <- ggplot.gam(gam.freq2, DT$AGEPH, "s(AGEPH)", "ageph", expression(hat(f)(ageph)))
plot.gam.freq.ageph

## ------------------------------------------------------------------------
gam.freq3 <- gam(NCLAIMS ~ s(AGEPH) + s(POWER) + ti(AGEPH, POWER, bs = "tp"), offset = log(EXP), data = DT, family = poisson(link = "log"))
summary(gam.freq3)

## ------------------------------------------------------------------------
plot.gam.freq.ageph <- ggplot.gam(gam.freq3, DT$AGEPH, "s(AGEPH)", "ageph", expression(hat(f)(ageph)))
plot.gam.freq.ageph
plot.gam.freq.power <- ggplot.gam(gam.freq3, DT$POWER, "s(POWER)", "power", expression(hat(f)(power)))
plot.gam.freq.power

## ------------------------------------------------------------------------
getExtendedAgephPower <- function(){
  ageph <- seq(min(DT$AGEPH), max(DT$AGEPH))
  power <- seq(min(DT$POWER), max(DT$POWER))
  agephpower <- expand.grid(ageph, power)
  DText_agephpower <- data.frame("AGEPH" = agephpower$Var1, "POWER" = agephpower$Var2)
  return(DText_agephpower)
}
DText_agephpower <- getExtendedAgephPower()

## ------------------------------------------------------------------------
pred <- predict(gam.freq3, DText_agephpower, type = "terms", terms = "ti(AGEPH,POWER)")
GAMext.freq.AGEPHPOWER <- data.frame(DText_agephpower$AGEPH, DText_agephpower$POWER,pred)
names(GAMext.freq.AGEPHPOWER) <- c("ageph","power","s")

plot.gam.freq.agephpower <- ggplot(data=GAMext.freq.AGEPHPOWER, aes(ageph, power, z = s)) + geom_raster(aes(fill = s)) + theme_bw()
plot.gam.freq.agephpower <- plot.gam.freq.agephpower + scale_fill_gradient(expression(hat(f)(ageph, power)), low="#99CCFF", high="#003366") 
plot.gam.freq.agephpower <- plot.gam.freq.agephpower + stat_contour(breaks = seq(-0.7, 0.7, 0.05), lty = 2, colour = "white") + stat_contour(breaks = 0, lty = 1, colour = "white")
plot.gam.freq.agephpower

## ------------------------------------------------------------------------
gam.freq4 <- gam(NCLAIMS ~ s(LONG,LAT), offset = log(EXP), data = DT, family = poisson(link = "log"))
summary(gam.freq4)

## ------------------------------------------------------------------------
belgium_shape = readShapefile()
str(belgium_shape@data)
str(coordinates(belgium_shape))
postcode_DT <- data.frame(PC = belgium_shape@data$POSTCODE, LONG = coordinates(belgium_shape)[,1], LAT = coordinates(belgium_shape)[,2])

## ------------------------------------------------------------------------
pred <- predict(gam.freq4, newdata = postcode_DT, type = "terms", terms = "s(LONG,LAT)")
DT_pred <- data.frame(PC = postcode_DT$PC, LONG = postcode_DT$LONG, LAT = postcode_DT$LAT, pred)
names(DT_pred)[4] <- "s(LONG,LAT)"

## ------------------------------------------------------------------------
belgium_shape@data <- merge(belgium_shape@data, DT_pred, by.x = "POSTCODE", by.y = "PC", all.x = TRUE)
belgium_shape_f <- fortify(belgium_shape)
belgium_shape_f <- merge(belgium_shape_f, belgium_shape@data, by = "id", all.x = TRUE)

plot.gam.freq.map <- ggplot(belgium_shape_f, aes(long, lat, group = group)) + geom_polygon(aes(fill = belgium_shape_f$`s(LONG,LAT)`))
plot.gam.freq.map <- plot.gam.freq.map + theme_bw() + scale_fill_gradient(low="#99CCFF",high="#003366") + labs(fill = expression(hat(f)(long,lat)))
plot.gam.freq.map 

## ------------------------------------------------------------------------
gam.freq <- gam(NCLAIMS  ~  COVERAGE + FUEL + s(AGEPH) + s(BM) + s(POWER) + s(LONG,LAT) + ti(AGEPH,POWER,bs="tp"), offset = log(EXP), data = DT, family = poisson(link = "log"))
summary(gam.freq)

## ------------------------------------------------------------------------
plot.gam.freq.ageph <- ggplot.gam(gam.freq,DT$AGEPH, "s(AGEPH)", "ageph", expression(hat(f)[1](ageph)))
plot.gam.freq.power <- ggplot.gam(gam.freq,DT$POWER, "s(POWER)", "power", expression(hat(f)[2](power)))
plot.gam.freq.bm <- ggplot.gam(gam.freq, DT$BM, "s(BM)", "bm", expression(hat(f)[3](bm)))

## ------------------------------------------------------------------------
DText_agephpower <- getExtendedAgephPower()
DText_agephpower$COVERAGE <- DT$COVERAGE[1]
DText_agephpower$FUEL <- DT$FUEL[1]
DText_agephpower[c("BM", "LONG", "LAT","EXP")] <- c(DT$BM[1], DT$LONG[1], DT$LAT[1], DT$EXP[1])
pred <- predict(gam.freq, DText_agephpower, type = "terms", terms = "ti(AGEPH,POWER)")
GAMext.freq.AGEPHPOWER <- data.frame(DText_agephpower$AGEPH, DText_agephpower$POWER, pred)
names(GAMext.freq.AGEPHPOWER) <- c("ageph", "power", "s")

## ------------------------------------------------------------------------
plot.gam.freq.agephpower <- ggplot(data = GAMext.freq.AGEPHPOWER, aes(ageph, power, z=s)) + geom_raster(aes(fill = s)) + theme_bw()
plot.gam.freq.agephpower <- plot.gam.freq.agephpower + scale_fill_gradient(expression(hat(f)[4](ageph,power)), low = "#99CCFF", high = "#003366") 
plot.gam.freq.agephpower <- plot.gam.freq.agephpower + stat_contour(breaks = seq(-0.7, 0.7, 0.05), lty = 2, colour = "white") + stat_contour(breaks = 0, lty = 1, colour = "white")

## ------------------------------------------------------------------------
belgium_shape = readShapefile()
DT_maps <- data.frame(PC = belgium_shape@data$POSTCODE, LONG = coordinates(belgium_shape)[,1], LAT = coordinates(belgium_shape)[,2])
DT_maps$COVERAGE <- DT$COVERAGE[1]
DT_maps$FUEL <- DT$FUEL[1]
DT_maps[c("BM", "AGEPH", "POWER", "EXP")] <- c(DT$BM[1], DT$AGEPH[1], DT$POWER[1], DT$EXP[1])
pred = predict(gam.freq,newdata = DT_maps, type = "terms", terms = "s(LONG,LAT)")
DT_pred = data.frame(PC = DT_maps$PC, LONG = DT_maps$LONG, LAT = DT_maps$LAT,pred)
names(DT_pred)[4] <- "s(LONG,LAT)"
belgium_shape@data <- merge(belgium_shape@data, DT_pred, by.x = "POSTCODE", by.y = "PC", all.x = TRUE)
belgium_shape_f <- fortify(belgium_shape)
belgium_shape_f <- merge(belgium_shape_f, belgium_shape@data, by = "id", all.x=TRUE)

## ------------------------------------------------------------------------
plot.gam.freq.map <- ggplot(belgium_shape_f, aes(long, lat, group = group)) + geom_polygon(aes(fill = belgium_shape_f$`s(LONG,LAT)`))
plot.gam.freq.map <- plot.gam.freq.map + theme_bw() + scale_fill_gradient(low="#99CCFF",high="#003366") + labs(fill = expression(hat(f)[5](long,lat)))

## ------------------------------------------------------------------------
layout <- rbind(c(1,1,2,2,3,3),
             c(4,4,4,5,5,5))
grid.arrange(plot.gam.freq.ageph, plot.gam.freq.power, plot.gam.freq.bm, plot.gam.freq.agephpower, plot.gam.freq.map, layout_matrix = layout)

## ------------------------------------------------------------------------
pred <- predict(gam.freq, newdata = DT_maps, type = "terms", terms = "s(LONG,LAT)")
GAM.freq.LONGLAT = data.frame("pc" = factor(DT_maps$PC), "long" = DT_maps$LONG, "lat" = DT_maps$LAT,pred)
names(GAM.freq.LONGLAT) <- c("pc","long","lat","s")
GAM.freq.LONGLAT <- GAM.freq.LONGLAT[order(GAM.freq.LONGLAT$pc), ]
str(GAM.freq.LONGLAT)
head(GAM.freq.LONGLAT)

## ------------------------------------------------------------------------
num_bins = 5
library(classInt)
classint.fisher = classIntervals(GAM.freq.LONGLAT$s, num_bins, style = "fisher")
str(classint.fisher)
classint.fisher$brks
min(GAM.freq.LONGLAT$s)
max(GAM.freq.LONGLAT$s)

## ------------------------------------------------------------------------
crp <- colorRampPalette(c("#99CCFF","#003366"))  
plot(classint.fisher, crp(num_bins), xlab = expression(hat(f)[5](long,lat)), main = "Fisher")

## ------------------------------------------------------------------------
belgium_shape <- readShapefile()
str(belgium_shape@data)
belgium_shape@data <- merge(belgium_shape@data, GAM.freq.LONGLAT[c("pc","s")], by.x = "POSTCODE", by.y = "pc", all.x = TRUE)
belgium_shape@data$class_fisher <- cut(as.numeric(belgium_shape@data$s), breaks = classint.fisher$brks, right = FALSE, include.lowest=TRUE, dig.lab = 2) 
belgium_shape_f <- fortify(belgium_shape)
belgium_shape_f <- merge(belgium_shape_f, belgium_shape@data, by="id", all.x=TRUE)

plot.bin.map.fisher <- ggplot(belgium_shape_f, aes(long,lat, group = group)) + geom_polygon(aes(fill = belgium_shape_f$class_fisher)) + theme_bw() + labs(fill = "Fisher") + scale_fill_brewer(palette="Blues", na.value = "white") 
plot.bin.map.fisher

## ------------------------------------------------------------------------
DT.geo <- DT[c("NCLAIMS", "EXP", "COVERAGE", "FUEL", "AGEPH", "BM", "POWER", "PC")]
DT.geo <- merge(DT.geo, GAM.freq.LONGLAT, by.x = "PC", by.y = "pc", all.x = TRUE)
DT.geo$GEO <- as.factor(cut(DT.geo$s, breaks = classint.fisher$brks, right = FALSE, include.lowest=TRUE, dig.lab = 2))
head(DT.geo$GEO)

## ------------------------------------------------------------------------
gam.freq.geo <- gam(NCLAIMS ~ COVERAGE + FUEL + s(AGEPH) + s(BM) + s(POWER) + ti(AGEPH,POWER,bs="tp") +
                GEO, offset=log(EXP) , data = DT.geo, family = poisson(link = "log"))
summary(gam.freq.geo)

## ------------------------------------------------------------------------
getGAMdata_single = function(GAMmodel,term,var,varname){
  pred = predict(GAMmodel, type = "terms", terms = term)
  DT_pred = data.frame("x"=var, pred)
  DT_pred = DT_pred[order(DT_pred$x),]
  names(DT_pred) = c("x","s")
  DT_unique = unique(DT_pred)
  DT_exp <- aggregate(s ~ x, data=DT_pred, length)
  DT_exp$exp <- DT_exp$s
  DT_exp <- DT_exp[c("x","exp")]
  GAM_data = merge(DT_unique,DT_exp,by="x")
  names(GAM_data) = c(varname,"s","exp")
  GAM_data = GAM_data[which(GAM_data$exp!=0),]
  return(GAM_data)
}

## ------------------------------------------------------------------------
GAM.freq.AGEPH <- getGAMdata_single(gam.freq.geo, "s(AGEPH)", DT.geo$AGEPH, "ageph")
GAM.freq.BM <- getGAMdata_single(gam.freq.geo, "s(BM)", DT.geo$BM, "bm")
GAM.freq.POWER <- getGAMdata_single(gam.freq.geo, "s(POWER)", DT.geo$POWER, "power")
head(GAM.freq.AGEPH)

## ------------------------------------------------------------------------
getGAMdata_int = function(GAMmodel,term,var1,var2,varname1,varname2){
  pred <- predict(GAMmodel, type = "terms",terms = term)
  DT_pred <- data.frame("x"=var1,"y"=var2, pred)   
  DT_pred <- with(DT_pred, DT_pred[order(x,y),])
  names(DT_pred) = c("x","y","s")
  DT_unique = unique(DT_pred)
  DT_exp <- aggregate(s ~ x+y, data=DT_pred, length)
  DT_exp$exp <- DT_exp$s
  DT_exp <- DT_exp[c("x","y","exp")]
  GAM_data = merge(DT_unique,DT_exp,by=c("x","y"))
  names(GAM_data) = c(varname1,varname2,"s","exp")
  GAM_data = GAM_data[which(GAM_data$exp!=0),]
  return(GAM_data)
}


GAM.freq.AGEPHPOWER = getGAMdata_int(gam.freq.geo,"ti(AGEPH,POWER)",DT.geo$AGEPH, DT.geo$POWER,"ageph","power")
head(GAM.freq.AGEPHPOWER)

## ------------------------------------------------------------------------
library(evtree)
source("evtree.R")

## ------------------------------------------------------------------------
ctrl.freq = evtree.control(minbucket = 0.05*nrow(DT), alpha = 550, maxdepth = 5)

## ------------------------------------------------------------------------
evtree.freq.AGEPH <- evtree(s ~ ageph, data = GAM.freq.AGEPH, weights = exp, control = ctrl.freq)
evtree.freq.AGEPH 
plot(evtree.freq.AGEPH)

## ------------------------------------------------------------------------
evtree.freq.BM <- evtree(s ~ bm, data = GAM.freq.BM, weights = exp, control = ctrl.freq)
evtree.freq.BM
evtree.freq.POWER <- evtree(s ~ power, data = GAM.freq.POWER, weights = exp, control = ctrl.freq)
evtree.freq.POWER
evtree.freq.AGEPHPOWER <- evtree(s ~ ageph + power,data = GAM.freq.AGEPHPOWER, weights = exp, control = ctrl.freq)
evtree.freq.AGEPHPOWER

## ------------------------------------------------------------------------
splits_evtree = function(evtreemodel,GAMvar,DTvar){
  preds=predict(evtreemodel,type="node")
  nodes=data.frame("x"=GAMvar,"nodes"=preds)
  nodes$change=c(0,pmin(1,diff(nodes$nodes)))
  splits_evtree=unique(c(min(DTvar),nodes$x[which(nodes$change==1)],max(DTvar)))
  return(splits_evtree)
}

splits2D_evtree = function(evtreemodel,GAMdata,GAMdata_X,GAMdata_Y){
  pred = predict(evtreemodel,GAMdata,type="response")
  values <- data.frame("X"=GAMdata_X,"Y"=GAMdata_Y,"pred"=pred)
  min.X <- as.numeric(tapply(values$X, values$pred, min))
  min.Y <- as.numeric(tapply(values$Y, values$pred, min))
  max.X <- as.numeric(tapply(values$X, values$pred, max))
  max.Y <- as.numeric(tapply(values$Y, values$pred, max))
  splits_2D_evtree <- data.frame("xmin"=min.X,"xmax"=max.X,"ymin"=min.Y,"ymax"=max.Y)
  return(splits_2D_evtree)
}

splits.freq.AGEPH = splits_evtree(evtree.freq.AGEPH,GAM.freq.AGEPH$ageph,DT$AGEPH)
splits.freq.AGEPH
splits.freq.BM = splits_evtree(evtree.freq.BM,GAM.freq.BM$bm,DT$BM)
splits.freq.BM
splits.freq.POWER = splits_evtree(evtree.freq.POWER,GAM.freq.POWER$power,DT$POWER)
splits.freq.POWER

DText_agephpower <- getExtendedAgephPower()
DText_agephpower$COVERAGE <- DT$COVERAGE[1] 
DText_agephpower$FUEL <- DT$FUEL[1] 
DText_agephpower[c("BM", "LONG", "LAT","EXP")] <- c(DT$BM[1], DT$LONG[1], DT$LAT[1], DT$EXP[1])
pred <- predict(gam.freq,DText_agephpower,type = "terms",terms = "ti(AGEPH,POWER)")
GAMext.freq.AGEPHPOWER <- data.frame(DText_agephpower$AGEPH,DText_agephpower$POWER,pred)
names(GAMext.freq.AGEPHPOWER) <- c("ageph","power","s")

splits.freq.AGEPHPOWER = splits2D_evtree(evtree.freq.AGEPHPOWER,GAMext.freq.AGEPHPOWER,GAMext.freq.AGEPHPOWER$ageph,GAMext.freq.AGEPHPOWER$power)
splits.freq.AGEPHPOWER

plot.bin.freq.ageph = ggplot.gam(gam.freq,DT$AGEPH,"s(AGEPH)","ageph",expression(hat(f)[1](ageph))) + geom_vline(xintercept = splits.freq.AGEPH[2:(length(splits.freq.AGEPH)-1)])
plot.bin.freq.power = ggplot.gam(gam.freq,DT$POWER,"s(POWER)","power",expression(hat(f)[2](power))) + geom_vline(xintercept = splits.freq.POWER[2:(length(splits.freq.POWER)-1)])
plot.bin.freq.bm = ggplot.gam(gam.freq,DT$BM,"s(BM)","bm",expression(hat(f)[3](bm))) + geom_vline(xintercept = splits.freq.BM[2:(length(splits.freq.BM)-1)])

plot.bin.freq.agephpower <- ggplot(data=GAMext.freq.AGEPHPOWER,aes(ageph,power)) + geom_raster(aes(fill=s)) + theme_bw() +
  scale_fill_gradient(expression(hat(f)[4](ageph,power)),low="#99CCFF",high="#003366") +
  stat_contour(aes(z=s),breaks=seq(-0.7,0.7,0.05),lty=2,colour="white") + stat_contour(aes(z=s),breaks=0,lty=1,colour="white") +
  geom_segment(aes(x=xmin,y=ymin,xend=xmin,yend=ymax),data=splits.freq.AGEPHPOWER) +
  geom_segment(aes(x=xmin,y=ymin,xend=xmax,yend=ymin),data=splits.freq.AGEPHPOWER) + 
  geom_segment(aes(x=xmin,y=ymax,xend=xmax,yend=ymax),data=splits.freq.AGEPHPOWER) + 
  geom_segment(aes(x=xmax,y=ymin,xend=xmax,yend=ymax),data=splits.freq.AGEPHPOWER)

grid.arrange(plot.bin.freq.ageph,plot.bin.freq.power,plot.bin.freq.bm,plot.bin.freq.agephpower,ncol=2)

## ------------------------------------------------------------------------
DT.freq.bin <- DT.geo[c("NCLAIMS", "EXP", "COVERAGE", "FUEL", "GEO")]
DT.freq.bin$AGEPH <- cut(DT.geo$AGEPH, splits.freq.AGEPH, right = FALSE, include.lowest = TRUE)
summary(DT.freq.bin$AGEPH)
DT.freq.bin$BM <- cut(DT.geo$BM, splits.freq.BM, right = FALSE, include.lowest = TRUE)
summary(DT.freq.bin$BM)
DT.freq.bin$POWER <- cut(DT.geo$POWER, splits.freq.POWER, right = FALSE, include.lowest=TRUE)
summary(DT.freq.bin$POWER)
DT.freq.bin$AGEPHPOWER <- round(predict(evtree.freq.AGEPHPOWER, data.frame("ageph" = DT.geo$AGEPH, "power" = DT.geo$POWER), type = "response"), digits=3)
DT.freq.bin$AGEPHPOWER[abs(DT.freq.bin$AGEPHPOWER) < 0.01] <- 0
DT.freq.bin$AGEPHPOWER <- as.factor(DT.freq.bin$AGEPHPOWER)
summary(DT.freq.bin$AGEPHPOWER)

## ------------------------------------------------------------------------
summary(DT.freq.bin$COVERAGE)
summary(DT.freq.bin$FUEL)
summary(DT.freq.bin$GEO)

## ------------------------------------------------------------------------
DT.freq.bin$AGEPH <- relevel(DT.freq.bin$AGEPH, ref = "[33,51)") 
DT.freq.bin$BM <- relevel(DT.freq.bin$BM, ref = "[0,1)")
DT.freq.bin$POWER <- relevel(DT.freq.bin$POWER, ref = "[46,75)")
DT.freq.bin$AGEPHPOWER <- relevel(DT.freq.bin$AGEPHPOWER, ref = "0")
DT.freq.bin$GEO <- relevel(DT.freq.bin$GEO, ref = "[-0.036,0.11)")
DT.freq.bin$COVERAGE <- relevel(DT.freq.bin$COVERAGE, ref = "TPL")
DT.freq.bin$FUEL <- relevel(DT.freq.bin$FUEL, ref = "gasoline")

## ------------------------------------------------------------------------
glm.freq <- gam(NCLAIMS ~ COVERAGE + FUEL + AGEPH + BM + POWER + AGEPHPOWER + GEO, offset = log(EXP) , data = DT.freq.bin, family = poisson(link = "log"))

summary(glm.freq)
anova(glm.freq)

## ---- eval=FALSE---------------------------------------------------------
## library(knitr)
## setwd("C://Users/u0043788/Dropbox/APC Module Data Science/computer labs")
## file.exists("2019_04_APC_Pricing_analytics_in_R.Rmd")
## purl("2019_04_APC_Pricing_analytics_in_R.Rmd")

