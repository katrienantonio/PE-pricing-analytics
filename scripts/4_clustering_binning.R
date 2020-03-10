## -----------------------------------------------------------------------------------
freq_gam <- gam(nclaims  ~  
                  coverage + fuel + 
                  s(ageph) + s(bm) + 
                  s(power) + s(long, lat) +
                  ti(ageph, power, bs = "tp"), 
                offset = log(expo), 
                data = mtpl, 
                family = poisson(link = "log"))


## -----------------------------------------------------------------------------------
summary(freq_gam)


## -----------------------------------------------------------------------------------
post_dt$coverage <- mtpl$coverage[1]
post_dt$fuel <- mtpl$fuel[1]
post_dt[c("bm", "ageph", "power", "expo")] <- 
  c(mtpl$bm[1], mtpl$ageph[1], mtpl$power[1], 
    mtpl$expo[1])


## -----------------------------------------------------------------------------------
pred <- predict(freq_gam, newdata = post_dt, 
                type = "terms", terms = "s(long,lat)")
dt_pred <- tibble(pc = post_dt$POSTCODE, 
                  long = post_dt$long, 
                  lat = post_dt$lat, pred)
names(dt_pred)[4] <- "fit_spatial"


## -----------------------------------------------------------------------------------
dt_pred <- dplyr::arrange(dt_pred, pc)

## -----------------------------------------------------------------------------------
num_bins <- 5
library(classInt)
classint_fisher <- classIntervals(dt_pred$fit_spatial, num_bins, style = "fisher")


## -----------------------------------------------------------------------------------
classint_fisher$brks
min(dt_pred$fit_spatial)
max(dt_pred$fit_spatial)

## -----------------------------------------------------------------------------------
crp <- colorRampPalette(c("#99CCFF", "#003366"))  
plot(classint_fisher, crp(num_bins), 
     xlab = expression(hat(f)(long,lat)), 
     main = "Fisher")


## -----------------------------------------------------------------------------------
belgium_shape_sf <- left_join(belgium_shape_sf, 
                              dt_pred, 
                              by = c("POSTCODE" = 
                                       "pc"))


## -----------------------------------------------------------------------------------
belgium_shape_sf$class_fisher <- 
  cut(belgium_shape_sf$fit_spatial, 
      breaks = classint_fisher$brks, 
      right = FALSE, include.lowest = TRUE, 
      dig.lab = 2) 

## -----------------------------------------------------------------------------------
ggplot(belgium_shape_sf) + theme_bw() + labs(fill = "Fisher") +
  geom_sf(aes(fill = class_fisher), colour = NA) +
  ggtitle("MTPL claim frequency data") +
  scale_fill_brewer(palette = "Blues", na.value = "white") +
  theme_bw()


## -----------------------------------------------------------------------------------
library(dplyr)
mtpl_geo <- mtpl %>% dplyr::select(nclaims, expo, coverage, fuel, ageph, bm, power, pc) 
mtpl_geo <- left_join(mtpl_geo, dt_pred)
mtpl_geo$geo <- as.factor(cut(mtpl_geo$fit_spatial, 
                              breaks = classint_fisher$brks, right = FALSE, 
                              include.lowest = TRUE, dig.lab = 2))
head(mtpl_geo$geo)


## -----------------------------------------------------------------------------------
freq_gam_geo <- gam(nclaims ~ coverage + fuel + s(ageph) + s(bm) + s(power) +
                      ti(ageph, power, bs = "tp") + geo, 
                    offset = log(expo) , data = mtpl_geo, family = poisson(link = "log"))
summary(freq_gam_geo)


## -----------------------------------------------------------------------------------
getGAMdata_single = function(model, term, var, varname){
  pred <- predict(model, type = "terms", terms = term)
  dt_pred <- tibble("x" = var, pred)
  dt_pred <- arrange(dt_pred, x)
  names(dt_pred) = c("x", "s")
  dt_unique <- unique(dt_pred)
  dt_exp <- dt_pred %>% group_by(x) %>% summarize(tot = n()) 
  dt_exp <- dt_exp[c("x", "tot")]
  GAM_data <- left_join(dt_unique, dt_exp)
  names(GAM_data) <- c(varname, "s", "tot")
  GAM_data <- GAM_data[which(GAM_data$tot != 0), ]
  return(GAM_data)
}

freq_gam_ageph <- getGAMdata_single(freq_gam_geo, "s(ageph)", mtpl_geo$ageph, "ageph")


## -----------------------------------------------------------------------------------
library(evtree)
source("./evtree.R")


## -----------------------------------------------------------------------------------
ctrl.freq <- evtree.control(
  minbucket = 0.05*nrow(mtpl), 
  alpha = 550, maxdepth = 5)

## -----------------------------------------------------------------------------------
evtree_freq_ageph <- evtree(s ~ ageph, data = freq_gam_ageph, weights = tot, control = ctrl.freq)
evtree_freq_ageph 
plot(evtree_freq_ageph) 


## -----------------------------------------------------------------------------------
splits_evtree = function(evtreemodel, GAMvar, DTvar){
  preds <- predict(evtreemodel, type = "node")
  nodes <- data.frame("x" = GAMvar, "nodes" = preds)
  nodes$change <- c(0, pmin(1, diff(nodes$nodes)))
  splits_evtree <- unique(c(min(DTvar), 
                            nodes$x[which(nodes$change==1)], 
                            max(DTvar)))
  return(splits_evtree)
}


## -----------------------------------------------------------------------------------
freq_splits_ageph <- splits_evtree(
  evtree_freq_ageph, 
  freq_gam_ageph$ageph, 
  mtpl$ageph)
freq_splits_ageph


## -----------------------------------------------------------------------------------
ggplot.gam <- function(model, variable, gam_term, 
                       xlabel, ylabel){
  pred <- predict(model, type = "terms", se = TRUE)
  col_index <- which(colnames(pred$fit)==gam_term)
  x <- variable
  b <- pred$fit[, col_index]
  l <- pred$fit[, col_index] - 
    qnorm(0.975) * pred$se.fit[, col_index]
  u <- pred$fit[, col_index] + 
    qnorm(0.975) * pred$se.fit[, col_index]
  df <- unique(data.frame(x, b, l, u))
  p <- ggplot(df, aes(x = x))
  p <- p + geom_line(aes(y = b), size = 1, 
                     col = "#003366")
  p <- p + geom_line(aes(y = l), size = 0.5, 
                     linetype = 2, col = "#99CCFF")
  p <- p + geom_line(aes(y = u), size = 0.5, 
                     linetype = 2, col = "#99CCFF")
  p <- p + xlab(xlabel) + ylab(ylabel) + theme_bw()
  p
}

## -----------------------------------------------------------------------------------
plot_freq_bin_ageph <- ggplot.gam(freq_gam_geo, 
                                  mtpl_geo$ageph, 
                                  "s(ageph)", "ageph", 
                                  expression(hat(f)(ageph))) + 
  geom_vline(xintercept = freq_splits_ageph[2:(length(freq_splits_ageph)-1)])
plot_freq_bin_ageph


## -----------------------------------------------------------------------------------
mtpl_bin <- mtpl_geo[c("nclaims", "expo", "coverage", "fuel", "geo")]
mtpl_bin$ageph <- cut(mtpl_geo$ageph, freq_splits_ageph, right = FALSE, include.lowest = TRUE)
summary(mtpl_bin$ageph)


## -----------------------------------------------------------------------------------
summary(mtpl_bin$coverage)
summary(mtpl_bin$fuel)
summary(mtpl_bin$geo)


## -----------------------------------------------------------------------------------
mtpl_bin$ageph <- relevel(mtpl_bin$ageph, ref = "[33,51)") 
mtpl_bin$geo <- relevel(mtpl_bin$geo, ref = "[-0.036,0.11)")
mtpl_bin$coverage <- relevel(mtpl_bin$coverage, ref = "TPL")
mtpl_bin$fuel <- relevel(mtpl_bin$fuel, ref = "gasoline")


## -----------------------------------------------------------------------------------
freq_glm <- gam(nclaims ~ coverage + fuel + ageph + geo, offset = log(expo), 
                data = mtpl_bin, family = poisson(link = "log"))

summary(freq_glm)


## -----------------------------------------------------------------------------------
freq_glm <- gam(nclaims ~ coverage + fuel + ageph + geo, offset = log(expo), 
                data = mtpl_bin, family = poisson(link = "log"))

anova(freq_glm)