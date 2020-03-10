## -----------------------------------------------------------------------------------
# install.packages("rstudioapi")
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
mtpl_orig <- read.table('./data/P&Cdata.txt',
                                   header = TRUE)
mtpl_orig <- as_tibble(mtpl_orig)

## -----------------------------------------------------------------------------------
mtpl_orig %>% slice(1:3) %>% select(-LONG, -LAT) 


## ----prepare-mtpl-------------------------------------------------------------------
mtpl <- mtpl_orig %>%
  # rename all columns 
  rename_all(function(.name) {
    .name %>% 
      # replace all names with the lowercase versions
      tolower 
    # replace all spaces with underscores is also useful, with `str_replace(" ", "-")`
  })
mtpl <- rename(mtpl, expo = exp)

## -----------------------------------------------------------------------------------
mean(mtpl$nclaims)
sum(mtpl$nclaims)/sum(mtpl$expo)
mtpl %>% summarize(emp_freq = sum(nclaims) / sum(expo)) 


## -----------------------------------------------------------------------------------
m <- sum(mtpl$nclaims)/sum(mtpl$expo)
m
var <- sum((mtpl$nclaims - m * mtpl$expo)^2)/sum(mtpl$expo)
var

## -----------------------------------------------------------------------------------
dim(mtpl)

## -----------------------------------------------------------------------------------
mtpl %>% summarize(emp_freq = sum(nclaims) / sum(expo)) 

## -----------------------------------------------------------------------------------
mtpl %>% 
  group_by(sex) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo))

## -----------------------------------------------------------------------------------
KULbg <- "#116E8A"
g <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(col = KULbg, fill = KULbg) + 
  labs(y = "Abs frequency") +
  ggtitle("MTPL - number of claims")
g

## -----------------------------------------------------------------------------------
KULbg <- "#116E8A"
g <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(aes(weight = expo), col = KULbg, 
           fill = KULbg) + 
  labs(y = "Abs freq (in exposure)") +
  ggtitle("MTPL - number of claims")
g

## -----------------------------------------------------------------------------------
g <- ggplot(mtpl, aes(nclaims)) + theme_bw()
g + geom_bar(aes(y = (..count..)/sum(..count..)), 
             col = KULbg, fill = KULbg) + 
  labs(y = "Relative frequency") +
  ggtitle("MTPL - relative number of claims")

## -----------------------------------------------------------------------------------
g <- ggplot(mtpl, aes(bm)) + theme_bw()
g + geom_histogram(binwidth = 1, col = KULbg, fill = KULbg, alpha = .5)

## -----------------------------------------------------------------------------------
g <- ggplot(mtpl, aes(bm)) + theme_bw()
g + geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 1, col = KULbg, fill = KULbg, alpha = 0.5) + labs(y = "Relative frequency")

## -----------------------------------------------------------------------------------
## Your Turn!





## -----------------------------------------------------------------------------------
col <- KULbg
fill <- KULbg
ylab <- "Relative frequency"

# wrapper functions
ggplot.bar <- function(DT, variable, xlab){
  ggplot(data = DT, aes(as.factor(variable))) + theme_bw() + 
    geom_bar(aes(y = (..count..)/sum(..count..)), col = col, fill = fill, alpha = 0.5) + labs(x = xlab, y = ylab)
}

ggplot.hist <- function(DT, variable, xlab, binwidth){
  ggplot(data = DT, aes(variable)) + theme_bw() + 
    geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = binwidth, col = col, fill = fill, alpha = 0.5) + 
    labs(x = xlab, y = ylab)
}

## -----------------------------------------------------------------------------------
# Targets: frequency, exposure and total severity
plot.eda.nclaims <- ggplot.bar(mtpl, variable = mtpl$nclaims, "nclaims")
plot.eda.exp <- ggplot.hist(mtpl, mtpl$expo, "expo", 0.05)
mtpl.sev <- mtpl %>% filter(amount > 0 & avg <= 81000) # see SAJ paper for motivation
plot.eda.amount <- ggplot(data = mtpl.sev, aes(avg)) + geom_density(adjust = 3, col = col, fill = fill, alpha = 0.5) + xlim(0, 1e4) + ylab(ylab) + xlab("severity") + theme_bw()

# Bar plots of factor variables
plot.eda.coverage <- ggplot.bar(mtpl, mtpl$coverage, "coverage")
plot.eda.fuel <- ggplot.bar(mtpl, mtpl$fuel, "fuel")
plot.eda.sex <- ggplot.bar(mtpl, mtpl$sex, "sex")
plot.eda.use <- ggplot.bar(mtpl, mtpl$use, "use")
plot.eda.fleet <- ggplot.bar(mtpl, mtpl$fleet, "fleet")

# Histograms of continuous variables
plot.eda.ageph <- ggplot.hist(mtpl, mtpl$ageph, "ageph", 2)
plot.eda.agec <- ggplot.hist(mtpl, mtpl$agec, "agec", 1)
plot.eda.bm <- ggplot.bar(mtpl, mtpl$bm, "bm")
plot.eda.power <- ggplot.hist(mtpl, mtpl$power, "power", 10)

# Putting these together
library(gridExtra)
grid.arrange(plot.eda.nclaims, plot.eda.exp, plot.eda.amount, plot.eda.coverage, plot.eda.fuel, plot.eda.sex, plot.eda.use, plot.eda.fleet, plot.eda.ageph, plot.eda.power, plot.eda.agec, plot.eda.bm, ncol = 4)

