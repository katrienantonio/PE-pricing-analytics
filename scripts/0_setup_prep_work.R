## ----setup-------------------------------------------------------------------------
library(tidyverse)
library(rgdal)
library(sf)
library(tmap)
library(rgeos)
library(mapview)
library(leaflet)
library(mgcv)
library(rstudioapi)
library(gridExtra)

KULbg <- "#116E8A"

## ---- indeeddotcom, echo = F, fig.align = 'center', fig.width = 10, fig.height = 4.5, dev = "svg"----
# The popularity data (by Katrien on Jan 12, 2020 via indeed.com)
pop_df <- 
  data.frame(
    lang = c("SQL", "Python", "R", "SAS", "Matlab", "SPSS", "Stata"),
    n_jobs = c(80329, 71894, 51865, 24355, 11710, 3497, 1874),
    free = c(T, T, T, F, F, F, F)
  )
## Plot it
pop_df %>% 
  mutate(lang = lang %>% factor(ordered = T)) %>%
  ggplot(aes(x = lang, y = n_jobs, fill = free)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  aes(x = reorder(lang, -n_jobs), fill = reorder(free, -free)) +
  xlab("Statistical language") +
  scale_y_continuous(label = scales::comma) +
  ylab("Number of jobs") +
  labs(
    title = "Comparing statistical languages",
    subtitle = "Number of job postings on Indeed.com, 2020/01/12"
  ) +
  scale_fill_manual(
    "Free?",
    labels = c("True", "False"),
    values = c("#116E8A", "slategray")
  ) +
  ggthemes::theme_pander(base_size = 17) +
  # theme_ipsum() +
  theme(legend.position = "bottom")

## -----------------------------------------------------------------------------------
mtcars
as_tibble(mtcars)

## -----------------------------------------------------------------------------------
diamonds %>% filter(cut == "Ideal")

## -----------------------------------------------------------------------------------
diamonds %>% mutate(price_per_carat = price/carat) %>% filter(price_per_carat > 1500)

## -----------------------------------------------------------------------------------
diamonds %>% group_by(cut) %>% summarize(price = mean(price), carat = mean(carat))

## -----------------------------------------------------------------------------------
library(ggplot2)
ggplot(data = mpg)
ggplot(mpg)


## -----------------------------------------------------------------------------------
library(ggplot2)
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point() + theme_bw()