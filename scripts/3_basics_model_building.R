## Regression models
library(tidyverse)
library(gridExtra)
library(mgcv)


KULbg <- "#116E8A"

## -----------------------------------------------------------------------------------
g_freq <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(aes(weight = expo), col = KULbg, 
           fill = KULbg, alpha = .5) + 
  labs(y = "Abs freq (in exposure)") +
  ggtitle("MTPL - number of claims")
g_freq

## -----------------------------------------------------------------------------------
g_sev <- ggplot(mtpl, aes(x = avg)) + theme_bw() +
  geom_histogram(bins = 30, boundary = 0, color = KULbg, fill = KULbg, alpha = .5) + 
  labs(x = "claim severity") +
  xlim(c(0, 20000))
g_sev

## -----------------------------------------------------------------------------------
freq_by_gender <- mtpl %>% 
  group_by(sex) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo)) 
freq_by_gender 

## -----------------------------------------------------------------------------------
ggplot(freq_by_gender, aes(x = sex, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", col = KULbg, fill = KULbg, alpha = .5)

## -----------------------------------------------------------------------------------
freq_glm_1 <- glm(nclaims ~ sex, offset = log(expo), 
                  family = poisson(link = "log"), 
                  data = mtpl)
freq_glm_1 %>% broom::tidy() 

## -----------------------------------------------------------------------------------
freq_glm_1 %>% broom::augment(type.predict = "response") %>% slice(1:2) %>% select(nclaims, sex, .fitted, .se.fit) 

## -----------------------------------------------------------------------------------
exp(coef(freq_glm_1)[1])
exp(coef(freq_glm_1)[1] + coef(freq_glm_1)[2])

## -----------------------------------------------------------------------------------
## Your Turn!





## -----------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------
KULbg <- "#116E8A"

# number 1
library(MASS)
bias_model <- gam(accel ~ s(times, sp = 0, k = 2), data = mcycle)
mcycle$predictions <- predict(bias_model, mcycle)
p_1 <- ggplot(mcycle, aes(times, accel)) + theme_bw() +
  geom_point(alpha = .3) +
  geom_line(aes(times, predictions), size = 1.0, color = KULbg) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) + 
  ggtitle("sp = 0 and k = 2")
# number 2
bias_model <- gam(accel ~ s(times, sp = 0, k = 5), data = mcycle)
mcycle$predictions <- predict(bias_model, mcycle)
p_2 <- ggplot(mcycle, aes(times, accel)) + theme_bw() +
  geom_point(alpha = .3) +
  geom_line(aes(times, predictions), size = 1.0, color = KULbg) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) +
  ggtitle("sp = 0 and k = 5")
# number 3
bias_model <- gam(accel ~ s(times, sp = 0, k = 55), data = mcycle)
mcycle$predictions <- predict(bias_model, mcycle)
p_3 <- ggplot(mcycle, aes(times, accel)) + theme_bw() +
  geom_point(alpha = .3) +
  geom_line(aes(times, predictions), size = 1.0, color = KULbg) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) + 
  ggtitle("sp = 0 and k = 15")
# number 4
library(MASS)
bias_model <- gam(accel ~ s(times), data = mcycle)
mcycle$predictions <- predict(bias_model, mcycle)
p_4 <- ggplot(mcycle, aes(times, accel)) + theme_bw() + 
  geom_point(alpha = .3) +
  geom_line(aes(times, predictions), size = 1.0, color = KULbg) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) + 
  ggtitle("optimal sp and default k")
# number 5
bias_model <- gam(accel ~ s(times, sp = 3), data = mcycle)
mcycle$predictions <- predict(bias_model, mcycle)
p_5 <- ggplot(mcycle, aes(times, accel)) + theme_bw() +
  geom_point(alpha = .3) +
  geom_line(aes(times, predictions), size = 1.0, color = KULbg) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) + 
  ggtitle("sp = 3 and default k")
# number 6
bias_model <- gam(accel ~ s(times, sp = 20), data = mcycle)
mcycle$predictions <- predict(bias_model, mcycle)
p_6 <- ggplot(mcycle, aes(times, accel)) + theme_bw() +
  geom_point(alpha = .3) +
  geom_line(aes(times, predictions), size = 1.0, color = KULbg) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) +
  ggtitle("sp = 10 and default k")

gridExtra::grid.arrange(p_1, p_2, p_3, p_4, p_5, p_6, nrow = 2)

## -----------------------------------------------------------------------------------
 model <- gam(accel ~ s(times, sp = 1.2,
                        k = 5, bs = "cr"),
              family = gaussian, data = mcycle)

## -----------------------------------------------------------------------------------
model <- gam(accel ~ s(times, bs = "cr"),
              method = "REML",
              family = gaussian, data = mcycle)

## -----------------------------------------------------------------------------------
print(model)

## -----------------------------------------------------------------------------------
model$sp

## -----------------------------------------------------------------------------------
plot(model, pages = 1, scheme = 0)
plot(model, pages = 1, scheme = 1)

## -----------------------------------------------------------------------------------
mtpl %>% group_by(ageph) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo)) %>% 
  ggplot(aes(x = ageph, y = emp_freq)) + theme_bw() +
  geom_point(color = KULbg)

## -----------------------------------------------------------------------------------
a <- min(mtpl$ageph):max(mtpl$ageph)

## -----------------------------------------------------------------------------------
freq_glm_age <- glm(nclaims ~ ageph, offset = log(expo), data = mtpl, family = poisson(link = "log"))
pred_glm_age <- predict(freq_glm_age, newdata = data.frame(ageph = a, expo = 1), type = "terms", se.fit = TRUE)
b_glm_age <- pred_glm_age$fit
l_glm_age <- pred_glm_age$fit - qnorm(0.975)*pred_glm_age$se.fit
u_glm_age <- pred_glm_age$fit + qnorm(0.975)*pred_glm_age$se.fit
df <- data.frame(a, b_glm_age, l_glm_age, u_glm_age)

## -----------------------------------------------------------------------------------
p_glm_age <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_glm_age <- p_glm_age + geom_line(aes(a, b_glm_age), size = 1, col = KULbg)   
p_glm_age <- p_glm_age + geom_line(aes(a, u_glm_age), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_glm_age), size = 0.5, linetype = 2, col = KULbg)
p_glm_age <- p_glm_age + xlab("ageph") + ylab("fit") + theme_bw()
p_glm_age

## -----------------------------------------------------------------------------------
freq_glm_age_f <- glm(nclaims ~ as.factor(ageph), offset = log(expo), data = mtpl, family = poisson(link = "log"))
pred_glm_age_f <- predict(freq_glm_age_f, newdata = data.frame(ageph = a, expo = 1), type = "terms", se.fit = TRUE)
b_glm_age_f <- pred_glm_age_f$fit
l_glm_age_f <- pred_glm_age_f$fit - 
  qnorm(0.975)*pred_glm_age_f$se.fit
u_glm_age_f <- pred_glm_age_f$fit + 
  qnorm(0.975)*pred_glm_age_f$se.fit
df <- data.frame(a, b_glm_age_f, 
                 l_glm_age_f, u_glm_age_f)

## -----------------------------------------------------------------------------------
p_glm_age_f <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_glm_age_f <- p_glm_age_f + geom_line(aes(a, b_glm_age_f), size = 1, col = KULbg)   
p_glm_age_f <- p_glm_age_f + geom_line(aes(a, u_glm_age_f), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_glm_age_f), size = 0.5, linetype = 2, col = KULbg)
p_glm_age_f <- p_glm_age_f + xlab("ageph") + ylab("fit") + theme_bw()
p_glm_age_f

## -----------------------------------------------------------------------------------
level <- seq(min(mtpl$ageph), max(mtpl$ageph), by = 5)
freq_glm_age_c <- glm(nclaims ~ cut(ageph, level), offset = log(expo), data = mtpl, family = poisson(link = "log"))
pred_glm_age_c <- predict(freq_glm_age_c, newdata = data.frame(ageph = a, expo = 1), type = "terms", se.fit = TRUE)
b_glm_age_c <- pred_glm_age_c$fit
l_glm_age_c <- pred_glm_age_c$fit - 
  qnorm(0.975)*pred_glm_age_c$se.fit
u_glm_age_c <- pred_glm_age_c$fit + 
  qnorm(0.975)*pred_glm_age_c$se.fit
df <- data.frame(a, b_glm_age_c, 
                 l_glm_age_c, u_glm_age_c)

## -----------------------------------------------------------------------------------
p_glm_age_c <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_glm_age_c <- p_glm_age_c + geom_line(aes(a, b_glm_age_c), size = 1, col = KULbg)   
p_glm_age_c <- p_glm_age_c + geom_line(aes(a, u_glm_age_c), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_glm_age_c), size = 0.5, linetype = 2, col = KULbg)
p_glm_age_c <- p_glm_age_c + xlab("ageph") + ylab("fit") + theme_bw()
p_glm_age_c

## -----------------------------------------------------------------------------------
freq_gam_age <- gam(nclaims ~ s(ageph), 
                    offset = log(expo), 
                    data = mtpl, 
                    family = poisson(link = "log"))
pred_gam_age <- predict(freq_gam_age, 
                        newdata = data.frame(ageph = a, expo = 1), 
                        type = "terms", se.fit = TRUE)
b_gam_age <- pred_gam_age$fit
l_gam_age <- pred_gam_age$fit -
  qnorm(0.975)*pred_gam_age$se.fit
u_gam_age <- pred_gam_age$fit +
  qnorm(0.975)*pred_gam_age$se.fit
df <- data.frame(a, b_gam_age, 
                 l_gam_age, u_gam_age)

## -----------------------------------------------------------------------------------
p_gam_age <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_gam_age <- p_gam_age + geom_line(aes(a, b_gam_age), size = 1, col = KULbg)   
p_gam_age <- p_gam_age + geom_line(aes(a, u_gam_age), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_gam_age), size = 0.5, linetype = 2, col = KULbg)
p_gam_age <- p_gam_age + xlab("ageph") + ylab("fit") + theme_bw()
p_gam_age


## -----------------------------------------------------------------------------------
## Your Turn!-------------------------------------------------------------------------




## ----Your Turn ends here------------------------------------------------------------
## -----------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------
freq_gam_spatial <- gam(nclaims ~ s(long, lat, 
                                    bs = "tp"), 
                        offset = log(expo), 
                        family = 
                          poisson(link = "log"), 
                        data = mtpl)
freq_gam_spatial$sp

## -----------------------------------------------------------------------------------
freq_gam_inter <- gam(nclaims ~ s(ageph) + s(power) + 
                        ti(ageph, power, bs = "tp"), 
                      offset = log(expo), 
                      family = poisson(link = "log"), 
                      data = mtpl)
freq_gam_inter$sp


## -----------------------------------------------------------------------------------
post_dt <- st_centroid(belgium_shape_sf)
post_dt$long <- do.call(rbind, post_dt$geometry)[,1]
post_dt$lat <- do.call(rbind, post_dt$geometry)[,2]


## -----------------------------------------------------------------------------------
pred <- predict(freq_gam_spatial, newdata = post_dt, 
                type = "terms", terms = "s(long,lat)")


## -----------------------------------------------------------------------------------
dt_pred <- data.frame(pc = post_dt$POSTCODE, 
                      long = post_dt$long, 
                      lat = post_dt$lat, pred)
names(dt_pred)[4] <- "fit_spatial"


## -----------------------------------------------------------------------------------
belgium_shape_sf <- left_join(belgium_shape_sf, 
                              dt_pred, 
                              by = c("POSTCODE" =
                                       "pc"))

## -----------------------------------------------------------------------------------
ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = fit_spatial), colour = NA) +
  ggtitle("MTPL claim frequency data") +
  scale_fill_gradient(low="#99CCFF", high="#003366") +
  theme_bw()

## -----------------------------------------------------------------------------------
tm_shape(belgium_shape_sf) + 
  tm_borders(col = 'white', lwd = .1) + 
  tm_fill("fit_spatial", style = "cont", palette = "RdBu", legend.reverse = TRUE, auto.palette.mapping = TRUE) + 
  tm_layout(legend.title.size = 1.0, legend.text.size = 1.0) 

library(dplyr)
belgium_shape_sf <- belgium_shape_sf %>% dplyr::select(-fit_spatial)


