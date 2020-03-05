## Regression models
library(tidyverse)
library(gridExtra)
library(mgcv)


KULbg <- "#116E8A"


## ----------------------------------------------------------------------------------------------
g_freq <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(aes(weight = expo), col = KULbg, 
           fill = KULbg, alpha = .5) + 
  labs(y = "Abs freq (in exposure)") +
  ggtitle("MTPL - number of claims")
g_freq


## ----------------------------------------------------------------------------------------------
g_sev <- ggplot(mtpl, aes(x = avg)) + theme_bw() +
  geom_histogram(bins = 30, boundary = 0, color = KULbg, fill = KULbg, alpha = .5) + 
  labs(x = "claim severity") +
  xlim(c(0, 20000))
g_sev


## ---------------------------------------------------------------------------------------------
freq_by_gender <- mtpl %>% 
  group_by(sex) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo)) 
freq_by_gender 


## ----------------------------------------------------------------------------------------------
ggplot(freq_by_gender, aes(x = sex, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", col = KULbg, fill = KULbg, alpha = .5)


## ----------------------------------------------------------------------------------------------
freq_glm_1 <- glm(nclaims ~ sex, offset = log(expo), 
                  family = poisson(link = "log"), 
                  data = mtpl)
freq_glm_1 %>% broom::tidy() 


## ----------------------------------------------------------------------------------------------
freq_glm_1 %>% broom::augment(type.predict = "response") %>% slice(1:2) %>% select(nclaims, sex, .fitted, .se.fit) 


## ----------------------------------------------------------------------------------------------
exp(coef(freq_glm_1)[1])
exp(coef(freq_glm_1)[1] + coef(freq_glm_1)[2])


## ----Your Turn---------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------



## ----Your Turn ends here------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
mtpl %>% group_by(ageph) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo)) %>% 
  ggplot(aes(x = ageph, y = emp_freq)) + theme_bw() +
  geom_point(color = KULbg)


## ----------------------------------------------------------------------------------------------
a <- min(mtpl$ageph):max(mtpl$ageph)


## ----------------------------------------------------------------------------------------------
freq_glm_age <- glm(nclaims ~ ageph, offset = log(expo), data = mtpl, family = poisson(link = "log"))
pred_glm_age <- predict(freq_glm_age, newdata = data.frame(ageph = a, expo = 1), type = "terms", se.fit = TRUE)
b_glm_age <- pred_glm_age$fit
l_glm_age <- pred_glm_age$fit - qnorm(0.975)*pred_glm_age$se.fit
u_glm_age <- pred_glm_age$fit + qnorm(0.975)*pred_glm_age$se.fit
df <- data.frame(a, b_glm_age, l_glm_age, u_glm_age)


## ----------------------------------------------------------------------------------------------
p_glm_age <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_glm_age <- p_glm_age + geom_line(aes(a, b_glm_age), size = 1, col = KULbg)   
p_glm_age <- p_glm_age + geom_line(aes(a, u_glm_age), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_glm_age), size = 0.5, linetype = 2, col = KULbg)
p_glm_age <- p_glm_age + xlab("ageph") + ylab("fit") + theme_bw()
p_glm_age


## ----------------------------------------------------------------------------------------------
freq_glm_age_f <- glm(nclaims ~ as.factor(ageph), offset = log(expo), data = mtpl, family = poisson(link = "log"))
pred_glm_age_f <- predict(freq_glm_age_f, newdata = data.frame(ageph = a, expo = 1), type = "terms", se.fit = TRUE)
b_glm_age_f <- pred_glm_age_f$fit
l_glm_age_f <- pred_glm_age_f$fit - 
  qnorm(0.975)*pred_glm_age_f$se.fit
u_glm_age_f <- pred_glm_age_f$fit + 
  qnorm(0.975)*pred_glm_age_f$se.fit
df <- data.frame(a, b_glm_age_f, 
                 l_glm_age_f, u_glm_age_f)


## ----------------------------------------------------------------------------------------------
p_glm_age_f <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_glm_age_f <- p_glm_age_f + geom_line(aes(a, b_glm_age_f), size = 1, col = KULbg)   
p_glm_age_f <- p_glm_age_f + geom_line(aes(a, u_glm_age_f), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_glm_age_f), size = 0.5, linetype = 2, col = KULbg)
p_glm_age_f <- p_glm_age_f + xlab("ageph") + ylab("fit") + theme_bw()
p_glm_age_f


## ----------------------------------------------------------------------------------------------
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


## ----------------------------------------------------------------------------------------------
p_glm_age_c <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_glm_age_c <- p_glm_age_c + geom_line(aes(a, b_glm_age_c), size = 1, col = KULbg)   
p_glm_age_c <- p_glm_age_c + geom_line(aes(a, u_glm_age_c), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_glm_age_c), size = 0.5, linetype = 2, col = KULbg)
p_glm_age_c <- p_glm_age_c + xlab("ageph") + ylab("fit") + theme_bw()
p_glm_age_c


## ----------------------------------------------------------------------------------------------
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


## ----------------------------------------------------------------------------------------------
p_gam_age <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_gam_age <- p_gam_age + geom_line(aes(a, b_gam_age), size = 1, col = KULbg)   
p_gam_age <- p_gam_age + geom_line(aes(a, u_gam_age), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_gam_age), size = 0.5, linetype = 2, col = KULbg)
p_gam_age <- p_gam_age + xlab("ageph") + ylab("fit") + theme_bw()
p_gam_age


## ----Your Turn---------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------




## ----Your Turn ends here-----------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------

