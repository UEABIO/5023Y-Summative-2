# An analysis of average June temperatures on the body size of a univoltine butterfly species

# Packages====
library(tidyverse)
library(janitor)
library(car)
library(lmtest)
library(emmeans)

# Dataset====

butterfly_df <- read_csv("data/univoltine_butterfly.csv")

colnames(butterfly_df)

butterfly_df <- janitor::clean_names(butterfly_df)

nrow(butterfly_df)
# number of rows of dataframe

hist(butterfly_df$rain_jun)
# outlier detected

butterfly_df %>% 
  ggplot(aes(x = rain_jun)) +
  geom_histogram()

hist(butterfly_df$forewing_length)
# no outliers

hist(butterfly_df$jun_mean)
# no outliers

unique(butterfly_df$sex)
# typos detected

## Fix dataset===

butterfly_clean <- butterfly_df %>% 
  filter(rain_jun < 200) %>% 
  mutate(sex = case_when(sex == "Female" ~ "Females",
                             sex == "Maes" ~ "Males",
                             .default = as.character(sex)))
unique(butterfly_clean$sex)

# Exploratory Analysis

butterfly_clean %>% 
  ggplot(aes(x = rain_jun, 
             y = forewing_length,
             colour = sex))+
  geom_point()

butterfly_clean %>% 
  ggplot(aes(x = jun_mean, 
             y = forewing_length,
             colour = sex))+
  geom_point()+
  geom_smooth(method = "lm",
             se = FALSE)

# Model====

model <- lm(forewing_length ~ jun_mean + sex + rain_jun + jun_mean:sex,
            data = butterfly_clean)

## Check model fit

car::boxCox(model)
# indicates a log transformation could be suitable?

plot(model)
# look ok - check qq, residuals and linearity

bptest(model)


car::qqPlot(model) # adds a confidence interval check

shapiro.test(residuals(model))


# Test interactions
summary(model)
drop1(model, test = "F")

# Interaction term does not explain significant variance (report this)

model_2 <- lm(forewing_length ~ jun_mean + sex + rain_jun,
              data = butterfly_clean)

summary(model_2)


## Summarise model===

model_sum <- emmeans::emmeans(model_2, specs = ~jun_mean + sex,
                 at =list(jun_mean = c(11.8, 16.4))) %>% 
  as_tibble()


butterfly_clean %>% 
  ggplot(aes(x = jun_mean, 
             y = forewing_length,
             fill = sex))+
  geom_ribbon(data = model_sum,
              aes(x = jun_mean,
                  y = emmean,
                  ymin = lower.CL,
                  ymax = upper.CL,
                  fill = sex),
              alpha = .2)+
  geom_line(data = model_sum,
            aes(x = jun_mean,
                y = emmean,
                colour = sex),
            show.legend = FALSE)+
  geom_point(shape = 21,
             colour = "black",
             show.legend = FALSE)+
  scale_colour_manual(values = c("darkorange", "purple"))+
  scale_fill_manual(values = c("darkorange", "purple"))+
  labs(x = "Mean June Temperature (°C)",
       y = "Forewing length (mm)",
       fill = "Sex")+
  theme_classic()+
  theme(legend.position = "top")
  


