#Assignment_8
library(modelr)
library(broom)
library(tidyverse)
library(fitdistrplus)

data("mtcars")
glimpse(mtcars)

mod1 = lm(mpg ~ disp, data = mtcars)
summary(mod1)

ggplot(mtcars, aes(x=disp,y=mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

mod2 = lm(mpg ~ qsec, data = mtcars)
ggplot(mtcars, aes(x=disp,y=qsec)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

mean(mod1$residuals^2)
mean(mod2$residuals^2)

df <- mtcars %>% 
  add_predictions(mod1) 
df[,c("mpg","pred")] %>% head()

# Make a new dataframe with the predictor values we want to assess
# mod1 only has "disp" as a predictor so that's what we want to add here
newdf = data.frame(disp = c(500,600,700,800,900)) 
# anything specified in the model needs to be here with exact matching column names

# making predictions
pred = predict(mod1, newdata = newdf)

# combining hypothetical input data with hypothetical predictions into one new data frame
hyp_preds <- data.frame(disp = newdf$disp,
                        pred = pred)

# Add new column showing whether a data point is real or hypothetical
df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

# joining our real data and hypothetical data (with model predictions)
fullpreds <- full_join(df,hyp_preds)

ggplot(fullpreds,aes(x=disp,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=mpg),color="Black") +
  theme_minimal()


#1. load data set
read_csv("../../Data/mushroom_growth.csv")
mg <- read_csv("../../Data/mushroom_growth.csv")

#2. create plots to explore relationships
mod_mush1 <- lm(GrowthRate ~ Nitrogen, data = mg)
summary(mod_mush1)

ggplot(mg, aes(x=Nitrogen, y=GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm")+ 
  theme_minimal()

mod_mush2 <- lm(GrowthRate ~ Light, data = mg)
summary(mod_mush2)

ggplot(mg, aes(x=Light, y=GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm")+ 
  theme_minimal()

mod_mush3 <- lm(GrowthRate ~ Temperature, data=mg)
summary(mod_mush3)

ggplot(mg, aes(x=Temperature, y=GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm")+ 
  theme_minimal()

#3. 2 models lm() & aov()

#4 mean sq. error of models
mean(mod_mush2$residuals^2)
mean(mod_mush1$residuals^2)
mean(mod_mush3$residuals^2)

#5. selects best model 
min(mean(mod_mush2$residuals^2),
     mean(mod_mush1$residuals^2),
      mean(mod_mush3$residuals^2))
#6. adds predictions based on new_hyp values for ind. var. 
mgdf <- mg %>% 
  add_predictions(mod_mush2) 
mgdf[,c("GrowthRate","pred")] %>% head()

#7. plots predictions alongside real data 

newmg = data.frame(Light = c(30, 40, 50, 60))
pred = predict(mod_mush2, newdata = newmg)
hyppreds = data.frame(Light = newmg$Light,
                      pred = pred)
mgdf$PredictionType <-"Real"
hyppreds$PredictionType <- "Hypthothetical"

full_predictions <- full_join(mgdf, hyppreds)

ggplot(full_predictions, aes(x=Light, y=pred, color= PredictionType)) +
  geom_point() + 
  geom_point(aes(y=GrowthRate), color = "Black") + 
  theme_minimal()

