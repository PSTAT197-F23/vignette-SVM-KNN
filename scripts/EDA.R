
# Load all necessary packages for EDA
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(corrplot)
library(ggpubr)
library(gridExtra)

# Load and explore data

load('./data/heart.rda')
heart_num <- heart

heart %>%
  head()

sum(is.na(heart))

# Add a new variable called `age_class` at the back which gives us better idea of the `age` variable. 
# Put all categorical variables into its factor form so that we could see a count in the summary sheet instead of a meaningless mean 
# and variance, since it was all converted into number representations. Factors make visualizations possible for 
# categorical variables.

heart <- heart %>%
  dplyr::mutate(gender = case_when(sex == "0" ~ "Female", sex == "1" ~ "Male"),
                output = case_when(output == "0" ~ "No", output == "1" ~ "Yes"),
                gender = factor(gender),  
                cp = factor(cp),
                fbs = factor(fbs),
                restecg = factor(restecg),
                exng = factor(exng),
                caa = factor(caa),
                thall = factor(thall),
                output = factor(output),
                sex = factor(sex),
                slp = factor(slp),
                age_class = cut(age, breaks = seq(25, 80, by = 5))
  )

heart %>% 
  summary

# From this summary we need to pay some to several things:

# -   The number of male is twice the number of female in this data. This might affect any analysis based on gender.
# 
# -   The older it gets, the less data points we have, thus out model might have a better performance when predicting younger individuals.
# 
# -   There are several data points that is so rare that they are considered outliers in several columns like level4 in `caa`, level0 in `thall`, level2 in `restecg`.
# 
# -   The good thing is, our outcome variable is pretty balanced. This means stratifying is not so needed.
# 
# -   An issue of the dataset is that it contains only 303 data points. However, we can see that it is generally balanced, not skewed. Which means it is reasonable to proceed. 


# CORRELATION

# Use the corrplot function to create a correlation plot amongst all variables.
graph_corr <- corrplot(cor(heart_num[,]),
                       order = "AOE",
                       method = "number")

# From this correlation plot we can see that highest positive correlation is between `cp` and `output`, which mean chest pain might be directly leading to heart problems. The other correlation stood out is the negative correlation between `slp` and `oldpeak`. Which implies depression might be reflected in the slope of the peak exercise. Overall, correlation between variables are not very strong, this implies that building a machine learning model is necessary since we can not tell whether someone has high risk of heart attack by looking at a single variable. Also, this implies we do not have to eliminate any variables, since that all provides information in different aspects.


# VISUAL EDA: Variables
# In the following plots, we aim to explore the various features, or variables, in our dataset. To do this, we use bar plots by running and configuring the ggplot() function.

# Age

ggplot(heart, aes(x=age_class, fill = output, color = output)) + 
  geom_bar(position = "dodge", alpha=0.5) + 
  theme(legend.position = "top") + 
  labs(title = "Age vs. Heart Disease", 
       x = "Age", y = "Number of People")
ggplot(heart, aes(x=age_class, fill = output, color = output)) + 
  geom_bar(position = "fill", alpha=0.5) + 
  theme(legend.position = "top") + 
  labs(title = "Age vs. Heart Disease", 
       x = "Age", y = "Number of People")
ggplot(heart, aes(x=age)) + 
  geom_bar(position = "dodge", alpha=0.5) + 
  theme(legend.position = "top")+ 
  labs(title = "Age Distibution", 
       x = "Age", y = "Number of People")

# The `age` distribution is generally normally distributed, with a little bit of a  left skewed. However, when we take a look at the joint graph with `age` distribution separated by outcome variable, we see that people who has low risk of heart attack is normally distribution and has mean between 55-60, while people with high risk is normally distribution except there is a high value of people in age group 40-45. As for probability of getting a higher risk, we could see that surprisingly, it seems like the younger it is, the higher risk of getting heart attack is. 


# Resting blood pressure

heart <- heart %>%
  mutate(trtbps_class = cut(trtbps, breaks = seq(90, 200, by = 10))) 
graph_trtbps <- ggplot(heart, aes(x=trtbps_class, fill = output, color = output)) + 
  geom_bar(position = "dodge", alpha=0.5) + 
  theme(legend.position = "top")
graph_trtbps + labs(title = "Resting Blood Pressure vs. Heart Disease", 
                    x = "RBP", y = "Number of People")
# The resting blood pressure is right skewed and does not change much due to whether what output variable is. It is most likely to be irrelevant. 


# Cholesterol

heart <- heart %>%
  mutate(chol_class = cut(chol, breaks = seq(126, 564, by = 40))) 
graph_chol <- ggplot(heart, aes(x=chol_class, fill = output, color = output)) + 
  geom_bar(position = "dodge", alpha=0.5) + 
  theme(legend.position = "top")
graph_chol + labs(title = "cholestoral vs. Heart Disease", 
                  x = "cholestoral", y = "Number of People")

# This is another right skewed variable, with not so many correlation with outcome variable.

# Maximum heart rate

heart <- heart %>%
  mutate(thalachh_class = cut(thalachh, breaks = seq(70, 210, by = 10))) 
graph_thalachh <- ggplot(heart, aes(x=thalachh_class, fill = output, color = output)) + 
  geom_bar(position = "dodge", alpha=0.5) + 
  theme(legend.position = "top")
graph_thalachh + labs(title = "Max Heart Rate vs. Heart Disease", 
                      x = "Max Heart Rate", y = "Number of People")

# The distribution curve seems smooth and bell shaped so the data looks valid. We can see that the maximum heart rate is an important indicator for heart attack risk. Once it exceed 150, the risk of having heart attach is much higher than before. The huge difference in the mean of the two color bar shows the result as well.

# Gender

heart %>%
  ggplot(aes(x = output, fill = gender)) +
  geom_bar(position = "stack")

heart_gender <- heart %>%
  group_by(age_class) %>%
  select(age_class, output, gender)

heart_gender <- heart_gender %>%
  transform(freq = ave(seq(nrow(heart_gender)), age_class, FUN=length))

# We've made a lot of bar plots, so now, in order to do further data exploration, we will use balloon plots, made with the ggballoonplot() function.
graph_gender <- ggballoonplot(data = heart_gender, x = "age_class", y = "output", size = "freq", 
                              fill = "freq", facet.by = "gender", 
                              ggtheme = theme_bw()) + scale_fill_viridis_c(option = "C")
graph_gender

# the number of females is far less than the number of males, and this might greatly influence the result. We can see here from the first graph that although male patient result in half and half, most of the female individuals has high risk of getting heart attach. This rate is imbalanced and might greatly affect the result. Also, let us take a look when we incooperate the age variable as well. We do not see a strong correlation between age and gender together with outcome.


# Chest pain

cp1 <- heart %>%
  ggplot(aes(x = cp, fill = output)) +
  geom_bar(position = "fill")

cp2<- heart %>%
  ggplot(aes(x = cp, fill = output)) +
  geom_bar(position = "stack")

graph_cp <- grid.arrange(cp1,cp2, ncol = 2)

# We see a rather strong correlation between chest pain and having a high risk of heart attack. If we consider `cp={1,2,3}` as having chest pain, this correlation is even stronger. It is very reasonable. However, we should not think is that chest pain causes heart attack. It is the other way around, chest pain might be caused by many things, however, if you have heart problem, you probably have chest pain. This means having or not having chest pain is a reasonable indicator.

# Fasting blood sugar

fbs1<- heart %>%
  ggplot(aes(x = fbs, fill = output)) +
  geom_bar(position = "fill")

fbs2<- heart %>%
  ggplot(aes(x = fbs, fill = output)) +
  geom_bar(position = "stack")

graph_fbs <- grid.arrange(fbs1,fbs2, ncol = 2)

# We could see from the graph that it is pretty balanced and not affected by outcome. Although there are a lot less people have blood sugar > 120, it should still not significant in out model.

# Exercise induced Angina

exng1 <- heart %>%
  ggplot(aes(x = exng, fill = output)) +
  geom_bar(position = "fill")

exng2 <- heart %>%
  ggplot(aes(x = exng, fill = output)) +
  geom_bar(position = "stack")

graph_exng <- grid.arrange(exng1,exng2, ncol = 2)

# We see that the portion of `No` in `exng=1` is significantly higher. This implies it might has a negative correlation with the outcome variable. This is some how counter intuitive because I think it is more seasonable to relate having angina after exercise with heart problem, which means a higher risk of having a heart attack.

# Slope

slp1 <- heart %>%
  ggplot(aes(x = slp, fill = output)) +
  geom_bar(position = "fill")

slp2 <- heart %>%
  ggplot(aes(x = slp, fill = output)) +
  geom_bar(position = "stack")

graph_slp <- grid.arrange(slp1,slp2, ncol = 2)

# From the barplot we can see that `slp=2` has a clear difference than the other two cases. If we consider the other two cases as baseline, having a upsloping line might be a indicator. However, the number of flat slope is too small so we are not so sure to use that as a baseline. We need for analysis to make further conclusion.

# EDA CONCLUSION
# From the analysis above, we could see that `cp` and `exng` tends to be the strongest predictor of outcome. Also, `age` and `gender` may also affect but we are unsure about it. Among the first two, `cp` has a positive correlation while `exng` has a negative. Luckily, according to correlation matrix, those variables does not relate to other predictor variables, which mean we do not have to worry much above avoiding the interaction in our model building stage.