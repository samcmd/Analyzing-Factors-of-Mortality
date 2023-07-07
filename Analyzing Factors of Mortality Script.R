knitr::opts_chunk$set(echo = TRUE,
                      fig.align = "center")

# Load the tidyverse, gt, and corrplot packages
pacman::p_load(tidyverse, gt, corrplot, rpart, rpart.plot, MASS)

#Set graph theme
theme_set(theme_bw())

#Read in life expectancy data set
le <- read.csv("P_Life_Expectancy_Data.csv")

#Make column names
le <- le %>% filter(X.1 == "Developing" | X.1 == "Developed") %>% rename(Country = https...www.kaggle.com.kumarajarshi.life.expectancy.who, Year = X, Status = X.1, LifeExp = X.2, AdultMort = X.3, InfantDeaths = X.4, Alcohol = X.5, HealthExpend = X.6, Hepatitis = X.7, Measles = X.8, BMI = X.9, Under5 = X.10, Polio = X.11, TotalExpend = X.12, Diphtheria = X.13, HIV = X.14, GDP = X.15, Population = X.16, thin1to19 = X.17, thin5to9 = X.18, IncomeComp = X.19, Schooling = X.20)

#Change numeric columns to numeric values
numer <- c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)
le[ , numer] <- apply(le[ , numer], 2, function(x) as.numeric(as.character(x)))



#Group the data by developed or developing status to give a more diverse peek into the data
le %>% group_by(Status) %>% 
  
  #Examine 5 random countries from each group status
  slice_sample(n = 5) %>% 
  
  #Remove the grouping for further unbiased examination
  ungroup()



#Take the first 15 data entries from the year 2013
head(le %>% filter(Year == "2013" & Population>0), 15) %>% 
  
  #Shrink the data set down to examine only some specific key information
  summarize(Country=Country,Status=Status, LifeExpectancy=LifeExp, HealthExpenditures=HealthExpend, BMI=BMI, GDP=GDP, Population=Population, SchoolingLevel=Schooling) %>% 
  
  #Add nice table formatting to the data in question
  gt() %>% tab_options(table.font.size = pct(85))



#Graph year and life expectancy for the country of Afghanistan
ggplot(le %>% filter(Country == "Afghanistan"), aes(x = Year, y = LifeExp)) + 
  
  #Make the graph a bar graph with the bars of color navy blue
  geom_bar(stat = "identity", fill="navyblue") + 
  
  #Add better formatting for the axes and title
  labs(title="Life Expectancy in Afghanistan", x="Year",y="Life Expectancy (age)") + 
  
  #Make the graph fit up to the edges of the plot
  guides(fill = "none") + 
  
  #Add line labels on the y-axis in increments of 5
  scale_y_continuous(breaks=c(55, 60, 65)) + 
  
  #Zoom in on the data to better see the difference over the years
  coord_cartesian(ylim = c(54, 65))



#Graph year and infant deaths for the country of Afghanistan
ggplot(le %>% filter(Country == "Afghanistan"), aes(x = Year, y = InfantDeaths)) + 
  
  #Make the graph a bar graph with the bars of color navy blue
  geom_bar(stat = "identity", fill="navyblue") + 
  
  #Add better formatting for the axes and title
  labs(title="Infant Deaths in Afghanistan", x="Year",y="Infant Deaths") + 
  
  #Make the graph fit up to the edges of the plot
  guides(fill = "none") + 
  
  #Add line labels on the y-axis in increments of 5
  scale_y_continuous(breaks=c(65, 70, 75, 80, 85)) + 
  
  #Zoom in on the data to better see the difference over the years
  coord_cartesian(ylim = c(62, 88))



#Graph life expectancy and BMI, removing any missing values that may be present
ggplot(le %>% filter(!is.na(BMI) & !is.na(LifeExp)), aes(x = BMI, y = LifeExp)) + 
  
  #Make the graph a scatter plot
  geom_point() + 
  
  #Add better formatting for the axes and title
  labs(title="Life Expectancy Based on BMI", x="BMI",y="Life Expectancy (age)") + 
  
  #Add line of best fit to illustrate relationship
  geom_smooth(method=lm, se=FALSE, formula = y ~ x)



#Graph life expectancy and thinness, removing any missing values that may be present
ggplot(le %>% filter(!is.na(thin1to19) & !is.na(LifeExp)), aes(x = thin1to19, y = LifeExp)) + 
  
  #Make the graph a scatter plot
  geom_point() + 
  
  #Add better formatting for the axes and title
  labs(title = "Life Expectancy Based on Thinness Ages 1 - 19", x="Thinness", y="Life Expectancy (age)") + 
  
  #Add line of best fit to illustrate relationship
  geom_smooth(method=lm, se=FALSE, formula = y ~ x) + 
  
  #Make the x-axis labels appear as percentages
  scale_x_continuous(labels = scales::percent_format(scale = 1))



#Create a correlation matrix for the country of Afghanistan
corr <- cor(le[1:16,c(4,5,6,8,11,10,17,22)], use = "complete.obs")

#Plot the correlation matrix visually
corrplot(corr)



#New data frame with no missing values
le1 <- na.omit(le)

#Choose on quantitaive variables
le1 <- le1[,4:22]

#Create a linear model of the data
le_lm <- lm(formula = LifeExp ~ ., data = le1)

#Check VIF of model
regclass::VIF(le_lm)

#Trim columns with colinearity
le1 <- le1[,c(1,2,4,6,7,8,10,11,12,13,15,18,19)]

#New model with trimmed variables
le_lm <- lm(formula = LifeExp ~ ., data = le1)

#Check VIF of new model
regclass::VIF(le_lm)

#Graph life expectancy against all chosen variables
le1 %>% pivot_longer(cols = AdultMort:Schooling, names_to = "stat", values_to = "score") %>% ggplot(mapping = aes(x = score, y = LifeExp)) + 
  geom_point() + 
  geom_smooth(se = F, method = "loess", formula = y~x) + 
  facet_wrap(facets = ~ stat, scales = "free_x", ncol = 5) + 
  labs(x = NULL, y = "Life Expectancy")

#Create cross validation function
lm_cv <- function(lm_formula, lm_data){
  
  #Create a data frame to store the models data
  data_cv <- data.frame(lm_data, y_hat = rep(-100, nrow(lm_data)))
  
  #Loop through model's data
  for (i in 1:nrow(lm_data)){
    loop_lm <- lm(formula = lm_formula,
                  data = lm_data[-i,])
    
    data_cv[i, "y_hat"] <- predict(object = loop_lm,
                                   newdata = lm_data[i,])
  }
  
  #Return cross validated data
  return(data_cv)
}

#Perform cross validation on model
le2 <- lm_cv(lm_data = le1, lm_formula = LifeExp ~ .)

#Create new model based on the cross validated data
le_lm1 <- lm(formula = LifeExp ~ ., data = le2)

#Create an MAE function
MAE <- function(actual, predicted){
  return(abs(actual - predicted) %>% mean())
}

# R-squared and MAE
le2 %>% 
  summarize(R2 = cor(LifeExp, y_hat)^2, 
            mae = MAE(actual = LifeExp, 
                      predicted = y_hat), 
            mae_improve = 1 - mae/MAE(actual = LifeExp, 
                                      predicted = mean(LifeExp)))



# R-squared plot: y vs y_hat
ggplot(data = le2, mapping = aes(x = y_hat, y = LifeExp)) + 
  geom_point() + 
  geom_smooth(se = F, formula = y~x, method = "lm") + 
  labs(x = "Predicted Life Expectancy", y = "Life Expectancy")



#Store jackknife residuals
jack <- rstudent(le_lm)

#Store models predicted values
pred <- predict(le_lm)

#Create a data frame to store both jackknife residuals and predicted values
jackvpred <- data.frame(jack, pred)

#Plot jackknife residuals vs predicted values
ggplot(jackvpred, aes(x = pred, y = jack)) +
  geom_point() + 
  geom_hline(yintercept=0, color = "red") + 
  labs(x = "Predicted life expectancy", y = "Jackknife Residuals")

#Store the density of the jackknife residuals
den <- density(jack)

#Plot the jackknife residuals density
plot(den, xlab = "Jackknife Residuals", ylab = "density", main = "")

#Color the area underneath the density plot orange
polygon(den, col = "orange")



#Select the four variables with the lowest VIF
le3 <- le2[,c(1,5,8,10,11)]

# Fully grow the tree:
le_trees <- 
  rpart(formula = LifeExp ~ .,
        data = le3,
        method = "anova",
        minsplit = 2,
        minbucket = 1,
        cp = -1)


# Find the xerror cutoff
le_trees$cptable %>% 
  data.frame() %>% 
  slice_min(xerror, n = 1, with_ties = F) %>% 
  mutate(xcutoff = xerror + xstd) %>% 
  pull(xcutoff) -> xcutoff


# Finding the row with the xerror_cutoff:
le_trees$cptable %>% 
  data.frame() %>% 
  filter(xerror < xcutoff & nsplit < 20)

le_trees$cptable %>% 
  data.frame() %>% 
  filter(xerror < xcutoff) %>%
  slice(1) %>%
  pull(CP) -> cp_prune

# Prune the tree below
le_pruned <- 
  prune(tree = le_trees,
        cp = cp_prune)


# Display the tree
rpart.plot(le_pruned, 
           digits = 4,
           fallen.leaves = TRUE,
           type = 5, 
           extra = 101,
           box.palette = 'BlGnYl',
           shadow.col = 'gray')



#create a table of the regression tree and the predicted tree values
le4 <- 
  tibble(le3, le_pred = predict(object = le_pruned))

# Create the R^2 plot for the regression tree
ggplot(data = le4, mapping = aes(x = le_pred, y = LifeExp)) + 
  geom_point(aes(color = le_pred)) +
  scale_color_viridis_c() + 
  labs(x = "Predicted Life Expectancy", y = "Life Expectancy") + 
  theme(legend.position = "none")

#Display R^2, MAE, and improved MAE values
le4 %>% 
  summarize(R2 = cor(LifeExp, le_pred)^2, 
            mae = MAE(actual = LifeExp, 
                      predicted = le_pred), 
            mae_improve = 1 - mae/MAE(actual = LifeExp, 
                                      predicted = mean(LifeExp)))
