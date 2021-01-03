#######################
## Analysis of       ##
##  Fish Weight      ##
##   based on        ##
##  Length, Specie   ##
##      and          ##
##    Width          ##
##                   ##
##      NO.2         ##
##                   ##
##   Data Analysis   ##
##                   ##
#######################

# Load necessary packages

# Data manipulation
library(tidyverse)

# Correlation Matrix
library(Hmisc)

# Different graph thenmes
library(ggthemes)

# For scaling ggplots
library(scales)

# Estimate robust SE
library(lmtest)
library(estimatr)

# Compare models with robust SE
library(texreg)

# Clear variable environmental memory
rm(list=ls())

# Call the data from github
my_url <- "https://raw.githubusercontent.com/yurialmeida5/DA2_Final_Project/master/data/clean/fish_data_clean.csv"

# Upload the cleaned data to a dataframe

fish_data <- read_csv( my_url )


# Introduction and Descriptive Analysis -----------------------------------


# The objective of the analysis is to develop a multiple regression model. 
# Predicting the weight in grams (dependent variable) of a fish using its physical dimensions:
# Height, Width, Lengths(1,2,3) and its specie type. (Explanatory Variables).
# The Lengths are continuous variables measured in centimeters(cm).
# The Height is the maximum height as % of Length3 (Height =  Height/Length3 * 100)
# The Width is the maximum height as % of Length3 (Width  =  Width/Length3 * 100)
# There are 7 species in the sample, with different numbers of observations of each.
# The variable species was transformed into dummy variables named from (Specie1 to Specie 7)
# The data set is sorted by species from 1 to 7 and then approximately by length within a species.

# Descriptive statistics of the Fish Data Set

# Summary Statistics (without Species Dummy Variables)

fish_data %>% 
  select(-c(Species1,Species2,Species3,Species4, Species5, Species6, Species7)) %>% 
  summary() 

# Check the histograms distribution (without Species Dummy Variables)

fish_data %>% select(-c(Species1,Species2,Species3,Species4, Species5, Species6, Species7)) %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()

# Considering the analysis of the graphs and the summary statistics, 
# it's possible to visualize that: 

# Whether Height and Width are closer to the regular normal distribution, 
# The Length(1,2,3) and Weight variables are slightly skewed forming Right-Tails with the presence of some outliers.
# Taking that into account, it's possible to apply some log transformations while the models are build.
# (Specially at the dependent variable Weight and the explanatory Length)

# Variable Sex shouldn't be used for analysis because it has a good amount of missing values that may impact the model

# Checking correlation between variables 

corr_matrix <- rcorr(as.matrix(select(
  fish_data , -c(Species1,Species2,Species3,Species4, Species5, Species6, Species7, Sex))))

# Correlation Matrix Values
corr_matrix$r

# As the correlation results between the Length's showed pretty much closer to 1 with significant p-values
# Length1 ~ Length3: r = 0.99 and p = 0.000.
# Length2 ~ Length3: r = 0.99 and p = 0.000.
# Length1 ~ Length2: r = 1.00 and p = 0.000.

# Having the three of them at the same regression model may lead to errors and won't produce
# significant better adjustments. With that, the variables Length1 and Length2 won't be considered at any analysis, 
# Having only the variable Length3, the total size of the fish, as the final length measurement.

# Remove unnecessary variables for analysis

fish_data <- fish_data %>% 
  select(-c(Length1,Length2,Sex))

fish_data <- rename(fish_data, Final_Length = Length3)

# Scatter-Plots


# Weight and Length -------------------------------------------------------


# 1) Weight and Length

# 1.1) Weight ~ Final_Length: level-level model without scaling

ggplot( fish_data , aes(x = Final_Length , y = Weight)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Length (cm)",y = "Weight (grams)") 
  
# 1.2) Weight ~ ln(Final_Length): level-log model without scaling

ggplot( fish_data , aes(x = Final_Length , y = Weight)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Ln(Length) (cm)", y = "Weight (grams)") +
  scale_x_continuous(trans = log_trans(), breaks = c(10,20,30,40,50,60, 70))

# 1.3) ln(Weight) ~ Final_Length: log-level model without scaling

ggplot( fish_data , aes(x = Final_Length , y = Weight)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Length (cm)", y = "ln(Weight) (grams)") +
  scale_y_continuous( trans = log_trans())

# 1.4) ln(Weight) ~ ln(Final_Length): log-level model without scaling

ggplot( fish_data , aes(x = Final_Length , y = Weight)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "ln(Length) (cm)", y = "ln(Weight) (grams)") +
  scale_y_continuous( trans = log_trans()) +
  scale_x_continuous(trans = log_trans(), breaks = c(10,20,30,40,50,60, 70))

# Creating the new variables - Log Transformation

fish_data <- fish_data %>% mutate(ln_Weight = log(Weight),
                    ln_Final_Length = log(Final_Length))

### Check homoskedastic - Breusch-Pagan Test
lvl_lvl <- lm( Weight ~ Final_Length , data = fish_data)
log_lvl <- lm( ln_Weight ~ Final_Length , data = fish_data )
lvl_log <- lm( Weight ~ ln_Final_Length , data = fish_data )
log_log <- lm( ln_Weight  ~ ln_Final_Length , data = fish_data )

bptest( lvl_lvl ) # small p-value can´t reject the null hypothesis (homoscedasticity)
bptest( lvl_log ) # small p-value can´t reject the null hypothesis (homoscedasticity) 
bptest( log_lvl ) # big p-value can reject the null hypothesis (homoscedasticity) 
bptest( log_log ) # big p-value can reject the null hypothesis (homoscedasticity)

log_lvl <- lm_robust( ln_Weight ~ Final_Length , data = fish_data , se_type = "HC2" )
log_log <- lm_robust( ln_Weight  ~ ln_Final_Length  , data = fish_data , se_type = "HC2" )

# Model Comparison - Log Transformation Options

data_out <- "out/"
htmlreg(list(lvl_lvl , log_lvl , lvl_log,  log_log ),
         type = 'html',
         custom.model.names = c("lvl_lvl", "log_lvl","lvl_log",
                                "log_log"),
         caption = "Log Transformations Comparison",
         file = paste0( data_out ,'logtrans_Weight_Length.html'), include.ci = FALSE)


# Conclusion 

# According to the results obtained from the graphs and model comparison,
# the log-log regression shows not only a better fit, 
# but also an extraordinary R-squared adjusted with a really significant p-value.
# This is supported by the Right-Tailed Distributions Patterns encountered both in Weight and Length 
# The variable Length itself could be used alone for the predictions.

# Considering that, the first regression model for analysis is: 

reg1 <- lm_robust( ln_Weight  ~ ln_Final_Length  , data = fish_data , se_type = "HC2" )

# Remove the temporary variables

rm(log_log,log_lvl,lvl_log,lvl_lvl)


# Weight and Height -------------------------------------------------------

# 2) Weight and Height

# Considering the results obtained before, the variable Weight will be log transformed.
# With that, the analysis of the graphs will be as following:

# 2.1) ln(Weight) ~ Height: log-level model without scaling

ggplot( fish_data , aes(x = Height , y = Weight)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Height (%)", y = "ln(Weight) (grams)") +
  scale_y_continuous(trans = log_trans())

# 2.2) ln(Weight) ~ ln(Height): log-level model without scaling

ggplot( fish_data , aes(x = Height , y = Weight)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "ln(Height) (%)", y = "ln(Weight) (grams)") +
  scale_y_continuous(trans = log_trans()) +
  scale_x_continuous(trans = log_trans())

# Creating new variables - Log transformation

fish_data <- fish_data %>% mutate(ln_Height = log(Height))

### Check homoskedastic - Breusch-Pagan Test
log_lvl <- lm( ln_Weight  ~ Height  , data = fish_data )
log_log <- lm( ln_Weight  ~ ln_Height , data = fish_data )

bptest( log_lvl ) # big p-value can reject the null hypothesis (homoscedasticity) 
bptest( log_log ) # big p-value can reject the null hypothesis (homoscedasticity)

data_out <- "out/"
htmlreg(list( log_lvl ,  log_log ),
        type = 'html',
        custom.model.names = c("log_lvl", "log_log"),
        caption = "Log Transformations Comparison",
        file = paste0( data_out ,'logtrans_Weight_Height.html'), include.ci = FALSE)

# Conclusion: 

# The analysis of the graphs and the comparisons between the models with and without the log-transformations,
# didn't show any significant statistic difference. The pattern expressed in both graphs are quite similar,
# And the results obtained in terms of R-squared are pretty close as well. 
# With that, due to the ease and more tangible interpretation of the results, the model chosen was the log-lvl one.

# Another important aspect to highlight is the fact that 
# the pattern observed can possibly fit a quadratic or cubic polynomial models.

# Evaluate the polynomial transformations for the variable Height

ggplot( data = fish_data, aes( x = Height, y = Weight ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

ggplot( data = fish_data, aes( x = Height, y = Weight ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )

# Creating the new variables - Cubic transformation

fish_data <- fish_data %>% mutate(Height_sq = Height^2,
                     Height_cb = Height^3)

# Comparing the regressions model with and without the quadratic/cubic transformations

reg2 <- lm_robust( ln_Weight  ~ ln_Final_Length + Height , data = fish_data , se_type = "HC2" )
reg3 <- lm_robust( ln_Weight ~ ln_Final_Length + Height + Height_sq , data = fish_data)
reg4 <- lm_robust( ln_Weight ~ ln_Final_Length + Height + Height_sq + Height_cb , data = fish_data)

# Model Comparison

data_out <- "out/"
htmlreg( list(reg1 , reg2 , reg3, reg4 ),
         type = 'html',
         custom.model.names = c("Only Length", "Length + Height",
                                "Length + Quadratic Height", "Length + Cubic Height" ),
         caption = "Modelling the size of fishes by Length and Height",
         file = paste0( data_out ,'model_comparison_Length_Height.html'), include.ci = FALSE)

# Remove non-used variables

rm(log_lvl,log_log)


# Conclusion 

# Although the cubic and quadratic models showed a slightly better fit than the log-lvl,
# the interpretation is much easier under the simple model. 
# The differences aren't that significant enough in terms R-Squared as well.



# Weigth and Width --------------------------------------------------------


# 3) Weight and Width

# Now that it´s known how to treat the Length and Height variables, it's time to focus on the Width

# 3.1) ln(Weight) ~ Width: log-level model without scaling

ggplot( fish_data , aes(x = Height , y = Width)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Width (%)", y = "ln(Weight) (grams)") +
  scale_y_continuous(trans = log_trans())

# 3.2) ln(Weight) ~ ln(Width): log-level model without scaling

ggplot( fish_data , aes(x = Height , y = Width)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "ln(Width) (%)", y = "ln(Weight) (grams)") +
  scale_y_continuous(trans = log_trans()) +
  scale_x_continuous(trans = log_trans())

# Creating new variables - Log transformation

fish_data <- fish_data %>% mutate(ln_Width = log(Width))

### Check homoskedastic - Breusch-Pagan Test
log_lvl <- lm( ln_Weight  ~ Width  , data = fish_data )
log_log <- lm( ln_Weight  ~ ln_Width , data = fish_data )

bptest( log_lvl ) # small p-value can´t reject the null hypothesis (homoscedasticity)
bptest( log_log ) # small p-value can´t reject the null hypothesis (homoscedasticity)

log_lvl <- lm_robust( ln_Weight ~ Width , data = fish_data , se_type = "HC2" )
log_log <- lm_robust( ln_Weight  ~ ln_Width  , data = fish_data , se_type = "HC2" )

data_out <- "out/"
htmlreg(list( log_lvl ,  log_log ),
        type = 'html',
        custom.model.names = c("log_lvl", "log_log"),
        caption = "Log Transformations Comparison",
        file = paste0( data_out ,'logtrans_Weight_Width.html'), include.ci = FALSE)

# Conclusion: 

# The analysis of the graphs and the comparisons between the models with and without the log-transformations,
# didn't show any significant statistic difference. The pattern expressed in both graphs are quite similar,
# And the results obtained in terms of R-squared are pretty close as well. 
# With that, due to the ease and more tangible interpretation of the results, the model chosen was the log-lvl one.

# Another important aspect to highlight is the fact that 
# the pattern observed can possibly fit a quadratic or cubic polynomial models.

# Evaluate the polynomial transformations for the variable Width

ggplot( data = fish_data, aes( x = Width, y = Weight ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

ggplot( data = fish_data, aes( x = Width, y = Weight ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )

# Creating the new variables - Cubic transformation

fish_data <- fish_data %>% mutate(Width_sq = Width^2,
                                  Width_cb = Width^3)

# Comparing the regressions model with and without the quadratic/cubic transformations

reg5 <- lm_robust( ln_Weight  ~ ln_Final_Length + Height + Width , data = fish_data , se_type = "HC2" )
reg6 <- lm_robust( ln_Weight ~ ln_Final_Length + Height + Width + Width_sq , data = fish_data)
reg7 <- lm_robust( ln_Weight ~ ln_Final_Length + Height + Width + Width_sq + Width_cb , data = fish_data)

# Model Comparison

data_out <- "out/"
htmlreg( list(reg1 , reg2 , reg5, reg6 , reg7 ),
         type = 'html',
         custom.model.names = c("Only Length", "Length + Height",
                                "Length + Height + Width", "Length + Height + Quadratic Width",
                                "Length + Height + Cubic Width"),
         caption = "Modelling the size of fishes by Length, Height and Width",
         file = paste0( data_out ,'model_comparison_Length_Height_Width.html'), include.ci = FALSE)

# Conclusion 

# Although the cubic and quadratic models showed a slightly better fit than the log-lvl,
# the interpretation is much easier under the simple model. 
# The differences aren't that significant enough in terms R-Squared as well.

# Remove non-used variables

rm(log_lvl,log_log)


# Weight and Species ------------------------------------------------------

# 4) Weight and Species

# Considering what was done so far, the best possible model so far is represented by reg5

# ln_Weight  ~ ln_Final_Length + Height + Width

# However, it's still missing the impact of the species variables. 
# Another regression model can actually be created for that situation: 

reg8 <- lm_robust( ln_Weight  ~ ln_Final_Length + Height + Width + Species1 + Species2 + Species3 + Species4 + Species5 + Species6 , data = fish_data , se_type = "HC2" )

# Model Comparison

data_out <- "out/"
htmlreg( list(reg1 , reg2 , reg5, reg8 ),
         type = 'html',
         custom.model.names = c("Only Length", "Length + Height",
                                "Length + Height + Width", 
                                "Length + Height + Width + Species"),
         caption = "Modelling the size of fishes by Length, Height, Width and Species",
         file = paste0( data_out ,'model_comparison_Length_Height_Width_Species.html'), include.ci = FALSE)

# Conclusion

# Adding the dummy variables created for species, the final model, reg8, shows itself really representative
# with higher fit and prediction capabilities. 
# Although the R-squared didn't present any substantive statistical difference from the previous models, 
# the p-values were highly significant. It means that the model explains well the dataset chosen, 
# using a good number of really representative variables.

# For all of that, the reg8 is going to be the model used for predictions.


# Predictions -------------------------------------------------------------

set.seed(123)

# Get the predicted y values from the model
fish_data$reg8_y_pred <- reg8$fitted.values

# Calculate the errors of the model
fish_data$reg8_res <- fish_data$ln_Weight - fish_data$reg8_y_pred 

# Find fishes with largest negative errors
fish_data %>% top_n( -5 , reg8_res ) %>% 
  select( Weight , Species , ln_Weight , reg8_y_pred , reg8_res )

# Find fishes with largest positive errors
fish_data %>% top_n( 5 , reg8_res ) %>% 
  select( Weight , Species , ln_Weight , reg8_y_pred , reg8_res )

# Create: y_hat-y plot
ggplot( data = fish_data ) +
  geom_point (aes( x = reg8_y_pred , y = ln_Weight ) ,  color='red')+
  geom_line( aes( x = ln_Weight , y = ln_Weight ) , color = 'navyblue' , size = 1.5 )+
  labs( x = 'Predicted fish weights', y = 'Actual fish Weights')

# Conclusion 

# As expected the model predicted very well the values for Weight. 
# The variables presented themselves significant (lower p-values) and important for the model description.
# The R-squared value is really representative and generates a great understanding of how the dependent variable Weight
# is changing along the dataset. 
# Although it's impossible to infer any causality, this model seems a good income for further studies in the area.

