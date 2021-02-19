########### Linear Regression ########
##### HarvardX online course ##########
# Question about baseball game
# The goal of a baseball game is to score more runs (points) than the other team.
# Each team has 9 batters who have an opportunity to hit a ball with a bat in a predetermined order. 
# Each time a batter has an opportunity to bat, we call it a plate appearance (PA).
# The PA ends with a binary outcome: the batter either makes an out (failure) and returns to the bench or the batter doesn’t (success) and can run around the bases, and potentially score a run (reach all 4 bases).
# We are simplifying a bit, but there are five ways a batter can succeed (not make an out):
 
#   Bases on balls (BB): the pitcher fails to throw the ball through a predefined area considered to be hittable (the strike zone), so the batter is permitted to go to first base.
# Single: the batter hits the ball and gets to first base.
# Double (2B): the batter hits the ball and gets to second base.
# Triple (3B): the batter hits the ball and gets to third base.
# Home Run (HR): the batter hits the ball and goes all the way home and scores a run.


# Historically, the batting average has been considered the most important offensive statistic. To define this average, we define a hit (H) and an at bat (AB). Singles, doubles, triples and home runs are hits. The fifth way to be successful, a walk (BB), is not a hit. An AB is the number of times you either get a hit or make an out; BBs are excluded. The batting average is simply H/AB and is considered the main measure of a success rate.

# Note: The video states that if you hit AFTER someone who hits many home runs, you will score many runs, while the textbook states that if you hit BEFORE someone who hits many home runs, you will score many runs. The textbook wording is accurate.


# The visualisation of choice when exploring the relationship betweens two variables is "scatter plot"
##### Scatterplot of the relationship between HRs and wins #####
library(Lahman) # load baseball dataset
library(tidyverse)
library(dslabs)
ds_theme_set()

Teams %>% filter(yearID %in% 1961:2001) %>% ## filter only a certain interval of years
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>% 
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point() 
# slope = 1.845, plot showed strong association - teams with more home runs tended to score more runs

# Scatterplot of the relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
# Ther relationship seemed not clear

# Scatterplot of the relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
# The plot showed slightly strong association althought less than HR and wins
# In general, HR causes runs. Because, when you hit a HR, at least one run will score
# It could be possible that HR cause the bases on the balls
# Thus, it might appear that a base on the balls is causing runs, in fact, it is HR that cause both runs and bases
# This is called "confounding"


# Use the filtered Teams data frame from Question 6. Make a scatterplot of win rate (number of wins per game) versus number of fielding errors (E) per game.
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(win_rate = W/G, E_per_game = E/G) %>%
  ggplot(aes(win_rate, E_per_game)) + 
  geom_point(alpha = 0.5)

# Use the filtered Teams data frame from Question 6. Make a scatterplot of triples (X3B) per game versus doubles (X2B) per game.
# Which of the following is true?
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(doubles = X2B, triples = X3B) %>%
  ggplot(aes(doubles, triples)) + 
  geom_point(alpha = 0.5)

##### Correlation ########
# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# `mean(father)` `sd(father)` `mean(son)` `sd(son)`
#       69.1         2.55        69.2      2.71

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) # the trend that the taller the father, the taller the son is not described by the summary of the average and sd

######## Correlation coefficient #######
# rho = mean(scale(x)*scale(y))
galton_heights  %>% summarise(r = cor(father, son)) %>% pull(r)
# correlation coefficient is 0.433 >> less association

# Sample correlation is a random variable

# The correlation that we compute and use as a summary is a random variable.
# When interpreting correlations, it is important to remember that correlations derived from samples are estimates containing uncertainty.
# Because the sample correlation is an average of independent draws, the central limit theorem applies. 

# compute sample correlation
R = sample_n(galton_heights, 25, replace = TRUE) %>%  # only sample from 25 sample size (N = 25), quite small
  summarise(r = cor(father, son))
R

# use Monte Carlo simulation to show distribution of sample correlation
B = 1000
N = 25
R = replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    summarise(r = cor(father, son)) %>% 
    pull(r)
}) 

# expected value and standard error
mean(R) # 0.429
sd(R) # 0.160

# illustrate in Q-Q plot to evaluate whether N is large enough or not
data.frame(R) %>% 
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N/2)))

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  summarize(corr_coeff = cor(AB_per_game, R_per_game))

# correlation between double and triple
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(dub_per_game = X2B/G, trip_per_game = X3B/G) %>%
  summarize(corr_coeff = cor(dub_per_game, trip_per_game))

#### Stratification ######
# Due to correlation is not a definite summary of relationship between variables
# The general idea of conditional expectation is that we stratify a population into groups and compute summaries in each group.

# Number of father with height 72 or 72.5
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg = galton_heights %>% 
  filter(round(father) == 72) %>% 
  summarise(avg = mean(son)) %>% 
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>%  mutate(father_strata = factor(round(father))) %>% 
  ggplot(aes(father_strata, son)) +
  geom_boxplot() + 
  geom_point()

# center of each boxplot
galton_heights %>% 
  mutate(father = round(father)) %>% 
  group_by(father) %>% 
  summarise(son_conditional_avg = mean(son)) %>% 
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

# add regression line to the plot
galton_heights %>% 
  ggplot(aes(father,son)) +
  geom_point() +
  geom_abline(intercept = b, slope = m)

########## Bivariate normal distribution ##########
# The regular normal distribution has one random variable
# a bivariate normal distribution is made up of two independent random variables
# The two variables in a bivaiate normal distribution are both normally distributed, and they have a normal distribution when both are added together

galton_heights %>% 
  mutate(z_father = round((father - mean(father)) / sd(father))) %>% 
  filter(z_father %in% -2:2) %>% 
  ggplot() +
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ father)

#### Variance explained ########
# The variance is the standard deviation squared.
# The “variance explained” statement only makes sense when the data is approximated by a bivariate normal distribution.

###### Regression lines #####
# There are two different regression lines depending on what we are interested (Y or X)
# previously we predicted height of son based on father (Y given X)
# if we would like to predict fathers' height based on sons' height, another regression line is needed (X given Y)

# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y

# In the second part of this assessment, you'll analyze a set of mother and daughter heights, also from GaltonFamilies.

# Define female_heights, a set of mother and daughter heights sampled from GaltonFamilies, as follows:

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mu_mom <- mean(female_heights$mother)
mu_mom

sd_mom = sd(female_heights$mother)
sd_mom

mu_daughter = mean(female_heights$daughter)
mu_daughter

sd_daughter = sd(female_heights$daughter)
sd_daughter

correlation = cor(female_heights$mother, female_heights$daughter)
correlation

# Calculate the slope and intercept of the regression line predicting
#  daughters' heights given mothers' heights. Given an increase in mother's
#  height by 1 inch, how many inches is the daughter's height expected to change?


# Slope of regression line predicting daughters' height from mothers' heights
m <- correlation*sd_daughter/sd_mom  
m
# Intercept of regression line predicting daughters' height from mothers' heights
b <- mu_daughter - m*mu_mom
b
# Change in daughter's height in inches given a 1 inch increase in the mother's height
(m*2+b) - (m*1+b) # essentially the slope

# percent variablity in daughter due to mother
correlation*correlation*100

# The conditional expected value of her daughter's height given the mother's height (60)
x = 60
m*x+b











