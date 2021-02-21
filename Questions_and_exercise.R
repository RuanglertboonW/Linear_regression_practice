##################################
###                        #######
### Questions and exercise #######
##                         #######
##################################





set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)




fit = lm(female_heights$mother ~ female_heights$daughter)
fit

fit = lm(female_heights$mother ~ female_heights$daughter[1])
fit


library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_03 <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>% 
  filter(pa >= 100) %>% 
  select(playerID, singles, bb)

player_groups <- bat_03 %>% group_by(playerID) %>% mutate(mean_singles = mean(singles), mean_bb = mean(bb)) 

player_groups %>% filter(mean_singles > 0.2 ) %>% nrow()



library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)


###################### QUESTION 1 a/b/c #####################
# Use runs (R) per game to predict average attendance.
# For every 1 run scored per game, attendance increases by how much?
Teams_small %>% mutate(rpg = R/G) %>%
  lm(avg_attendance ~ rpg, data = .)

Teams_small %>% mutate(hrpg = HR/G) %>%
  lm(avg_attendance ~ hrpg, data = .)


#Use number of wins to predict attendance; do not normalize for number of games.
# For every game won in a season, how much does average attendance increase?
Teams_small %>% lm(avg_attendance ~ W, data = .) # slope

# Suppose a team won zero games in a season.
# Predict the average attendance.
Teams_small %>% lm(avg_attendance ~ W = 0, data = .) # intercept


# Use year to predict average attendance.
# How much does average attendance increase each year?
Teams_small %>% lm(avg_attendance ~ yearID, data = .)


###################### QUESTION 2 a #####################
# Game wins, runs per game and home runs per game are positively correlated with attendance.
#  We saw in the course material that runs per game and home runs per game are correlated
#  with each other. Are wins and runs per game or wins and home runs per game correlated?

# What is the correlation coefficient for wins and runs per game?
corr <- Teams_small %>% mutate(rpg = R/G)
cor(x = corr$W, y = corr$rpg)

# What is the correlation coefficient for wins and home runs per game?
corr <- Teams_small %>% mutate(hrpg = HR/G)
cor(x = corr$W, y = corr$hrpg)


###################### QUESTION 3 a/b/c #####################
# Stratify Teams_small by wins: divide number of wins by 10 and then round to the nearest
#  integer. Keep only strata 5 through 10, which have 20 or more data points.
strat_teams <- Teams_small %>%
  mutate(strat = round(W/10)) %>%
  group_by(strat) %>%
  filter(strat %in% 5:10)

# How many observations are in the 8 win strata?
strat_teams %>% filter(strat == 8) %>% count()


strat_teams %>% filter(strat == 6) %>%
  mutate(hrpg = HR/G) %>%
  lm(avg_attendance ~ hrpg, data = .)

#######################################
#######                        ########
####### Confounding assessment ########
#######                        ########
#######################################                       
# Who is right here: the original paper or the response? Here, you will examine the data and come to your own conclusion.
# 
# The main evidence for the conclusion of the original paper comes down to a comparison of the percentages. The information we need was originally in Table S1 in the paper, which we include in dslabs:
library(tidyverse)
library(dslabs)
data("research_funding_rates")
research_funding_rates %>% select(discipline, applications_total, 
                                  success_rates_total) %>% head()

#We can compute the totals that were successful and the totals that were not as follows:
totals <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(sum) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) 

totals

# create two-by-two table
totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                     percent_women = yes_women/(yes_women+no_women))

# overall rate
rate <- totals %>%
  summarize(percent_total = 
              (yes_men + yes_women)/
              (yes_men + no_men +yes_women + no_women)) %>%
  pull(percent_total)


two_by_two <- data.frame(awarded = c("no", "yes"), 
                         men = c(totals$no_men, totals$yes_men),
                         women = c(totals$no_women, totals$yes_women))
two_by_two

# The general idea of the Chi-square test is to compare this two-by-two table to what you expect to see, which would be:
data.frame(awarded = c("no", "yes"), 
           men = (totals$no_men + totals$yes_men) * c(1 - rate, rate),
           women = (totals$no_women + totals$yes_women) * c(1 - rate, rate))

chisq_test = two_by_two %>% select(-awarded) %>% chisq.test()
chisq_test




###################### QUESTION 4 a #####################
# Fit a multivariate regression determining the effects of runs per game,
#  home runs per game, wins, and year on average attendance. Use the original
#  Teams_small wins column, not the win strata from question 3.

# build model
fit <- Teams_small %>% mutate(rpg = R/G, hrpg = HR/G) %>%
  lm(avg_attendance ~ rpg + hrpg + W + yearID, data = .)

fit
# first prediction
guess <- data.frame(
  rpg = 5,
  hrpg = 1.2,
  W = 80,
  yearID = 2002
)
predict(fit, guess)

# second prediction
guess <- data.frame(
  rpg = 5,
  hrpg = 1.2,
  W = 80,
  yearID = 1960
)
predict(fit, guess)

##############################################
# Assessment: Tibbles, do, and broom, part 2 #
##############################################
# The galton dataset is a sample of one male and one female child from each family in the GaltonFamilies dataset. The pair column denotes whether the pair is father and daughter, father and son, mother and daughter, or mother and son.
# 
# Create the galton dataset using the code below:

library(tidyverse)
library(HistData)
data("GaltonFamilies")

set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

# create sets of pair between parent and child
A = galton %>% group_by(pair) %>% filter(pair == "mother_son")
B = galton %>% group_by(pair) %>% filter(pair == "father_daughter")
C = galton %>% group_by(pair) %>% filter(pair == "father_son")
D = galton %>% group_by(pair) %>% filter(pair == "mother_daughter")

# correlation
cor(x = A$parentHeight, y = A$childHeight) #mother and son = 0.343
cor(x = B$parentHeight, y = B$childHeight) #father and daughter = 0.4
cor(x = C$parentHeight, y = C$childHeight) #father and son = 0.42
cor(x = D$parentHeight, y = D$childHeight) #mother and daughter = 0.38

# using lm() to analyse the coefficients

fitA = lm(A$childHeight ~ A$parentHeight)
summary(fitA)

fitB = lm(B$childHeight ~ B$parentHeight)
summary(fitB)

fitC = lm(C$childHeight ~ C$parentHeight)
summary(fitC)

fitD = lm(D$childHeight ~ D$parentHeight)
summary(fitD)































# # Use your model from Question 4 to predict average attendance for teams in 2002
# #  in the original Teams data frame.
# 
# pred_teams <- Teams %>% filter(yearID == 2002) %>%
#   mutate(rpg = R/G, hrpg = HR/G) %>%
#   mutate(pred_attendance = predict(fit, data.frame(rpg = rpg, hrpg = hrpg, W = W, yearID = yearID)))
# 
# cor(pred_teams$attendance, pred_teams$pred_attendance)
