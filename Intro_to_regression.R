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

##### Scatterplot of the relationship between HRs and wins #####
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()