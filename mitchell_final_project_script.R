## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline


## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("dataset.csv")

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
mean(dataset$main_topics)
sd(dataset$main_topics)
summary(dataset$main_topics)
table(dataset$main_topics)

mean(dataset$song_length)
sd(dataset$song_length)
summary(dataset$song_length)
table(dataset$song_length)

mean(dataset$tempo_bpm)
sd(dataset$tempo_bpm)
summary(dataset$tempo_bpm)
table(dataset$tempo_bpm)

mean(dataset$energy_level)
sd(dataset$energy_level)
summary(dataset$energy_level)
table(dataset$energy_level)

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset$main_topics, dataset$energy_level)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
# BOX PLOT
ggplot(dataset, aes(x = main_topics, y = tempo_bpm)) +
  geom_boxplot() +
  labs(title = "Box Plot of Main Topics and Energy Level",
       x = "main_topics",
       y = "tempo_bpm") +
  theme_minimal()

# BOX PLOT
ggplot(dataset, aes(x = main_topics, y = song_length)) +
  geom_boxplot() +
  labs(title = "Box Plot of Main Topics and Song Length",
       x = "main_topics",
       y = "song_length") +
  theme_minimal()

# BOX PLOT
ggplot(dataset, aes(x = energy_level, y = tempo_bpm)) +
  geom_boxplot() +
  labs(title = "Box Plot of Energy Level and Tempo BPM",
       x = "energy_level",
       y = "tempo_bpm") +
  theme_minimal()

# BOX PLOT
ggplot(dataset, aes(x = energy_level, y = song_length)) +
  geom_boxplot() +
  labs(title = "Box Plot of Energy Level and Song Length",
       x = "energy_level",
       y = "song_length") +
  theme_minimal()

anova <- aov(song_length ~ energy_level, data = dataset)
summary(anova)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(dataset$tempo_bpm, dataset$song_length)
print(linear_plot)

meany <- mean(dataset$tempo_bpm)
meanx <- mean(dataset$song_length)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")

linear_relationship <- lm(song_length ~ tempo_bpm, data = dataset)
summary(linear_relationship)

abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
plot(dataset$song_length, residuals(linear_relationship))
abline(h = 0, col = "red")

plot(dataset$tempo_bpm, residuals(linear_relationship))
abline(h = 0, col = "red")

