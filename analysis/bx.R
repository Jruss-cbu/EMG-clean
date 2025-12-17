# Null space Tapes Bx analysis
# Rupsha Panda
# 11/25/2025

# load in packages 
library(lme4)
library(lmerTest)
library(car)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(Rmisc)
library(readr)
library(rstan)
library(reshape2)
library(abind)  
library(lsa)
library(dplyr)

setwd('/Users/rupshapanda/Desktop/Plasticity Lab/Null_Space_EMG/')
tapes_df = read_csv(paste0(getwd(),'/tapes_bx.csv'))
cog_tapes_df = read_csv(paste0(getwd(),'/tapes_cog_bx.csv'))

tapes_df$Participant <- as.factor(tapes_df$Participant)
tapes_df$Day <- as.factor(tapes_df$Day)
tapes_df$Block <- as.factor(tapes_df$Block)
tapes_df$Transferred <- as.numeric(tapes_df$Transferred)

cog_tapes_df$Participant <- as.factor(cog_tapes_df$Participant)
cog_tapes_df$Day <- as.factor(cog_tapes_df$Day)
cog_tapes_df$Block <- as.factor(cog_tapes_df$Block)
cog_tapes_df$Places <- as.numeric(cog_tapes_df$Placed)

mean_subj_curve <- tapes_df %>%
  dplyr::group_by(Participant, Day) %>%
  dplyr::summarise(mean_transferred = mean(Transferred, na.rm = TRUE))

med_subj_curve =  tapes_df %>%
  dplyr::group_by(Participant, Day) %>%
  dplyr::summarise(med_transferred = median(Transferred, na.rm = TRUE))

group_curve <- med_subj_curve %>%
  dplyr::group_by(Day) %>%
  dplyr::summarise(
    mean_of_medians = mean(med_transferred),
    sd = sd(med_transferred),
    se = sd / sqrt(dplyr::n()),
    n = dplyr::n(),
    .groups = "drop"
  )


ggplot() +
  # 1) Individual subject curves (each gets a color)
  geom_line(
    data = med_subj_curve,
    aes(
      x = as.numeric(Day),
      y = med_transferred,
      group = Participant,
      color = Participant    # ← add this
    ),
    alpha = 0.5
  ) +
  geom_point(
    data = med_subj_curve,
    aes(
      x = as.numeric(Day),
      y = med_transferred,
      group = Participant,
      color = Participant    # ← and here
    ),
    alpha = 0.7,
    size = 1.8
  ) +
  
  # 2) Group-level curve (black on top)
  geom_line(
    data = group_curve,
    aes(
      x = as.numeric(Day),
      y = mean_of_medians,
      group = 1
    ),
    linewidth = 1.4,
    color = "black"
  ) +
  geom_point(
    data = group_curve,
    aes(
      x = as.numeric(Day),
      y = mean_of_medians
    ),
    size = 3,
    color = "black"
  ) +
  geom_errorbar(
    data = group_curve,
    aes(
      x = as.numeric(Day),
      ymin = mean_of_medians - se,
      ymax = mean_of_medians + se
    ),
    width = 0.1,
    color = "black"
  ) +
  
  scale_x_continuous(breaks = 1:5) +
  labs(
    x = "Day",
    y = "Transferred (median per participant)",
    title = "Tapes Task Learning: Individual Participants + Group Curve"
  ) +
  theme_classic()


# cognitive load
# cognitive load
cog_mean_subj_curve <- cog_tapes_df %>%
  dplyr::group_by(Participant, Day) %>%
  dplyr::summarise(mean_placed = mean(Placed, na.rm = TRUE), .groups = "drop")

cog_med_subj_curve <- cog_tapes_df %>%
  dplyr::group_by(Participant, Day) %>%
  dplyr::summarise(med_placed = median(Placed, na.rm = TRUE), .groups = "drop")

cog_group_curve <- cog_med_subj_curve %>%
  dplyr::group_by(Day) %>%
  dplyr::summarise(
    mean_of_medians = mean(med_placed),
    sd  = sd(med_placed),
    se  = sd / sqrt(dplyr::n()),
    n   = dplyr::n(),
    .groups = "drop"
  )

library(ggplot2)

ggplot() +
  # 1) Individual subject curves (each gets a color)
  geom_line(
    data = cog_med_subj_curve,
    aes(
      x = as.numeric(Day) * 2 - 1,
      y = med_placed,
      group = Participant,
      color = Participant
    ),
    alpha = 0.5
  ) +
  geom_point(
    data = cog_med_subj_curve,
    aes(
      x = as.numeric(Day) * 2 - 1,
      y = med_placed,
      group = Participant,
      color = Participant
    ),
    alpha = 0.7,
    size = 1.8
  ) +
  
  # 2) Group-level curve (black on top)
  geom_line(
    data = cog_group_curve,
    aes(
      x = as.numeric(Day) * 2 - 1,
      y = mean_of_medians,
      group = 1
    ),
    linewidth = 1.4,
    color = "black"
  ) +
  geom_point(
    data = cog_group_curve,
    aes(
      x = as.numeric(Day) * 2 - 1,
      y = mean_of_medians
    ),
    size = 3,
    color = "black"
  ) +
  geom_errorbar(
    data = cog_group_curve,
    aes(
      x = as.numeric(Day) * 2 - 1,
      ymin = mean_of_medians - se,
      ymax = mean_of_medians + se
    ),
    width = 0.1,
    color = "black"
  ) +
  
  scale_x_continuous(breaks = c(1,3,5)) +
  labs(
    x = "Day",
    y = "Placed (median per participant)",
    title = "Cog Load Tapes Task Learning: Individual Participants + Group Curve"
  ) +
  theme_classic()



