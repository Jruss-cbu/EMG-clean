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

setwd('/Users/rupshapanda/Desktop/Plasticity Lab/Null_Space_EMG/analysis/')
raw_df = read_csv(paste0(getwd(),'/EMG_features_per_channel_new.csv'))

# if you only care about the Tapes task, filter first
df <- raw_df %>%
  dplyr::group_by(Subject, Day, Task, Trial, Muscle) %>%
  filter(Task %in% c("Tapes", "Tapes_cog_load")) %>% 
  filter(RMS_extractor > 1e-6) %>% 
  filter(!(RMS == 0 & MNF == 0 & ZC == 0)) 

# Fractal Dimension Feature
# take the mean from each participant everyday 

# 1. Base: Tapes only, non-missing/nonnull FractalDim
fractal_base <- df %>%
  filter(
    Task == "Tapes_cog_load",
    !is.na(FractalDim),
    FractalDim != 0
  ) %>%
  select(Subject, Day, Trial, Muscle, Channel_new, FractalDim) %>%
  mutate(
    Day   = as.numeric(as.character(Day)),
    Trial = as.numeric(as.character(Trial))
  )

# 2. Subject-level median per Day × Trial × Muscle
fractal_subj <- fractal_base %>%
  group_by(Subject, Day, Trial, Muscle) %>%
  dplyr::summarise(
    FractalDim = median(FractalDim, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Group-level mean + SE (within-subject) across subjects
fractal_group <- summarySEwithin(
  data       = fractal_subj %>%
    mutate(
      Day   = factor(Day),
      Trial = factor(Trial)
    ),
  measurevar = "FractalDim",
  withinvars = c("Day", "Trial", "Muscle"),
  idvar      = "Subject"
) %>%
  mutate(
    Day   = as.numeric(as.character(Day)),
    Trial = as.numeric(as.character(Trial))
  )

ggplot(fractal_group,
       aes(x = factor(Trial),        # X = Trial (discrete)
           y = FractalDim,
           color = Muscle,
           group = Muscle)) +
  geom_line(aes(group = Muscle), linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = FractalDim - se,
                    ymax = FractalDim + se),
                width = 0.15,
                linewidth = 0.8) +
  facet_wrap(~ Day, nrow = 1) +     # facet by Day
  labs(
    title = "Tapes Cog load Fractal Dimension by Trial",
    x = "Trial",
    y = "Fractal Dimension"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.text  = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    plot.title  = element_text(size = 16, face = "bold")
  )


#days_of_interest <- c(1, 3, 5)   # change this to c(3,4,5) etc if needed
# ZERO CROSSING
# 1. Base: Tapes only, non-missing/nonnull FractalDim
zc_base <- df %>%
  filter(
    Task == "Tapes",
    !is.na(ZC),
    ZC != 0
  ) %>%
  select(Subject, Day, Trial, Muscle, Channel_new, ZC) %>%
  mutate(
    Day   = as.numeric(as.character(Day)),
    Trial = as.numeric(as.character(Trial))
  ) #%>%
  #filter(Day %in% days_of_interest) 

# 2. Subject-level median per Day × Trial × Muscle
zc_subj <- zc_base %>%
  group_by(Subject, Day, Trial, Muscle) %>%
  dplyr::summarise(
    ZC = median(ZC, na.rm = TRUE),
    .groups = "drop"
  )


# 3. Group-level mean + SE (within-subject) across subjects
zc_group <- summarySEwithin(
  data = zc_subj %>%
    mutate(
      Day   = factor(Day),
      Trial = factor(Trial)
    ),
  measurevar = "ZC",
  withinvars = c("Day", "Trial", "Muscle"),
  idvar      = "Subject"
) %>%
  mutate(
    Day   = as.numeric(as.character(Day)),
    Trial = as.numeric(as.character(Trial))
  )

#COGLOAD
ggplot(zc_group,
       aes(x = Trial, y = ZC, color = Muscle, group = Muscle)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ZC - se,
                    ymax = ZC + se),
                width = 0.15,
                linewidth = 0.8) +
  facet_wrap(~ Day, nrow = 1) +
  scale_x_continuous(breaks = 1:5) +  # show Trials 1–5 on x-axis
  labs(
    title = "Tapes Cog Load Zero Crossing by Trial",
    x = "Trial",
    y = "Zero Crossing (ZC)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.text  = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    plot.title  = element_text(size = 16, face = "bold")
  )



#TAPES 
ggplot(zc_group,
       aes(x = factor(Trial),  # x = Trial (discrete)
           y = ZC,
           color = Muscle,
           group = Muscle)) +
  geom_line(aes(group = Muscle), linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ZC - se,
                    ymax = ZC + se),
                width = 0.15,
                linewidth = 0.8) +
  facet_wrap(~ Day, nrow = 1) +
  labs(
    title = "Tapes Zero Crossing by Trial",
    x = "Trial",
    y = "Zero Crossing (ZC)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.text  = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    plot.title  = element_text(size = 16, face = "bold")
  )



# Band Energy
# 1. Base: Tapes only, non-missing/nonnull FractalDim
be_base <- df %>%
  filter(Task == "Tapes_cog_load" ) %>%
  select(Subject, Day, Trial, Muscle, Channel_new, BandEnergy_20_60,BandEnergy_60_120) %>% #,BandEnergy_120_250) %>%
  mutate(
    Day   = as.numeric(as.character(Day)),
    Trial = as.numeric(as.character(Trial))
  )

be_long <- be_base %>%
  pivot_longer(
    cols = starts_with("BandEnergy"),
    names_to = "Band",
    values_to = "BandEnergy"
  )

be_subj <- be_long %>%
  group_by(Subject, Day, Trial, Muscle, Band) %>%
  dplyr::summarise(
    BandEnergy = median(BandEnergy, na.rm = TRUE),
    .groups = "drop"
  )

be_group <- summarySEwithin(
  data = be_subj %>%
    mutate(Day = factor(Day), Trial = factor(Trial)),
  measurevar = "BandEnergy",
  withinvars = c("Day", "Trial", "Muscle", "Band"),
  idvar = "Subject") %>%
  mutate(
    Day   = as.numeric(as.character(Day)),
    Trial = as.numeric(as.character(Trial)))

ggplot(be_group,
       aes(x = factor(Trial),          # X = Trial (discrete)
           y = BandEnergy,
           color = Muscle,
           group = Muscle)) +
  geom_line(aes(group = Muscle), linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = BandEnergy - se,
                    ymax = BandEnergy + se),
                width = 0.15, linewidth = 0.8) +
  facet_grid(Band ~ Day) +            # rows = Band, columns = Day
  labs(
    title = "Tapes Cog Load Band Energy by Trial",
    x = "Trial",
    y = "Band Energy"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.text = element_text(size = 10),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1)
  )

# PEAK FREQUENCY (PKF)
# 1. Base: Tapes only, non-missing/nonnull FractalDim
pkf_base <- df %>%
  filter(
    Task == "Tapes_cog_load",
    !is.na(PKF),
    PKF != 0
  ) %>%
  select(Subject, Day, Trial, Muscle, Channel_new, PKF) %>%
  mutate(
    Day   = as.numeric(as.character(Day)),
    Trial = as.numeric(as.character(Trial))
  ) #%>%
#filter(Day %in% days_of_interest) 

# 2. Subject-level median per Day × Trial × Muscle
pkf_subj <- pkf_base %>%
  group_by(Subject, Day, Trial, Muscle) %>%
  dplyr::summarise(
    PKF = median(PKF, na.rm = TRUE),
    .groups = "drop"
  )


# 3. Group-level mean + SE (within-subject) across subjects
pkf_group <- summarySEwithin(
  data = pkf_subj %>%
    mutate(
      Day   = factor(Day),
      Trial = factor(Trial)
    ),
  measurevar = "PKF",
  withinvars = c("Day", "Trial", "Muscle"),
  idvar      = "Subject"
) %>%
  mutate(
    Day   = as.numeric(as.character(Day)),
    Trial = as.numeric(as.character(Trial))
  )

ggplot(pkf_group,
       aes(x = factor(Trial),        # X = Trial (discrete)
           y = PKF,
           color = Muscle,
           group = Muscle)) +
  geom_line(aes(group = Muscle), linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = PKF - se,
                    ymax = PKF + se),
                width = 0.15,
                linewidth = 0.8) +
  facet_wrap(~ Day, nrow = 1) +
  labs(
    title = "Tapes Cog Load Peak Frequency (PKF) by Trial",
    x = "Trial",
    y = "Peak Frequency (Hz)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.text  = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    plot.title  = element_text(size = 16, face = "bold")
  )

