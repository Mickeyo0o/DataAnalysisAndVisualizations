library(plotly)
library(dplyr)
library(ggplot2)
library(zoo)
library(stringr)
library(ggplot2)
library(ggExtra)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(plotly)
df = read.csv("reviews.csv")


data <- data.frame(name = df$userName, 
                   time = df$at, 
                   content = df$content, 
                   score = df$score, 
                   thumbs = df$thumbsUpCount) %>% 
  filter(thumbs > 0) %>%
  filter(thumbs >= max(thumbs) * (1-0.95)) %>%
  mutate(time = str_extract(time, "^[^\\s]+")) %>%
  mutate(time = as.Date(time)) %>%
  mutate(color = ifelse(score == "1", "#ff0000", 
                        ifelse(score == "2", "#ffa700",
                               ifelse(score == "3", "#fff400",
                                      ifelse(score == "4", "#a3ff00", "#2cba00"))))) %>%
  filter(score %in% df5$selectedScores)
