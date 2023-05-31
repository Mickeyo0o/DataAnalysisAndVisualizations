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
df = read.csv("reviews.csv")
df1 <- data.frame(score = df$score, time = df$at, reply = df$repliedAt) %>% 
  group_by(score) %>%
  summarize(percentage = sum(!is.null(reply)) / n() * 100) %>%
  mutate(color = ifelse(score == "1", "#ff0000", 
                        ifelse(score == "2", "#ffa700",
                               ifelse(score == "3", "#fff400",
                                      ifelse(score == "4", "#a3ff00", "#2cba00")))))


p1 <- ggplot(df1, aes(x = score, y = percentage)) +
  geom_bar(stat = "identity", fill = df1$color) +
  labs(x = "Grade", y = "Percentage of Reviews with Reply") +
  theme_minimal() 

ggplotly(p1)

