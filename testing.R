library(plotly)
library(dplyr)
library(ggplot2)
library(zoo)
library(stringr)

df = read.csv("reviews.csv")
df1 <- data.frame(name = df$userName, time = df$at, content = df$content, 
                  score = df$score, thumbs = df$thumbsUpCount) %>% 
  filter(thumbs > 0) %>%
  filter(thumbs >= max(.$thumbs) * (1-0.95)) %>%
  mutate(time = str_extract(time, "^[^\\s]+")) %>%
  mutate(time = as.Date(time))



# classic plot :
p <- ggplot(df1, aes(x=time, y=score, size=thumbs)) +
  geom_point() +
  theme(legend.position="none") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d") 

p = ggplotly(p)

p2 <- ggplot(df1, aes(x = score)) +
  geom_density(fill = "lightblue", color = "black") +
  theme_void() +
  ylab("Density") +
  coord_flip()


p2 <- ggplotly(p2)
p2

final = subplot(p, p2, margin = 0)
final
