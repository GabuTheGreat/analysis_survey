#load libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(plotly)

cleaned_data <- read.csv("~/work/cleaned_capacity.csv", header = TRUE, strip.white=TRUE)#count Unique departments
by_department = group_by(cleaned_data, department)
unique_dpt = summarise(by_department, count = n())

p = ggplot(data = bxplt_data, aes(group, value, fill = group)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.shape = 8,
    outlier.size = 4
  ) +
  theme(
    axis.text.x = element_text(
      colour = "#7F7F7F",
      size = 12,
      face = "bold"
    ),
    axis.title.x = element_text(
      colour = "#FE6F88",
      size = 12,
      face = "bold"
    ),
    axis.text.y = element_text(
      colour = "#7F7F7F",
      size = 12,
      face = "bold"
    ),
    axis.title.y = element_text(
      colour = "#FE6F88",
      size = 12,
      face = "bold"
    ),
    legend.title = element_text(
      colour = "#FE6F88",
      size = 12,
      face = "bold"
    ),
    legend.text = element_text(
      colour = "#7F7F7F",
      size = 9,
      face = "bold"
    )
  )
ggplotly(p) %>% config(displayModeBar = F) %>% layout(legend = list(x = 1, y = 1))
