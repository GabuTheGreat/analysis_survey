#load libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(plotly)

training_data <- cleaned_data[, c('training', 'unit', 'department')]
grouped_training = group_by(training_data, unit)
#unit per training
unit_training <-
  summarise(grouped_training,
            Yes = sum(training == 'Yes'),
            No = sum(training == 'No'))
unit_renamed <- c("C&P", "DL", "FMT", "IS", "PDA", "PDM")
unit_training$unit <- unit_renamed

#reshaped training
shp_training <- melt(unit_training)
shp_training <- filter(shp_training, value > 0)

#plotting unit against
#create a bar plot
p = ggplot(shp_training, aes(unit, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = value),
    fontface = "bold",
    position = position_dodge(width = 1),
    vjust = 0.5
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
