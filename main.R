library(readr)
library(psych)
library(corrplot)
library(tidyverse)
library(ggplot2)
library(knitr)
library(plotly)
library(ggthemes)
library(DT)
library(scales)
library(DataExplorer)

df <- read_csv("1553768847-housing.csv")

# Data Distribution Visualization
#Density plot
distribution_graph <- function(display_value, break_down){ 
  ggplot(df, aes(x = df[[display_value]], color = if (break_down) ocean_proximity else NULL)) +
  geom_density()+
  labs(
    title = paste0('Density Plot of ', display_value),
    x = '',
    y = '',
    color = 'Ocean Proximity',
    fill = ""
  )+
  scale_y_continuous(labels = label_number())+
  scale_x_continuous(labels = label_number())+
  theme_bw()+
  theme(
    plot.title = element_text(size = 12),
    legend.background = element_rect(
      fill = "white", 
      linewidth = 4, 
      colour = "white"
    ),
    axis.ticks = element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.major = element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    legend.position = if (break_down) NULL else "none"
  )
}



#Box-Wishker Plot
box_graph <- function(display_value){ 
  ggplot(df, aes(x = df[[display_value]])) +
    geom_boxplot()+
    labs(
      title = paste0('Box Plot of ', display_value),
      x = '',
      y = '',
    )+
    scale_y_continuous(labels = label_number())+
    scale_x_continuous(labels = label_number())+
    theme_bw()+
    theme(
      plot.title = element_text(size = 17))
}








