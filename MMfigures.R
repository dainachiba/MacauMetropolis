##-------------------------------------------------------------------------
## R code to produce figures for Macau Metropolis
## Written by: Daina Chiba
## Last modified: 2021-09-24
##-------------------------------------------------------------------------

pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

library(tidyverse)
library(stargazer)
library(hrbrthemes)
library(here)

theme_set(theme_light())

data("iris")

# UM colors (red, green, blue, yellow)
um_colors <- c("#F53E41", "#00AA94", "#005F96", "#FFD446")


p <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, 
                      color = Species))
p + geom_point() + geom_smooth() + 
  facet_wrap(~Species) + guides(color = "none") + 
  scale_color_manual(values = um_colors) + 
  labs(x = "Sepal Width", y = "Sepal Length", 
       caption = "Source: Iris data") + 
  theme(
    panel.background = element_rect(fill = "transparent", 
                                    color = NA),
    plot.background = element_rect(fill = "transparent", 
                                   color = NA))
ggsave(here("figures", "iris.pdf"), width = 8, height = 4)



# regression --------------------------------------------------------------

fit.0 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, 
            data = iris)
fit.1 <- update(fit.0, data = subset(iris, Species == "setosa"))
fit.2 <- update(fit.0, data = subset(iris, Species == "versicolor"))
fit.3 <- update(fit.0, data = subset(iris, Species == "virginica"))

library(stargazer)
stargazer(fit.1, fit.2, fit.3, type = "text")

