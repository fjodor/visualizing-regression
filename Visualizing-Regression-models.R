# Visualizing regression models in R

rm(list = ls())

library(ggplot2)
library(dplyr)
library(broom)
library(plotly)

data(mtcars)
str(mtcars)

# Modelle zur Erklärung des Verbrauchs in miles per gallon

mod1 <- lm(mpg ~ hp, data = mtcars)

# Lineare Einfach-Regression

library(ggplot2)
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  labs(x = "PS (horsepower)", y = "Verbrauch in miles per gallon\n(Je höher, desto sparsamer)",
       title = "lm(mpg ~ hp), data = mtcars")

# Zwei parallele Regressionsgeraden: parallel slopes model

library(dplyr)
mtcars$am <- factor(mtcars$am)
mtcars$am <- recode(mtcars$am, "0" = "Automatik", "1" = "Schaltgetriebe")

mod2 <- lm(mpg ~ hp + am, data = mtcars)

library(ggplot2)
library(broom)
ggplot(augment(mod2), aes(x = hp, y = mpg, color = am)) +
  geom_point() +
  geom_line(aes(y = .fitted)) +
  labs(x = "PS (horsepower)", y = "Verbrauch in miles per gallon\n(Je höher, desto sparsamer)",
       title = "lm(mpg ~ hp + am, data = mtcars)")

# Zwei Regressionsgeraden mit Interaktionseffekt

mod2b <- lm(mpg ~ disp * am, data = mtcars)
summary(mod2b)

ggplot(mtcars, aes(x = disp, y = mpg, color = am)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "disp (Verdrängung / Hubraum)", y = "mpg - Verbrauch in miles per gallon\n(Je höher, desto sparsamer)",
       title = "mpg ~ disp separat nach am, data = mtcars")

ggplot(augment(mod2b), aes(x = disp, y = mpg, color = am)) +
  geom_point() +
  geom_line(aes(y = .fitted), size = 1) +
  labs(x = "disp (Verdrängung / Hubraum in cubic inch)",
       y = "mpg - Verbrauch in miles per gallon\n(Je höher, desto sparsamer)",
       title = "lm(mpg ~ disp * am, data = mtcars)")


# Zwei numerische Prädiktoren: 3D-Modell

mod3 <- lm(mpg ~ hp + disp, data = mtcars)

hp <- mtcars$hp
disp <- mtcars$disp

grid <- expand.grid(hp, disp)
d <- setNames(data.frame(grid), c("hp", "disp"))
vals <- predict(mod3, newdata = d)

mpg <- matrix(vals, nrow = length(d$hp), ncol = length(d$disp))
plane <- mpg

rm(d, grid, vals)

library(plotly)

# Just plane: plane
plot_ly() %>%
  add_surface(z = ~plane, x = ~disp, y = ~hp) %>%
  layout(showlegend = FALSE)

# Just plane: mpg
plot_ly() %>%
  add_surface(z = ~mpg, x = ~disp, y = ~hp) %>%
  layout(showlegend = FALSE)

# Just markers
plot_ly() %>%
  add_markers(data = mtcars, z = ~mpg, x = ~disp, y = ~hp, opacity = 0.6)

library(plotly)

# Markers and plane
p <- plot_ly(data = mtcars, z = ~mpg, x = ~disp, y = ~hp, opacity = 0.6) %>%
  add_markers()
p %>% add_surface(z = ~plane, x = ~disp, y = ~hp, showscale = FALSE) %>%
  layout(showlegend = FALSE)

rm(plane, plane2, p, disp, hp, mpg, m)


#############################################################
# Diagnostic plots

par(mfrow = c(2, 2))
png()
plot(mod3)
par(mfrow = c(1, 1))

# Better: Use ggplot2

library(ggfortify)
autoplot(mod3)
