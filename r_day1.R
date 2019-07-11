library(ggplot2)
data("airquality")
air <- airquality
head(air)
str(air)
ggplot(air, aes(x = Day, y = Temp)) +
  geom_point() +
  theme_light()

data("mtcars")
ggplot(mtcars, aes(x = factor(cyl), fill = factor(gear))) + 
  geom_bar(position = "dodge")

data("economics")
head(economics)

coefs <- coef(lm(psavert ~ date, data = economics))
ggplot(economics, aes(x = date, y = psavert)) +
  geom_line() + geom_abline(intercept = coefs[1], slope = coefs[2])

ggplot(air, aes(x = Day, y = Temp)) +
  geom_point() + geom_text(aes(label = Temp, vjust = -.2, hjust = -.2))

ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + 
  annotate("rect", xmin = 3, ymin = 12, xmax = 4, ymax = 21, alpha = .3, fill = "royalblue") +
  annotate("segment", x = 2.5, xend = 3.7, y = 10, yend = 17, color = "red", arrow = arrow())

ggplot(mtcars, aes(x = gear)) + geom_bar() +
  labs(x = "Gear", y = "Count")


# googleVis
install.packages("googleVis")
library(googleVis)
motion <- gvisMotionChart(economics, idvar = "psavert", timevar = "date")
plot(motion)

gauge <- gvisGauge(CityPopularity, labelvar = "City", numvar = "Popularity", options = list(min = 0, max = 1000))
plot(gauge)

install.packages("ggmap")
library(ggmap)
gg_seoul <- get_googlemap("seoul", maptype = "terrain")
?register_google

