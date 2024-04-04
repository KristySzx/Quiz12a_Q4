install.packages("ggplot2")
library(ggplot2)
install.packages("rstanarm")
library(rstanarm)

lambda <- 50
data <- data.frame(
  year = rep(2002:2021, time = 5),
  hospital = rep(1:5, each = 20),
  deaths = rpois(100, lambda)
)

mean_deaths <- mean(data$deaths)
var_deaths <- var(data$deaths)
test_statistic <- (var_deaths - mean_deaths) / sqrt(2 * mean_deaths / length(data$deaths))
p_value <- 2 * (1 - pnorm(abs(test_statistic)))

ggplot(data, aes(x = year, y = deaths, fill = factor(hospital))) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Cancer Deaths per Year in Sydney Hospitals", x = "Year", y = "Total Number of Cancer Deaths") + 
  theme_minimal()

model <- glm(deaths ~ year + factor(hospital), data = data, family = poisson(link = "log"))
summary(model)

