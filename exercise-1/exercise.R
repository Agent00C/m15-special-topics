# Exercise-1
# Implement code from this book chapter: http://r4ds.had.co.nz/many-models.html

# Packages
install.packages('modelr')
install.packages('tidyverse')
install.packages('gapminder')
library(gapminder)
library(modelr)
library(tidyverse)

my.df <- read.csv(file = "EAG_GRAD_ENTR_FIELD_15112016214524613.csv")
stem.df <- my.df %>%  select(Country., Sex, Field,Level.of.education, YEAR, Value) %>% 
  filter(Sex == "Women", 
         Level.of.education == "Total tertiary education (ISCED2011 levels 5 to 8)",
         Field == "Science, mathematics and computing")

# Initial view of the data with ggplot
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

#### View of data for women recieving degrees in STEM at all levels #####
stem.df %>% 
  ggplot(aes(YEAR, Value, colour = Country.)) +
  geom_line(alpha = 1/3) +
  geom_text(aes(label = Country.))

# Look only at new zealand
nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")

## Look at Australia only
oz <- filter(stem.df, Country. == "Australia")
oz %>% 
  ggplot(aes(YEAR, Value)) + 
  geom_line() + 
  ggtitle("Full data = (Australia)")

oz_mod <- lm(Value ~ YEAR, data = oz)
oz %>% 
  add_predictions(oz_mod) %>%
  ggplot(aes(YEAR, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + (Australia)")

oz %>% 
  add_residuals(oz_mod) %>% 
  ggplot(aes(YEAR, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern (Australia)")

# Better yet, write your own function to accept a country as a parameter,
# and produce the same graphics

# Nest the data by country/continent
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

# Define a statistical model, and store it in a function
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

# Use the `map` functionality to run the same model for each country separately
by_country <- by_country %>% 
  mutate(model = map(data, country_model))

# Add additional columns to store your residuals (distance between data and prediction)
by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )

# Unnest your residual
resids <- unnest(by_country, resids)

# Plot the residuals
resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

# Plot residuals by continent
resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~continent)

# Use `glance` to look at model quality
glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)

# Compare model quality to continent
glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(width = 0.5)

# View country that have an r.squared value of less than .25
bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()