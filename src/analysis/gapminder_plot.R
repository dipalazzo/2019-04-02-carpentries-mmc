library(here)
library(tidyverse)

gapminder <- readr::read_csv(here("data/gapminder/raw/gapminder_data.csv"))

head(gapminder)

mean(gapminder$gdpPercap[gapminder$continent=="Africa"])

mean(gapminder$gdpPercap[gapminder$continent=="Americas"])

mean(gapminder$gdpPercap)

year_country_gdp <- select(gapminder, year, country, gdpPercap)

year_country_gdp2 <- gapminder %>% 
  filter(continent=="Africa") %>% 
  select(lifeExp, year, country)  

year_country_gdp3 <- gapminder %>% 
  group_by(continent) %>% 
  summarize(mean_val = mean(gdpPercap))
    
year_country_gdp4 <- gapminder %>% 
  group_by(country) %>% 
  summarize(mean_le = mean(lifeExp)) %>% 
  filter(mean_le == max(mean_le))

ggplot(data=gapminder, aes(x=year, y=lifeExp, color=continent)) +
  geom_line() +
  facet_wrap(~ country)
