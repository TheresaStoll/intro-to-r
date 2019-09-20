library(tidyverse) #tell R that you want to use the tidyverse packages and functions

read_csv("data/gapminder.csv") #tidyverse function that reads data into R

gapminder <- read_csv("data/gapminder.csv")

nrow(gapminder) #to check how many rows the data has

ncol #to check how many columns the data has

colnames(gapminder) #to show the names of the columns

glimpse(gapminder) #to show a glimpse of the data

#chr = characters, text
#dbl = double = numbers 
#ord = ordered factor (numbers that have been ordered)
#int = intenger (whole numbers, not fractional or decimal numbers)

nrow(storms)
ncol(storms)
colnames(storms)
glimpse(storms)

select(gapminder, year, country, pop)
select(gapminder, 3, 1, 5)
select(gapminder, 3:5)
select(gapminder, -lifeExp, -pop)
select(gapminder, year:lifeExp)
select(gapminder, -(year:lifeExp))

just_population <- select(gapminder, year, country, pop)                                                                                                                                                                                                               
just_population

select(gapminder, country)
select(gapminder, country, year, pop, gdpPercap)
select(gapminder, 1, 3, 5, 6)
select(gapminder, -continent, -lifeExp)

select(gapminder, starts_with("co"))
select(gapminder, contains("e"))                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
select(gapminder, ends_with("p"))
select(gapminder, endsWith("p"))

select(gapminder, population = pop, country)
rename(gapminder, population = pop)

filter(gapminder, country == Australia)
filter(gapminder, country =="Australia")
filter(gapminder, year >=1997)

filter(gapminder, lifeExp >=80)
filter(gapminder, continent == "Europe")
filter(gapminder, lifeExp >=80, gdpPercap >=30.000)
filter(gapminder, lifeExp >= 80 | gdpPercap >= 30.000)
filter(gapminder, continent %in% c("Africa", "Asia", "Europe"))

select(gapminder, continent == "Australia")
select(gapminder, country == "Australia")
filter(gapminder, country == "Australia")
select(gapminder, country, year, pop)

gapminder_new <- filter(gapminder, country == "Australia")
select(gapminder_new, country, year, pop)

select(filter(gapminder, country == "Australia"), country, year, pop)
filter(select(gapminder, country, pop), country == "Australia")

select(gapminder, year, pop)        
gapminder %>% select(year, pop)        

filter(gapminder, country == "Australia", year >- 1997)
small <- gapminder %>% 
  filter(country == "Australia", year >- 1997)
small

filter(select(gapminder, country, year, pop), country == "Australia")

large <- gapminder %>%
  filter(country == "Australia") %>%
  select(country, year, pop)

#mutate = create new columns

mutate(gapminder, gdp = gdpPercap * pop)
# want to create the new column called "gdp" - how does it create it?
#mutata creates the column gdp by multiplying gdpPercap with gdp

mutate(gapminder, pop_M = pop/1000000)

#can also take logarithms inside functions such as mutate
#the following will add a new column in which the logarith of pop will be taken:
mutate(gapminder, log_of_pop = log(pop))

#str_sub = string subset function
# takes the text we provided 
#and will extract from this text and show the first character to the fifth character
str_sub("A long bit of text", start = 1, end = 5)

#tell string subset function to look in the column 'country' for the text we want
#tell string subset, it should only use the first until the third characters for text
mutate(gapminder, country_abbr = str_sub(country, start = 1, end = 3))

#tells you how long the word in brackets is
str_length("some words")

#new column with the number of characters in a country's name
mutate(gapminder, characters = str_length(country))

mutate(gapminder, gdp = gdpPercap * pop, log_of_pop = log(pop))

mutate(
  gapminder,
  gdp = gdpPercap * pop,
  log_of_pop = log(pop)
)

mutate(
  gapminder,
  gdp = gdpPercap * pop,
  log_of_gdp = log(gdp)
)

mutate(
  gapminder,
  lifeExp_days = lifeExp * 365,
  gdp_billions = gdpPercap * pop / 1000000000
)

#summarising data
summarise(gapminder, mean_life_exp = mean(lifeExp))

summarise(
  gapminder, 
  mean_life_exp = mean(lifeExp), 
  sd_life_exp = sd(lifeExp), 
  biggest_gdp = max(gdpPercap)
  )       

#mean of population, median of the population

summarise(
  gapminder,
  mean_pop = mean(pop),
  median_pop = median(pop)
)

summarise_if(gapminder, is.numeric, mean)

group_by(gapminder, country)

by_country <- group_by(gapminder, country)
by_country

summarise(gapminder, mean_pop = mean(pop))       
summarise(by_country, mean_pop = mean(pop))

#Challenge 2: to show the mean and median population for each continent
by_continent <- group_by(gapminder, continent)
by_continent
summarise(by_continent, mean_pop = mean(pop), median_pop = median(pop))

#to use Pipe to do the same/to show the mean and median population of each continent
by_continent %>% summarise(mean_pop = mean(pop), median_pop = median(pop))

gapminder %>%
  group_by(continent) %>%
  summarise(mean_pop = mean(pop), median_pop = median(pop))

arrange(gapminder, gdpPercap)
arrange(gapminder, desc(gdpPercap))  

#Challenge 3 p.42
gapminder_by_country <- group_by(gapminder, country)
summarised_lifeExp <- summarise(gapminder_by_country, mean_lifeExp = mean(lifeExp))
summarised_lifeExp
arrange(summarised_lifeExp, mean_lifeExp)

by_country %>%
  summarise(mean_lifeExp = mean(lifeExp)) %>%
  arrange(mean_lifeExp)

#get gapmidner data
# group data by country
# mean life exp by group
# order result

gapminder %>%
  group_by(country) %>%
  summarise(mean_lifeExp = mean(lifeExp)) %>%
  arrange(mean_lifeExp)

# number of rows we have in gapminder data  
summarise(gapminder, num_rows = n())

#how many rows of data we have for each country
counts <- summarise(by_country, num_rows = n())

view(counts)

summarise(by_country, num_rows = n()) %>%
  view()

# Challenge 4 p. 43
gap_by_cont_year <- group_by(gapminder, continent, year)
summarised_gdp <- summarise(gap_by_cont_year, mean_gdp_per_cap = mean(gdpPercap))
arrange(summarised_gdp, desc(year), desc(mean_gdp_per_cap))
arrange(summarised_gdp, desc(mean_gdp_per_cap), desc(year))

#read in excel files into R

read_csv("data/gapminder.xlsx")

read_excel("data/gapminder.xlsx")

library(readxl)

read_excel("Data/gapminder.xlsx")


        