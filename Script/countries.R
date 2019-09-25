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

gapminder_excel <- read_excel("Data/gapminder.xlsx")

read_excel("data/gapminder.xlsx", range = "A1:E4")

read_excel("data/gapminder.xlsx", sheet = "gapminder")

#writing data out

write_csv(gapminder_excel, "results/gapminder_output.csv")

#write out just the Australian data from gapminder 

gapminder_Australia <- filter(gapminder, country == "Australia")
write_csv(gapminder_Australia, "results/gapminder_output_Australia.csv")

#another way to do it with piping
gapminder %>%
  filter(country == "Australia") %>%
  write_csv("results/australian_data_2.csv")


# Constructing an analysis challenge 
# countries that are in the top 10 life expectancy lists in 1987 and 2007

gapminder # start with gapminder
filter # just work with data from 1987 and from 2007
group_by
arrange # sort our data by life expectancy (needs to be done by year)
top_n(10) # get just the top ten (bye year)
summarising with n # count the number of times a country appears
filter # just keep the countries that appear twice

gapminder %>%
  filter(year == 1987, year == 2007) %>%
  group_by(year) %>%
  arrange(lifeExp) %>%
  top_n(10) %>%
  group_by(country) %>%
  n() 

#day 5 (25 September) stuff

gapminder %>%
  filter(year == 1957) %>% # Filter for 1957
  group_by(continent) %>% # Group by continent 
  summarise(max_gdp = max(gdpPercap)) # Summarise to find the maximum gdp per cap

gapminder_2012 <- read_csv(file="data/gapminder_2012.csv")
gapminder_2012

?bind_rows() #to get help function and look at examples especially!

# Adding & Combining datasets: Challenge 1 p.46
combined_gapminder <- bind_rows(gapminder, gapminder_2012)
combined_gapminder #creates a bigger dataframe with the same columns but additional rows
# this works if datasets have same number of columns (in this example, the columns also have the same names)

renamed_2012 <- rename(gapminder_2012, population = pop)
renamed_2012

mismatched_names <- bind_rows(gapminder, renamed_2012)
mismatched_names

tail(mismatched_names)

# different ways to combine dataframes: https://csiro-data-school.github.io/r/10-Data-Verbs---join/index.html

# joins lesson (different join functions)

example_vector <- c(1,4,2,7)
example_vector

string_vector <- c('hello', 'this', 'is', 'a', 'vector')
string_vector

broken_vector <- c('hello', 2)
broken_vector

df1 <- tibble(sample = c(1,2,3), measure1 = c(4.2, 5.3, 6.1))
df1

df2 <- tibble(sample = c(1,3,4), measure2 = c(7.8, 6.4, 9.0))
df2

inner_join(df1, df2)

df3 <- tibble(ID = c(1,2,4), measure3 = c(4.7, 34, 2.6))
df3

inner_join(df1, df2)
full_join(df1, df3, by = c("sample" = "ID"))


full_join(df1, df2)

left_join(df2, df1)

cows <- tibble(id = c(1, 2, 3),
        weight1 = c(203, 227, 193),
        weight2 = c(365, 344, 329))
cows

cows_tidy <- gather(cows, rep, weight, -id)              
cows_tidy              

cows_tidy %>%
  arrange(id)

spread(cows_tidy, rep, weight)

#gather

# new dataframe with a column for year, column for number, another column for country
# year, number, country

table4a

table4_tidy <- gather (table4a, year, number, -country)
table4_tidy

table4_tidy %>%
  arrange(country)

# this all does the same:
gather(table4a, year, pop, -country) # same as
gather(table4a, year, pop, 2:3) # identify which columsn to do this on
gather(table4a, year, pop, "1999", "2000") # can name the column headers

#spread your gathered table4a
#spread table2

spread(table4_tidy, year, number)

table2
spread(table2, type, count)

# separate
cows_with_breed <- cows %>% mutate(id = c("1_A", "2_A", "3_B"))
cows_with_breed

separate(cows_with_breed, col = id, into = c("ID", "breed"))

separate(cows_with_breed, col = id, into = c("ID", "breed"), sep = "_")

separate(cows_with_breed, id, c("ID", "breed"), "_")




