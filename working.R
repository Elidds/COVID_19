# Tittle: covid personal project

# install and load packages

install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse,
               janitor,
               rstatix,
               flextable,
               ggplot2,
               dplyr)
install.packages("tinytex")
library(tinytex)

# Import dataset into Rstudio
library(readxl)
Covid_dataset <- read_excel("Covid dataset.xlsx")
View(Covid_dataset)

# Data exploration
covid_data<-Covid_dataset
colnames(covid_data)
nrow(covid_data)
str(covid_data)
dim(covid_data)
summary(covid_data)


# Conduct statistical analysis
# calculate mean, median, and standard deviation of cases
mean(cleaned_data$population)
mean(cleaned_data$total_cases)

mean_cases <- mean(cleaned_data$total_cases)
median(cleaned_data$total_cases)
sd(cleaned_data$total_cases)
summary(cleaned_data)

## Data cleaning and manipulation
# Remove rows with any NA values
cleaned_data <- na.omit(covid_data)
print(cleaned_data)

# Total population per continent
population_table <- cleaned_data %>%
  group_by(continent) %>%
  summarise(total_population = sum(population))
print(population_table)

# Total death cases per continent
deaths_table <- cleaned_data %>%
  group_by(continent) %>%
  summarise(total_deaths = sum(total_deaths))

# Total cases per continent
cases_table <- cleaned_data %>%
  group_by(continent) %>%
  summarise(total_cases = sum(total_cases))


aggregate(covid_data$population ~ covid_data$continent, FUN = mean)
aggregate(covid_data$population ~ covid_data$continent, FUN = max)

## Data description and summarization
# Join the tables
joined_table <- left_join(population_table, deaths_table, by = "continent") %>%
  left_join(cases_table, by = "continent")

# Print the joined table
print(joined_table)

cleaned_data %>% 
  group_by(continent, population) %>%
  summarise(total_cases = n(),
            average_cases = mean(population)) %>% 
  arrange(continent, population) %>%
  View()
summary <- cleaned_data %>% 
  group_by(continent, population) %>%
  summarise(total_cases = n(),
            average_cases = mean(population)) %>% 
  arrange(continent, population)


## Data visualization
# Visualize total population per continent

population_plot <- ggplot(joined_table, aes(x = continent, y = total_population)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Population per Continent",
       x = "Continent",
       y = "Total Population")
ggsave("population_plot.png", plot = population_plot)
# Visualize total death cases per continent

Death_plot <- ggplot(joined_table, aes(x = continent, y = total_deaths)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(title = "Total Death Cases per Continent",
       x = "Continent",
       y = "Total Death Cases")
ggsave("Death_plot.png", plot = Death_plot)
# Visualize total cases per continent

Cases_plot <- ggplot(joined_table, aes(x = continent, y = total_cases)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Total Cases per Continent",
       x = "Continent",
       y = "Total Cases")
ggsave("Cases_plot.png", plot = Cases_plot)

ggplot(summary, aes(x = continent, y = average_cases)) +
  geom_histogram(stat = "identity", fill = "red") +
  labs(title = "Average Cases per Continent",
       x = "Continent",
       y = "Average Cases")
# Inferential Statistics
# Automated report