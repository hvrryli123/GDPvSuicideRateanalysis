#Peilin Li, the code will clean csv data of the suicide rate, GDP, deflatorrate of 48 countries from 1985 - 2015

rm(list = ls())
dev.off()

install.packages("devtools")
library(devtools)
devtools::install_github("tidyverse/ggplot2")
library(dplyr)
library(ggplot2)

setwd("C:/Users/Harry/Documents/UC San Diego/Freshman Year/Winter/Econ5/Project/Final Project")
df_orig <- read.csv("suicide rate data.csv")
head(df_orig)
cpi <- read.csv("cpidata.csv")

#clean up original data for analysis
names(df_orig)[names(df_orig) == "ï..country"] <- "country"
names(cpi)[names(cpi) == "ï..year"] <- "year"
df_orig$population <- as.numeric(as.character(df_orig$population))
df_orig$gdp_for_year <- as.numeric(as.character(df_orig$gdp_for_year))

#Analysis of original world data
plot(df_orig$gdp_per_capita, df_orig$suicides.100k.pop,
     xlab = "GDP per capita",
     ylab = "Suicide rate per 100k", 
     main = "Suicide/100k pop vs GDP per Capita for the world")

reg_all <- lm(suicides.100k.pop ~ real_gdp_per_capita, data = df_orig)
summary(reg_all)
abline(reg_all)

#Clean up the world data for further analysis
df_orig_clean <- filter(df_orig, suicides.100k.pop != 0.00)
yearrate_all <- aggregate(suicides.100k.pop~country+year, data = df_orig_clean, FUN = sum, na.rm = TRUE)
yeargdp_all <- aggregate(gdp_for_year~country+year , data = df_orig_clean, FUN = mean)
yearpop_all <- aggregate(population~country+year , data = df_orig_clean, FUN = sum)
clean_all <- left_join(yearrate_all, yeargdp_all, by = c("year", "country"))
clean_all <- left_join(clean_all, yearpop_all, by = c("year", "country"))

plot(clean_all$gdp_per_capita, clean_all$suicides.100k.pop,
     xlab = "GDP per capita",
     ylab = "Suicide rate per 100k", 
     main = "Suicide Rate vs GDP per Capita")

reg_all <- lm(suicides.100k.pop ~ gdp_per_capita, data = clean_all)
summary(reg_all)
abline(reg_all)

all_x <- clean_all[4]
all_y <- clean_all[3]
cor(all_x, all_y)

boxplot(suicides.100k.pop ~ year, data = clean_all, 
        main = "Suicide rates from 1985 - 2015",
        ylab = "Suicide rates per 100,000 of the population",
        xlab = "Year"
        )

boxplot(gdp_per_capita~year, data = clean_all,
        #ylim = c(0,60000),
        main = "GDP per capita from 1985 - 2015",
        ylab = "GDP per capita",
        xlab = "Year"
        )

ggplot(clean_all, aes(gdp_per_capita, suicides.100k.pop, colour = country)) + geom_point() + ggtitle("suicide rate vs real GDP per capita 1985 - 2015")

#Apply CPI to selected countries
us_data <- subset(clean_all, country == "United States")
us_data <- left_join(us_data, cpi, by = "year")
us_data <- us_data[-c(7:14)]
us_data <- us_data %>% mutate(real_gdp_per_capita = (gdp_for_year/(US/100)/population))

uk_data <- subset(clean_all, country == "United Kingdom")
uk_data <- left_join(uk_data, cpi, by = "year")
uk_data <- uk_data[-c(6,8:14)]
uk_data <- uk_data %>% mutate(real_gdp_per_capita = (gdp_for_year/(UK/100)/population))

japan_data <- subset(clean_all, country == "Japan")
japan_data <- left_join(japan_data, cpi, by = "year")
japan_data <- japan_data[-c(6:7, 9:14)]
japan_data <- japan_data %>% mutate(real_gdp_per_capita = (gdp_for_year/(Japan/100)/population))

russia_data <- subset(clean_all, country == "Russian Federation")
russia_data <- left_join(russia_data, cpi, by = "year")
russia_data <- russia_data[-c(6:8, 10:14)]
russia_data <- russia_data %>% mutate(real_gdp_per_capita = (gdp_for_year/(Russia/100)/population))

#Analysis of the US data
plot(us_data$real_gdp_per_capita, us_data$suicides.100k.pop,
     xlab = "Real GDP per capita",
     ylab = "Suicide rate per 100k", 
     main = "Suicide rate vs Real GDP per Capita for US")

reg_us <- lm(suicides.100k.pop ~ real_gdp_per_capita, data = us_data)
summary(reg_us)
abline(reg_us)

us_x <- us_data[7]
us_y <- us_data[3]
cor(us_x,us_y)

us_data <- us_data %>% mutate(diff_year = year - lag(year),
                              suicide_growth = suicides.100k.pop - lag(suicides.100k.pop),
                              suiciderate_percent = (suicide_growth/diff_year)/suicides.100k.pop*100)

us_data <- us_data %>% mutate(gdp_growth = real_gdp_per_capita - lag(real_gdp_per_capita),
                              gdprate_percent = (gdp_growth/diff_year)/real_gdp_per_capita*100)


ggplot(us_data, aes(year)) + geom_line(aes(y = gdprate_percent), colour = "green") + geom_line(aes(y = suiciderate_percent), colour = "red") + ggtitle("year on year change suicide rate vs real gdp per capita") + labs(y="percentage")

ggplot(us_data, aes(real_gdp_per_capita, suicides.100k.pop, colour = year)) + geom_point() + ggtitle("suicide rate vs real GDP per capita US 1985 - 2015")

# Analysis of the United Kingdom data

plot(uk_data$real_gdp_per_capita, uk_data$suicides.100k.pop,
     xlab = "Real GDP per capita",
     ylab = "Suicide rate per 100k", 
     main = "Suicide rate vs Real GDP per Capita for UK")

reg_uk <- lm(suicides.100k.pop ~ real_gdp_per_capita, data = uk_data)
summary(reg_uk)
abline(reg_uk)

uk_data <- uk_data %>% mutate(diff_year = year - lag(year),
                              suicide_growth = suicides.100k.pop - lag(suicides.100k.pop),
                              suiciderate_percent = (suicide_growth/diff_year)/suicides.100k.pop*100)

uk_data <- uk_data %>% mutate(gdp_growth = real_gdp_per_capita - lag(real_gdp_per_capita),
                              gdprate_percent = (gdp_growth/diff_year)/real_gdp_per_capita*100)

uk_x <- uk_data[7]
uk_y <- uk_data[3]
cor(uk_x,uk_y)

ggplot(uk_data, aes(year)) + geom_line(aes(y = gdprate_percent), colour = "green") + geom_line(aes(y = suiciderate_percent), colour = "red") + ggtitle("year on year change suicide rate vs real gdp per capita") + labs(y="percentage")

ggplot(uk_data, aes(real_gdp_per_capita, suicides.100k.pop, colour = year)) + geom_point() + ggtitle("suicide rate vs real GDP per capita UK 1985 - 2015")

#Analysis of Japan data
plot(japan_data$real_gdp_per_capita, japan_data$suicides.100k.pop,
     xlab = "Real GDP per capita",
     ylab = "Suicide rate per 100k", 
     main = "Suicide rate vs Real GDP per Capita for Japan")

reg_japan <- lm(suicides.100k.pop ~ real_gdp_per_capita, data = japan_data)
summary(reg_japan)
abline(reg_japan)

japan_x <- japan_data[7]
japan_y <- japan_data[3]
cor(japan_x,japan_y)


japan_data <- japan_data %>% mutate(diff_year = year - lag(year),
                              suicide_growth = suicides.100k.pop - lag(suicides.100k.pop),
                              suiciderate_percent = (suicide_growth/diff_year)/suicides.100k.pop*100)

japan_data <- japan_data %>% mutate(gdp_growth = real_gdp_per_capita - lag(real_gdp_per_capita),
                              gdprate_percent = (gdp_growth/diff_year)/real_gdp_per_capita*100)

ggplot(japan_data, aes(year)) + geom_line(aes(y = gdprate_percent), colour = "green") + geom_line(aes(y = suiciderate_percent), colour = "red") + ggtitle("year on year change suicide rate vs real gdp per capita") + labs(y="percentage")

ggplot(japan_data, aes(real_gdp_per_capita, suicides.100k.pop, colour = year)) + geom_point() + ggtitle("suicide rate vs real GDP per capita Japan 1985 - 2015")


#Analysis of Russia data
plot(russia_data$real_gdp_per_capita, russia_data$suicides.100k.pop,
     xlab = "Real GDP per capita",
     ylab = "Suicide rate per 100k", 
     main = "Suicide rate vs Real GDP per Capita for Russia")

reg_russia <- lm(suicides.100k.pop ~ real_gdp_per_capita, data = russia_data)
summary(reg_russia)
abline(reg_russia)

russia_x <- russia_data[7]
russia_y <- russia_data[3]
cor(russia_x,russia_y)

russia_data <- russia_data %>% mutate(diff_year = year - lag(year),
                                    suicide_growth = suicides.100k.pop - lag(suicides.100k.pop),
                                    suiciderate_percent = (suicide_growth/diff_year)/suicides.100k.pop*100)

russia_data <- russia_data %>% mutate(gdp_growth = real_gdp_per_capita - lag(real_gdp_per_capita),
                                    gdprate_percent = (gdp_growth/diff_year)/real_gdp_per_capita*100)

ggplot(russia_data, aes(year)) + geom_line(aes(y = gdprate_percent), colour = "green") + geom_line(aes(y = suiciderate_percent), colour = "red") + ggtitle("year on year change suicide rate vs real gdp per capita") + labs(y="percentage")

ggplot(russia_data, aes(real_gdp_per_capita, suicides.100k.pop, colour = year)) + geom_point() + ggtitle("suicide rate vs real GDP per capita Russia 1985 - 2015")
