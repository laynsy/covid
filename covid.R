library(sqldf)
library(ggplot2)
library(dplyr)
library(scales)
library(wesanderson)
library(tidyr)
library("reshape2")

covid <- read.table('us-states.txt', sep=",", header=TRUE, na.strings = c("NA", '', ' ', "NAN"))

covidCA <- sqldf("SELECT date, cases, deaths
                 FROM covid
                 WHERE fips = 6")

covidCA %>%
  gather(key, value, cases, deaths)  %>% 
  ggplot(aes(x=date, y=value, colour=key)) +
  geom_point()

## alternative method below ##
covidCA_long <- melt(covidCA, id="date")
covidCA_long <- sqldf("SELECT *
                      FROM covidCA_long
                      ORDER BY date")
ggplot(data=covidCA_long,
       aes(x=date, y=value, colour=variable)) +
  geom_point()
## alternative method above ##

## let's look at just non-zero cases ##
covidCAnz <- sqldf("SELECT *
                   FROM covidCA
                   WHERE cases > 0
                   ORDER BY date")
covidCAnz %>%
  gather(key, value, cases, deaths)  %>% 
  ggplot(aes(x=date, y=value, colour=key)) +
  geom_point()

## let's look at the log so we can see the earlier data ##
covidCAlog <- data.frame(covidCA$date, log(covidCA$cases), log(covidCA$deaths))
names(covidCAlog) <- c("date", "cases", "deaths")
## fix Infs 
covidCAlog <- covidCAlog %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
## fix dates
covidCAlog$date <- as.Date(strptime(covidCAlog$date, format="%Y-%m-%d"))

covidCAlog %>%
  gather(key, value, cases, deaths)  %>% 
  ggplot(aes(x=date, y=value, colour=key)) +
  geom_point() +
  scale_x_date(date_breaks = "weeks" , date_labels = "%b-%d")
