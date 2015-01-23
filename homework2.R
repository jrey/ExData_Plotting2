library(dplyr)

scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
nei <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")

nei$SCC <- as.factor(nei$SCC)

# Question 1:
    
yr_em <- nei %>% group_by(year) %>% summarise(emissions=sum(Emissions)/1e6)

mp <- barplot(yr_em$emissions, yr_em$year, main="Total emissions of PM2.5",
              xlab="Year", ylab="Millions of Tons of PM2.5")
axis(1, at=mp, labels=yr_em$year)

with(yr_em, 
     plot(year, emissions, main="Total emissions of PM2.5", 
     xlab="Year", ylab="Millions of Tons of PM2.5"))
abline(lm(emissions ~ year, data=yr_em), col="blue", lty='dashed')
legend(2006.5,7.2, c("Emissions","Trend"), lty=c("blank","dashed"), 
       pch=c("o",""), lwd=c(2.5,2.5), col=c("black","blue"))

# Question 2

library(ggplot2)

bl_yr_em <- nei %>% filter(fips == "24510") %>% group_by(year) %>%
    summarise(emissions=sum(Emissions)/1e3)

qplot(year, emissions, data = ty_yr_em, xlab="Year", ylab="Thousands of Tons of PM2.5")

with(ty_yr_em, 
     plot(emissions ~ year, xlab="Year", ylab="Thousands of Tons of PM2.5",
          main="Total emissions of PM2.5 for Baltimore City"))
abline(lm(emissions ~ year, data=bl_yr_em), col="blue", lty='dashed')
legend(2006.5,3.3, c("Emissions","Trend"), lty=c("blank","dashed"), 
       pch=c("o",""), lwd=c(2.5,2.5), col=c("black","blue"))

# Question 3

ty_yr_em <- nei %>% filter(fips == "24510") %>% group_by(type,year) %>% 
    summarise(emissions=sum(Emissions))

ggplot(ty_yr_em, aes(x=year, y=emissions)) + geom_point(shape=1) +
    facet_grid(. ~ type) + stat_smooth(method="lm", se=FALSE) +
    ggtitle("Total emissions of PM2.5 for Baltimore City by Source Type") +
    xlab("Year") + ylab("Tons of PM2.5")

# Question 4

scc1 <- scc[grep('coal',scc$Short.Name, ignore.case = TRUE),] %>% select(SCC)

q4 <- scc1 %>% inner_join(nei, by="SCC") %>% group_by(year) %>%
    summarise(emissions=sum(Emissions)/1e3)

with(q4, 
     plot(year, emissions, main="Total emissions of PM2.5 from Coal Related Sources", 
          xlab="Year", ylab="Millions of Tons of PM2.5"))
abline(lm(emissions ~ year, data=q4), col="blue", lty='dashed')
legend(2006.5,560, c("Emissions","Trend"), lty=c("blank","dashed"), 
       pch=c("o",""), lwd=c(2.5,2.5), col=c("black","blue"))

# Question 5

scc2 <- scc[grep('veh',scc$Short.Name, ignore.case = TRUE),]  %>% select(SCC)

q5 <- scc2 %>% inner_join(nei %>% filter(fips == "24510"), by="SCC") %>%
    group_by(year) %>%
    summarise(emissions=sum(Emissions))

with(q5, 
     plot(year, emissions, main="Total emissions of PM2.5 from Motor Vehicle Sources", 
          xlab="Year", ylab="Tons of PM2.5"))
abline(lm(emissions ~ year, data=q5), col="blue", lty='dashed')
legend(2006.5,350, c("Emissions","Trend"), lty=c("blank","dashed"), 
       pch=c("o",""), lwd=c(2.5,2.5), col=c("black","blue"))

# Question 6

nei2 <- nei %>% filter(fips == "24510" | fips == "06037") %>%
    mutate(city=ifelse(fips=="24510","Baltimore","Los Angeles"))

q6 <- scc2 %>% inner_join(nei2, by="SCC") %>%
    group_by(city, year) %>%
    summarise(emissions=sum(Emissions))

ggplot(q6, aes(x=year, y=emissions)) + geom_point(shape=1) +
    facet_grid(. ~ city) + stat_smooth(method="lm", se=FALSE) +
    ggtitle("Total emissions of PM2.5 for Baltimore City by Source Type") +
    xlab("Year") + ylab("Tons of PM2.5")

