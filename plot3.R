library(ggplot2)

source("load_data.R")

plot3 <- function () {
    yr_em <- nei %>% filter(fips == "24510") %>% group_by(type,year) %>% 
        summarise(emissions=sum(Emissions))
    
    png("plot3.png", width = 700, height = 480)
    g <- ggplot(yr_em, aes(x=year, y=emissions)) + geom_point(shape=1) +
        facet_grid(. ~ type) + stat_smooth(method="lm", se=FALSE) +
        ggtitle("Total emissions of PM2.5 for Baltimore City by Source Type") +
        xlab("Year") + ylab("Tons of PM2.5")
    print(g)
    dev.off()
}

load_data();
plot3()