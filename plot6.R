library(ggplot2)

source("load_data.R")

plot6 <- function () {
    nei2 <- nei %>% filter(fips == "24510" | fips == "06037") %>%
        mutate(city=ifelse(fips=="24510","Baltimore","Los Angeles"))
    
    scc2 <- scc[grep('veh',scc$Short.Name, ignore.case = TRUE),]  %>% select(SCC)
    
    q6 <- scc2 %>% inner_join(nei2, by="SCC") %>%
        group_by(city, year) %>%
        summarise(emissions=sum(Emissions))
    
    png("plot6.png", width = 700, height = 480)
    g <- ggplot(q6, aes(x=year, y=emissions)) + geom_point(shape=1) +
        facet_grid(. ~ city) + stat_smooth(method="lm", se=FALSE) +
        ggtitle("Total emissions of PM2.5 for Baltimore City by Source Type") +
        xlab("Year") + ylab("Tons of PM2.5")
    print(g)
    dev.off()
}

load_data();
plot6()