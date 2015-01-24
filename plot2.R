source("load_data.R")

plot2 <- function () {
    yr_em <- nei %>% filter(fips == "24510") %>% group_by(year) %>%
        summarise(emissions=sum(Emissions)/1e3)
    
    #qplot(year, emissions, data = yr_em, xlab="Year", ylab="Thousands of Tons of PM2.5")
    
    png("plot2.png", width = 700, height = 480)
    with(yr_em, 
         plot(emissions ~ year, xlab="Year", ylab="Thousands of Tons of PM2.5",
              main="Total emissions of PM2.5 for Baltimore City"))
    abline(lm(emissions ~ year, data=yr_em), col="blue", lty='dashed')
    legend(2006.5,3.3, c("Emissions","Trend"), lty=c("blank","dashed"), 
           pch=c("o",""), lwd=c(2.5,2.5), col=c("black","blue"))
    dev.off()
}

load_data();
plot2()