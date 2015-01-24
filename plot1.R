source("load_data.R")

plot1 <- function () {
    yr_em <- nei %>% group_by(year) %>% summarise(emissions=sum(Emissions)/1e6)
    
    png("plot1.png", width = 700, height = 480)
    with(yr_em, 
         plot(year, emissions, main="Total emissions of PM2.5", 
              xlab="Year", ylab="Millions of Tons of PM2.5"))
    abline(lm(emissions ~ year, data=yr_em), col="blue", lty='dashed')
    legend(2006.5,7.2, c("Emissions","Trend"), lty=c("blank","dashed"), 
           pch=c("o",""), lwd=c(2.5,2.5), col=c("black","blue"))
    dev.off()
}

load_data();
plot1()