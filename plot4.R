source("load_data.R")

plot4 <- function () {
    scc1 <- scc[grep('coal',scc$Short.Name, ignore.case = TRUE),] %>% select(SCC)
    
    q4 <- scc1 %>% inner_join(nei, by="SCC") %>% group_by(year) %>%
        summarise(emissions=sum(Emissions)/1e3)
    
    png("plot4.png", width = 700, height = 480)
    with(q4, 
         plot(year, emissions, 
              main="Total emissions of PM2.5 from Coal Related Sources", 
              xlab="Year", ylab="Millions of Tons of PM2.5"))
    abline(lm(emissions ~ year, data=q4), col="blue", lty='dashed')
    legend(2006.5,560, c("Emissions","Trend"), lty=c("blank","dashed"), 
           pch=c("o",""), lwd=c(2.5,2.5), col=c("black","blue"))
    dev.off()
}

load_data();
plot4()