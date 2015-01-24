source("load_data.R")

plot5 <- function () {
    scc2 <- scc[grep('veh',scc$Short.Name, ignore.case = TRUE),]  %>% select(SCC)
    
    q5 <- scc2 %>% inner_join(nei %>% filter(fips == "24510"), by="SCC") %>%
        group_by(year) %>%
        summarise(emissions=sum(Emissions))
    
    png("plot5.png", width = 700, height = 480)
    with(q5, 
         plot(year, emissions,
              main="Total emissions of PM2.5 from Motor Vehicle Sources", 
              xlab="Year", ylab="Tons of PM2.5"))
    abline(lm(emissions ~ year, data=q5), col="blue", lty='dashed')
    legend(2006.5,350, c("Emissions","Trend"), lty=c("blank","dashed"), 
           pch=c("o",""), lwd=c(2.5,2.5), col=c("black","blue"))
    dev.off()
}

load_data();
plot5()