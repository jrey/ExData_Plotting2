# Load the homework data, since it is a low operation data is read in global
# variables scc & nei if they does not exist.

load_data <- function () {    
    if ( !exists("scc") ) {
        scc <<- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")    
    }
    if ( !exists("nei") ) {
        nei <<- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")    
        nei$SCC <- as.factor(nei$SCC)
    }
}
