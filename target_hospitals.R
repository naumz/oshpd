cat('\014')
library(data.table)
mydata <- fread('ohspd_surgery_2015_report_card.csv')

# find academic hospitals
mydata[, is.academic := 0]
mydata[facility.owner %like% "UNIVERSITY" |
         facility.owner %like% "UC" |
         facility.hospital.name %like% "USC" |
         facility.hospital.name %like% "UCLA" |
         facility.hospital.name %like% "UNIVERSITY",
       is.academic := 1]

# break-down hospitals by size
hospital_breaks <- c(0,6,12,18,50)
hist(mydata[, operating.rooms], breaks=hospital_breaks, freq=TRUE)

# hospitals owned by big groups (>=4 hospitals by the same owner or owned by HCA)
hospital_management_data <- mydata[,
                                   .(facility.owner, .N),
                                   by=facility.owner][N>=4 |
                                                        facility.owner %like% "^HCA"][order(-N)]
mydata[, is.management.owned := 0]
mydata[, is.management.owned := ifelse(.N>=4 |
                                         facility.owner %like% "^HCA", 1, 0),
       by=facility.owner]
mydata.filtered <- mydata[is.management.owned == 0 & 
                            is.academic == 0 & 
                            operating.rooms >= 5 & 
                            operating.rooms <= 20, ]
hist(mydata.filtered[, operating.rooms], breaks=hospital_breaks, freq=TRUE)
 