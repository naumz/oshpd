# Create surgery relevant dataset from OSHPD annual financial disclosure
#
# Date: January 20, 2017
#
# Author: Naumaan Nayyar

#### ENVIRONMENT ####
# clear workspace
rm(list = ls())

# load required libraries
library(bit64)
library(data.table)
library(noncensus)

#### LOAD DATA ####
# read data
mydata.part1 <-
  fread(input = "datasets/Hospital-Annual-Financial-Data-2015_part1.csv",
        skip = 3L)
mydata.part2 <-
  fread(input = "datasets/Hospital-Annual-Financial-Data-2015_part2.csv",
        skip = 3L)
mydata.util <- fread(input = "datasets/Hosp15_util_data_FINAL_part2.csv")

#### DATA PREP ####
# print list of duplicate column names
duplicate.part1 <-
  names(mydata.part1)[duplicated(names(mydata.part1)) |
                        duplicated(names(mydata.part1), fromLast = TRUE)]
duplicate.part2 <-
  names(mydata.part2)[duplicated(names(mydata.part2)) |
                        duplicated(names(mydata.part2), fromLast = TRUE)]
cat("The following duplicate column names were found.\n")
print(duplicate.part1)
print(duplicate.part2)

# fix duplicate names
cat("\nFixing duplicate column name issues...\n")
names(mydata.part1)[1] <- 'ID'
names(mydata.part1)[2] <- 'Date'
names(mydata.part2)[1] <- 'ID'
names(mydata.part2)[2] <- 'Date'

# remove duplicate column names
duplicate.part1 <-
  names(mydata.part1)[duplicated(names(mydata.part1))]
# list.delete <- c()  # take user input to update duplicate list
# for (i in duplicate.part1) {
#   print(mydata.part1[, colnames(mydata.part1) == i, with = FALSE])
#   delete.column <-
#     as.numeric(readline(prompt = "Which column to delete (1,2,...). Press Enter for none: "))
#   list.delete <-
#     append(list.delete, which(names(mydata.part1) %in% i)[delete.column])
# }
# list.delete <- list.delete[!is.na(list.delete)]
list.delete <- c(14, 15, 436, 437, 438, 1474, 1784, 2379, 2396)
cat("\nRemoving column names: ")
cat(list.delete)
mydata.part1[, c(list.delete) := NULL]
# rename duplicate columns
cat("\nRenaming remaining duplicate column names with 'x'...\n")
while (length(names(mydata.part1)[duplicated(names(mydata.part1))]) > 0) {
  names(mydata.part1)[duplicated(names(mydata.part1))] <-
    paste0(names(mydata.part1)[duplicated(names(mydata.part1))], "x")
}
while (length(names(mydata.part2)[duplicated(names(mydata.part2))]) > 0) {
  names(mydata.part2)[duplicated(names(mydata.part2))] <-
    paste0(names(mydata.part2)[duplicated(names(mydata.part2))], "x")
}

# merge data
cat("\nMerging main dataset with cost allocation dataset...\n")
mydata <-
  merge(mydata.part1, mydata.part2, by=c("ID","Date"))

#### SURGERY RELEVANT DATASET ####
# extract surgery relevant data
cat("\nExtract surgery relevant fields...\n")
mydata.surgery <-
  mydata[, c(
    seq(1:38),  # hospital characteristics fields
    grep(
      "surger|surgical|emergency|revenue|profit|income|cost|expenses",
      names(mydata),
      ignore.case = T
    )
  ), with = FALSE]
# removed duplicate columns and rows
mydata.surgery[, "Cost_Allocation_Statistics_Gross_Patient_Revenue_(2)_Total_Statistical_Units" := NULL]
cat("\nRemoving duplicate columns and rows by values: \n")
cat("Removing duplicate rows of hospitals...", "\n")  # keep the last of duplicates as they are arranged by date
mydata.surgery <- mydata.surgery[which(!duplicated(mydata.surgery, by = "ID", fromLast = TRUE))]
cat("Removing duplicate columns...\n")
mydata.surgery <-
  mydata.surgery[, which(!duplicated(t(mydata.surgery))), with = FALSE]

# append county information for each row
cat("\nDetermining County Names...\n")
mydata.surgery[, Zip_Code := sub("-[0-9]+", "", Zip_Code, perl = TRUE)]
data(zip_codes)
data(counties)
zip_codes.dt <- data.table(zip_codes)
counties.dt <- data.table(counties)
counties.dt[, fips := as.numeric(paste0(state_fips, county_fips))]
zip_codes.dt[counties.dt, on = "fips", county_name := county_name]
mydata.surgery[zip_codes.dt, on = c(Zip_Code = "zip"), County_Name := county_name]

# alternative way to extract county information from zip code
# library(ggmap)
# geocode('94120', source = "google", output="more")

# sort by County Names
cat("\nSorting by County Names...\n")
setorder(mydata.surgery, County_Name)

#### SURGERY RELEVANT SUMMARY DATASET ####
# get surgery summary dataset
cat("\nCreating summary dataset...\n")
# replace NA's with 0
cat("\nReplacing missing values with 0...\n")
for (i in seq_along(mydata.surgery))
  suppressWarnings(set(mydata.surgery,
      i = which(is.na(mydata.surgery[[i]])),
      j = i,
      value = 0))  # idiomatic, does not copy object, unlike x[is.na(x)] = 0

# get relevant columns
# # ORIGINAL CODE # #
# rooms.cols <- grep("operating_rooms", names(mydata.surgery), ignore.case = T, value = T)
# names(rooms.cols) <- grep("operating_rooms", names(mydata.surgery), ignore.case = T)
# surgeries.cols <- grep("(?=.*surgeries)(?!.*medi-cal)(?!.*medicare)(?!.*other)(?!.*programs)", names(mydata.surgery), ignore.case = TRUE, perl = TRUE, value = TRUE)
# names(surgeries.cols) <- grep("(?=.*surgeries)(?!.*medi-cal)(?!.*medicare)(?!.*other)(?!.*programs)", names(mydata.surgery), ignore.case = TRUE, perl = TRUE, value = FALSE)
# # surgery.units.cols <- grep("(?=.*units_of_service)(?=.*(surgery))",names(mydata.surgery), ignore.case = TRUE, perl = TRUE, value = TRUE) # same as minutes for surgery
# # names(surgery.units.cols) <- grep("(?=.*units_of_service)(?=.*(surgery))",names(mydata.surgery), ignore.case = TRUE, perl = TRUE, value = FALSE)
# surgery.mins.cols <- grep("minutes", names(mydata.surgery), ignore.case = TRUE, perl  = TRUE, value = TRUE)
# names(surgery.mins.cols) <- grep("minutes", names(mydata.surgery), ignore.case = TRUE, perl  = TRUE, value = F)
# revenue.cols <- grep("(?=.*revenue)(?=.*(surger|gross_revenue_total_operating|revenue_totals|revenue_total_patient))((?=.*gross)|(?=.*net))(?!.*unit)(?!.*alloc)(?!.*minus)", names(mydata.surgery), ignore.case = T, perl = TRUE, value = T)
# names(revenue.cols) <- grep("(?=.*revenue)(?=.*(surger|gross_revenue_total_operating|revenue_totals|revenue_total_patient))((?=.*gross)|(?=.*net))(?!.*unit)(?!.*alloc)(?!.*minus)", names(mydata.surgery), ignore.case = T, perl = TRUE, value = F)
# profit.cols <- grep("(?=.*minus)(?=.*(surgery|totals))", names(mydata.surgery), ignore.case = TRUE, perl  = TRUE, value = TRUE)
# names(profit.cols) <- grep("(?=.*minus)(?=.*(surgery|totals))", names(mydata.surgery), ignore.case = TRUE, perl  = TRUE, value = F)
# # END ORIGINAL CODE # #

facility.cols <- list(
  "facility.oshpd.id" = "ID",
  "facility.hospital.name" = "Facility_(Doing_Business_As)_Name",
  "facility.street.address" = "Street_Address",
  "facility.city.name" = "City",
  "facility.zip.code" = "Zip_Code",
  "facility.county.name" = "County_Name",
  "facility.owner" = "Name_of_Owner",
  "facility.begin.date" = "Report_Period_From",
  "facility.end.date" = "Date"
)
rooms.cols <- list(
  "operating.rooms.surgery.recovery.services" = "Inpatient_Operating_RoomsTotal_Surgery_and_Recovery_Services",
  "operating.rooms.ambulatory.surgery.services" = "Outpatient_Operating_RoomsTotal_Ambulatory_Surgery_Services",
  "operating.rooms.satellite.ambulatory.surgery.center" = "Satellite_Operating_RoomsTotal_Satellite_Ambulatory_Surgery_Center"
)
surgeries.cols <- list(
  "surgeries.surgery.recovery.services" = "SurgeriesTotal_Surgery_and_Recovery_Services",
  "surgeries.ambulatory.surgery.services" = "SurgeriesTotal_Ambulatory_Surgery_Services",
  "surgeries.satellite.ambulatory.surgery.center" = "SurgeriesTotal_Satellite_Ambulatory_Surgery_Center",
  "open.heart.surgeries.surgery.recovery.services" = "Open_Heart_SurgeriesTotal_Surgery_and_Recovery_Services",
  "inpatient.surgeries.surgery.recovery.services" = "Surgeries_Inpatient_Surgery_and_Recovery_Services",
  "inpatient.surgeries.ambulatory.surgery.services" = "Surgeries_Inpatient_Ambulatory_Surgery_Services",
  "inpatient.surgeries.satellite.ambulatory.surgery.center" = "Surgeries_Inpatient_Satellite_Ambulatory_Surgery_Center",
  "outpatient.surgeries.surgery.recovery.services" = "Surgeries_Outpatient_Surgery_and_Recovery_Services",
  "outpatient.surgeries.ambulatory.surgery.services" = "Surgeries_Outpatient_Ambulatory_Surgery_Services",
  "outpatient.surgeries.satellite.ambulatory.surgery.center" = "Surgeries_Outpatient_Satellite_Ambulatory_Surgery_Center",
  "outpatient.surgeries" = "Outpatient_Visits_Total_Outpatient_Surgeries"
)
surgery.mins.cols <- list(
  "surgery.mins.surgery.recovery.services" = "Operating_Minutes_Total_Surgery_and_Recovery_Services",
  "surgery.mins.ambulatory.surgery.services" = "Operating_Minutes_Total_Ambulatory_Surgery_Services",
  "surgery.mins.satellite.ambulatory.surgery.center" = "Operating_Minutes_Total_Satellite_Ambulatory_Surgery_Center",
  "open.heart.surgery.mins.surgery.recovery.services" = "Open_Heart_Surgery_MinutesTotal_Surgery_and_Recovery_Services",
  "inpatient.surgery.mins.surgery.recovery.services" = "Operating_Minutes_Inpatient_Surgery_and_Recovery_Services",
  "inpatient.surgery.mins.ambulatory.surgery.services" = "Operating_Minutes_Inpatient_Ambulatory_Surgery_Services",
  "inpatient.surgery.mins.satellite.ambulatory.surgery.center" = "Operating_Minutes_Inpatient_Satellite_Ambulatory_Surgery_Center",
  "outpatient.surgery.mins.surgery.recovery.services" = "Operating_Minutes_Outpatient_Surgery_and_Recovery_Services",
  "outpatient.surgery.mins.ambulatory.surgery.services" = "Operating_Minutes_Outpatient_Ambulatory_Surgery_Services",
  "outpatient.surgery.mins.satellite.ambulatory.surgery.center" = "Operating_Minutes_Outpatient_Satellite_Ambulatory_Surgery_Center"
)
revenue.cols <- grep("(?=.*revenue)(?=.*(surger|gross_revenue_total_operating|revenue_totals|revenue_total_patient))((?=.*gross)|(?=.*net))(?!.*unit)(?!.*alloc)(?!.*minus)", names(mydata.surgery), ignore.case = T, perl = TRUE, value = T)
for (i in seq_along(revenue.cols)) {
  if (length(grep("outpatient", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- "outpatient."
  } else if (length(grep("inpatient", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- "inpatient."
  } else {
    temp.var <- ""
  }
  if (length(grep("gross", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- paste0(temp.var, "gross.revenue")
  } else if (length(grep("net", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- "net.revenue"
  }
  if (length(grep("satellite", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- paste0(temp.var, ".satellite.ambulatory.surgery.center")
  } else if (length(grep("recovery", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- paste0(temp.var, ".surgery.recovery.services")
  } else if (length(grep("(?=.*(ambulatory))(?=.*(services))", revenue.cols[i], ignore.case = TRUE, perl = TRUE))) {
    temp.var <- paste0(temp.var, ".ambulatory.surgery.services")
  } else {
    temp.var <- paste0(temp.var, ".total")
  }
  if (length(grep("medicare", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- paste0(temp.var, ".medicare")
  } else if (length(grep("medi-cal", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- paste0(temp.var, ".medical")
  } else if (length(grep("county", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- paste0(temp.var, ".county.indigent")
  } else if (length(grep("third", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- paste0(temp.var, ".other.third.parties")
  } else if (length(grep("(?=.*(other))(?=.*(indigent))", revenue.cols[i], ignore.case = TRUE, perl = TRUE))) {
    temp.var <- paste0(temp.var, ".other.indigent")
  } else if (length(grep("payors", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- paste0(temp.var, ".other.payors")
  }
  if (length(grep("traditional", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- paste0(temp.var, ".traditional")
  } else if (length(grep("managed", revenue.cols[i], ignore.case = TRUE))) {
    temp.var <- paste0(temp.var, ".managed")
  }
  names(revenue.cols)[i] <- temp.var
}
length(revenue.cols) <- length(revenue.cols) - 1
profit.cols <- list(
  "net.profit.surgery.recovery.services" = "Summary_of_Revenue_and_Costs_Net_Revenue_Minus_Net_Costs_Surgery_and_Recovery_Services",
  "net.profit.ambulatory.surgery.services" = "Summary_of_Revenue_and_Costs_Net_Revenue_Minus_Net_Costs_Ambulatory_Surgery_Services" ,
  "net.profit.satellite.ambulatory.surgery.center" = "Summary_of_Revenue_and_Costs_Net_Revenue_Minus_Net_Costs_Satellite_Ambulatory_Surgery_Center",
  "net.profit.total" = "Summary_of_Revenue_and_Costs_Net_Revenue_Minus_Net_Costs_Totals_/_Net_Profit_(Loss)"
)
map.cols <- c(facility.cols, rooms.cols, surgeries.cols, surgery.mins.cols, revenue.cols, profit.cols)

# rename fields in summary dataset
mydata.surgery.summary <- mydata.surgery[, unlist(map.cols), with=FALSE]
names(mydata.surgery.summary) <- names(map.cols)

# update operating room counts wih correct totals if present in UTIL dataset
mydata.surgery.summary[mydata.util, on = c(facility.oshpd.id = "OSHPD_ID"),
               operating.rooms.surgery.recovery.services := OP_RM_TOTL]

#### WRITE DATA TO FILE ####
# write surgery dataset to csv file
cat("\nDataset size: ", dim(mydata.surgery), "\n")
fwrite(mydata.surgery,
       "datasets/ohspd_surgery_2015.csv",
       row.names = FALSE)

# write surgery summary dataset to csv file
cat("\nStoring summary dataset...\n")
cat("\nFinal dataset size: ", dim(mydata.surgery.summary))
fwrite(mydata.surgery.summary,
          "datasets/ohspd_surgery_2015_summary.csv",
          row.names = FALSE)
