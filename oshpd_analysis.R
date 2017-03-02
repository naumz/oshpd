# Analyze OSHPD surgery financial/utilization summary dataset
#
# Date: February 4, 2017
#
# Author: Naumaan Nayyar

#### ENVIRONMENT ####
# clear workspace
rm(list = ls())
cat("\014")
graphics.off()

to.plot = FALSE  # plot various cluster and regression plots

# load required libraries
library(bit64)
library(data.table)
library(psych)
library(stringr)
library(MASS)
library(ggplot2)
library(cluster)

# This function returns TRUE wherever elements are the same, including NA's,
# and FALSE everywhere else.
compareNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

#### LOAD DATA ####
# load dataset
mydata <- fread('datasets/ohspd_surgery_2015_summary.csv')

#### FILTER DATA ####
# choose hospitals in LA County and Orange County with >= 1000 surgeries
mydata <-
  mydata[(facility.county.name == "Los Angeles County" | facility.county.name == "Orange County") &
           surgeries.surgery.recovery.services >= 1000 &
           gross.revenue.surgery.recovery.services > 0]

#### DATA PREP ####
facility.cols <-
  grep("facility",
       names(mydata),
       ignore.case = T,
       value = T)
rooms.cols <-
  grep("rooms",
       names(mydata),
       ignore.case = T,
       value = T)
surgeries.cols <-
  grep("surgeries",
       names(mydata),
       ignore.case = T,
       value = T)
surgery.mins.cols <-
  grep("mins",
       names(mydata),
       ignore.case = T,
       value = T)
revenue.cols <-
  grep("revenue",
       names(mydata),
       ignore.case = T,
       value = T)
profit.cols <-
  grep("profit",
       names(mydata),
       ignore.case = T,
       value = T)

# sum inpatient and outpatient statistics for individual payors, keep only sum
# values
for (elem in str_match(revenue.cols, "^(outpatient.|inpatient.)(.*)$")[, 3]) {
  if (is.na(elem))
    next
  mydata[, (elem) := Reduce(`+`, .SD),
         .SDcols = revenue.cols[which(!is.na(str_match(revenue.cols,
                                                       paste0('^(inpatient.|outpatient.)(', elem, ')$'))[, 3]))]]
}
mydata[, revenue.cols[grep('^(inpatient.|outpatient.)', revenue.cols)] := NULL]
revenue.cols <-
  grep("revenue",
       names(mydata),
       ignore.case = T,
       value = T)

# get monthly columns
mydata[, reporting.period.months := round(as.numeric(
  as.Date(facility.end.date, "%m/%d/%y") - as.Date(facility.begin.date, "%m/%d/%y")
) / 30.4)]
mydata[, paste0(c(surgeries.cols, surgery.mins.cols, revenue.cols, profit.cols),
                ".month") := lapply(.SD, `/`, reporting.period.months),
       .SDcols = c(surgeries.cols, surgery.mins.cols, revenue.cols, profit.cols)]

# remove ambulatory and satellite ambulatory services
mydata <- mydata[, grep('ambulatory', names(mydata)) := NULL]

#### ADD RELEVANT COLUMNS ####
# financial ratios
mydata[, `:=`(
  gross.revenue.surgery.recovery.services.month.per.or = gross.revenue.surgery.recovery.services.month / operating.rooms.surgery.recovery.services,
  gross.revenue.surgery.recovery.services.month.per.surgery = gross.revenue.surgery.recovery.services.month / surgeries.surgery.recovery.services.month,
  gross.revenue.surgery.recovery.services.month.per.hour.of.surgery = 60 * gross.revenue.surgery.recovery.services.month / surgery.mins.surgery.recovery.services.month,
  net.revenue.surgery.recovery.services.month.per.or = net.revenue.surgery.recovery.services.month / operating.rooms.surgery.recovery.services,
  net.revenue.surgery.recovery.services.month.per.surgery = net.revenue.surgery.recovery.services.month / surgeries.surgery.recovery.services.month,
  net.revenue.surgery.recovery.services.month.per.hour.of.surgery = 60 * net.revenue.surgery.recovery.services.month / surgery.mins.surgery.recovery.services.month,
  net.profit.surgery.recovery.services.month.per.or = net.profit.surgery.recovery.services.month / operating.rooms.surgery.recovery.services,
  net.profit.surgery.recovery.services.month.per.surgery = net.profit.surgery.recovery.services.month / surgeries.surgery.recovery.services.month,
  net.profit.surgery.recovery.services.month.per.hour.of.surgery = 60 * net.profit.surgery.recovery.services.month / surgery.mins.surgery.recovery.services.month
)]

#monthly measures
mydata[, `:=`(
  gross.revenue.surgery.recovery.services.indigent.month = gross.revenue.surgery.recovery.services.county.indigent.managed.month + gross.revenue.surgery.recovery.services.county.indigent.traditional.month + gross.revenue.surgery.recovery.services.other.indigent.month,
  gross.revenue.surgery.recovery.services.medical.month = gross.revenue.surgery.recovery.services.medical.managed.month + gross.revenue.surgery.recovery.services.medical.traditional.month,
  gross.revenue.surgery.recovery.services.medicare.month = gross.revenue.surgery.recovery.services.medicare.managed.month + gross.revenue.surgery.recovery.services.medicare.traditional.month,
  gross.revenue.surgery.recovery.services.other.month = gross.revenue.surgery.recovery.services.other.payors.month + gross.revenue.surgery.recovery.services.other.third.parties.managed.month + gross.revenue.surgery.recovery.services.other.third.parties.traditional.month,
  gross.revenue.managed.month = gross.revenue.surgery.recovery.services.county.indigent.managed.month + gross.revenue.surgery.recovery.services.medical.managed.month + gross.revenue.surgery.recovery.services.medicare.managed.month + gross.revenue.surgery.recovery.services.other.third.parties.managed.month,
  gross.revenue.traditional.month = gross.revenue.surgery.recovery.services.county.indigent.traditional.month + gross.revenue.surgery.recovery.services.medical.traditional.month + gross.revenue.surgery.recovery.services.medicare.traditional.month + gross.revenue.surgery.recovery.services.other.third.parties.traditional.month + gross.revenue.surgery.recovery.services.other.payors.month + gross.revenue.surgery.recovery.services.other.indigent.month
)]

# proportions
mydata[, `:=`(managed.traditional.ratio = gross.revenue.managed.month / gross.revenue.traditional.month,
              proportion.revenue.indigent = gross.revenue.surgery.recovery.services.indigent.month / gross.revenue.surgery.recovery.services.month,
              proportion.revenue.other = gross.revenue.surgery.recovery.services.other.month / gross.revenue.surgery.recovery.services.month,
              proportion.revenue.medical = gross.revenue.surgery.recovery.services.medical.month / gross.revenue.surgery.recovery.services.month,
              proportion.revenue.medicare = gross.revenue.surgery.recovery.services.medicare.month / gross.revenue.surgery.recovery.services.month
)]

#### CLUSTERING ANALYSIS ####
# find correlations between columns
# pairs.panels(mydata.surgery[, unname(surgeries.cols)])

# clustering model - cluster hospitals by payor mix
mydata.cluster.full <-
  mydata[, .(
    facility.hospital.name,
    facility.county.name,
    proportion.revenue.medicare,
    proportion.revenue.medical,
    proportion.revenue.other,
    proportion.revenue.indigent,
    surgeries.surgery.recovery.services.month
  )]
mydata.cluster.full[, `:=`(
  proportion.revenue.medicare = scale(proportion.revenue.medicare),
  proportion.revenue.medical = scale(proportion.revenue.medical),
  proportion.revenue.other = scale(proportion.revenue.other),
  proportion.revenue.indigent = scale(proportion.revenue.indigent),
  surgeries.surgery.recovery.services.month = scale(surgeries.surgery.recovery.services.month)
)]  # standardization
mydata.cluster <-
  mydata.cluster.full[, -c("facility.hospital.name", "facility.county.name"),
                      with = FALSE]

# Cluster method 1 - k-means
wss <- (nrow(mydata.cluster) - 1) * sum(apply(mydata.cluster, 2, var))
for (i in 2:30)
  wss[i] <- sum(kmeans(mydata.cluster, centers = i)$withinss)
plot(1:30,
     wss,
     type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")  # determine clusters
n <-
  as.numeric(readline(prompt = "Number of clusters for k-means: "))  # clusters
fit.mix <- kmeans(mydata.cluster, n)
clusters <- fit.mix$cluster
# # Cluster method 2 - hierarchical
# n <- 15
# d <- dist(mydata.cluster, method = "euclidean")
# fit <- hclust(d, method="ward.D")
# plot(fit) # display dendogram
# rect.hclust(fit, k=n, border="red")
# clusters <- cutree(fit, k=n) # cut tree into n clusters

mydata.cluster.full[, cluster := clusters]

if(to.plot) {
  # cluster plots
  clusplot(mydata.cluster, fit.mix$cluster, col.p = fit.mix$cluster)
  mydata.cluster.melt <-
    melt(
      mydata.cluster.full,
      id.vars = c("facility.hospital.name", "facility.county.name", "cluster"),
      variable.name = "Category",
      value.name = "Proportions"
    )
  q <-
    ggplot(mydata.cluster.melt,
           aes(x = facility.hospital.name, y = Proportions, fill = Category)) +
    geom_bar(stat = 'identity') + ggtitle("Facilities ordered by cluster")
  print(q)
  # payor mix plot
  mydata.cluster.summary <-
    mydata.cluster.full[, lapply(.SD, mean), .SDcols = 3:6, keyby = "cluster"]
  mydata.cluster.summary.melt <-
    melt(
      mydata.cluster.summary,
      id.vars = c("cluster"),
      variable.name = "Payors",
      value.name = "Proportions"
    )
  r <-
    ggplot(mydata.cluster.summary.melt,
           aes(x = cluster, y = Proportions, fill = Payors)) +
    geom_bar(stat = 'identity') +
    ggtitle("Mean center of clusters (similar to median)")
  print(r)
}

# determine closest hospital in terms of payor mix
d <- dist(mydata.cluster, method = "euclidean")
m <- as.matrix(d)
diag(m) <- NA
Most.like.hospital <- apply(m, 1, which.min)
mydata.cluster.full[,
                    c("most.like.hospital", "most.like.hospital.net.profit.surgery.per.surgery") :=
                      mydata[Most.like.hospital, .(facility.hospital.name, net.profit.surgery.recovery.services.month.per.surgery)]]

# add relevant information from full dataset
mydata.cluster.full[mydata, on = "facility.hospital.name", `:=`(
  net.revenue.surgery = i.net.revenue.surgery.recovery.services,
  net.revenue.surgery.per.surgery = i.net.revenue.surgery.recovery.services.month.per.surgery,
  gross.realization.ratio = i.net.revenue.surgery.recovery.services / i.gross.revenue.surgery.recovery.services,
  operating.rooms = i.operating.rooms.surgery.recovery.services,
  num.surgeries = i.surgeries.surgery.recovery.services,
  surgery.rev.share = i.net.revenue.surgery.recovery.services / i.net.revenue.total,
  managed.traditional = i.managed.traditional.ratio,
  proportion.medicare = i.proportion.revenue.medicare,
  proportion.other = i.proportion.revenue.other,
  net.profit.surgery = i.net.profit.surgery.recovery.services,
  net.profit.surgery.per.surgery = i.net.profit.surgery.recovery.services.month.per.surgery,
  net.profit.total = i.net.profit.total,
  facility.owner = i.facility.owner,
  facility.address = paste0(i.facility.street.address, ", ",
                            i.facility.city.name, ", CA ",
                            i.facility.zip.code),
  facility.zip.code = i.facility.zip.code,
  facility.state.name = "CA",
  or.utilization = i.surgery.mins.surgery.recovery.services.month / (i.operating.rooms.surgery.recovery.services * 21.7 * 60)
)]
setkey(mydata.cluster.full,
       net.profit.surgery.per.surgery)
mydata.cluster.full[,
                    like.hospitals := paste(facility.hospital.name, collapse=", "), by="cluster"]
mydata.cluster.full[,
                    like.hospitals := mapply(sub, paste0(facility.hospital.name, "(, )?"), '', like.hospitals)]

if(to.plot) {
  #profitability plot
  hist(
    mydata.cluster.full[sort(net.profit.surgery.per.surgery,
                             index.return = TRUE)$ix, net.profit.surgery.per.surgery],
    breaks = 50,
    main = "Net Profit / Surgery compared",
    xlab = "Net Profit/Surgery"
  )
}

#### PROFITABILITY GRADING ####
#get profitability of different hospitals
grading <- list(
  "D" = 25,
  "C-" = 35,
  "C" = 50,
  "C+" = 62,
  "B-" = 75,
  "B" = 87,
  "B+" = 95,
  "A-" = 98,
  "A" = 100
)
mydata.cluster.full[, `:=`(
  median.hospitals = median(net.profit.surgery.per.surgery),
  max.hospitals = max(net.profit.surgery.per.surgery),
  hospital.percentile = seq_len(.N) / .N,
  num.county.hospitals = .N
), by = "facility.county.name"]
mydata.cluster.full[order(-net.revenue.surgery.per.surgery),
                    revenue.rank := seq(1:.N), by=facility.county.name]
mydata.cluster.full[, hospital.grade :=
                      names(grading)[findInterval(hospital.percentile * 100,
                                                  unlist(grading),
                                                  rightmost.closed = TRUE) + 1]]

#### NET SURGERY PROFIT PREDICTION ####
# regression model - predict net profitability
mydata.regression <- mydata[,
                            .(scale(net.profit.surgery.recovery.services.month),
                              scale(net.revenue.surgery.recovery.services.month),
                              facility.county.name,
                              scale(proportion.revenue.medicare),
                              scale(surgery.mins.surgery.recovery.services))
                            ]
net.profit.model1 <-
  lm(
    V1 ~ V2 + facility.county.name + V4 + V5,
    data = mydata.regression,
    subset = -c(33, 54, 59, 9)
  )
net.profit.model.full <-
  lm(
    net.profit.surgery.recovery.services.month ~ net.revenue.surgery.recovery.services.month + gross.revenue.surgery.recovery.services.month + proportion.revenue.medicare + proportion.revenue.medical + proportion.revenue.other + proportion.revenue.indigent + surgeries.surgery.recovery.services.month + surgery.mins.surgery.recovery.services.month + operating.rooms.surgery.recovery.services + managed.traditional.ratio + facility.county.name,
    data = mydata,
    subset = -c(33, 54, 59, 9)
  )
net.profit.model2 <-
  stepAIC(net.profit.model.full, direction = "both", trace = 0)
y1 <- mydata.regression[, V1]
scales <- unlist(attributes(y1))
y1 <- y1 * scales["scaled:scale"] + scales["scaled:center"]  # adjust for scale
x1 <- predict(net.profit.model1, mydata.regression) * scales["scaled:scale"] + scales["scaled:center"]
mydata.predict <- data.table(mydata[, .(facility.hospital.name, reporting.period.months)], x1, y1)
mydata.cluster.full[mydata.predict, on="facility.hospital.name",
                    net.profit.surgery.predicted := x1 * reporting.period.months]
if(to.plot) {
  # prediction accuracy plot
  p <-
    qplot(
      x1,
      y1,
      colour = sign(x1 - y1),
      xlab = "Predicted Net Profit",
      ylab = "Actual Net Profit",
      main = "Net Profit Model"
    )
  print(p)
}

#### REPORT CARD ####
# generate report card
mydata.report.card <- mydata.cluster.full[, .(facility.hospital.name,
                                              facility.address,
                                              facility.state.name,
                                              facility.county.name,
                                              facility.owner,
                                              net.revenue.surgery,
                                              gross.realization.ratio,
                                              operating.rooms,
                                              num.surgeries,
                                              net.profit.surgery,
                                              net.profit.surgery.predicted,
                                              surgery.rev.share,
                                              proportion.medicare,
                                              proportion.other,
                                              managed.traditional,
                                              net.revenue.surgery.per.surgery,
                                              net.profit.surgery.per.surgery,
                                              net.profit.total,
                                              hospital.grade,
                                              revenue.rank,
                                              cluster,
                                              like.hospitals,
                                              most.like.hospital,
                                              most.like.hospital.net.profit.surgery.per.surgery,
                                              max.hospitals,
                                              median.hospitals,
                                              num.county.hospitals,
                                              or.utilization
)]

#### WRITE DATA TO FILE ####
# store datasets
fwrite(mydata,
       "datasets/ohspd_surgery_2015_summary_added.csv",
       row.names = FALSE)
fwrite(mydata.report.card,
       "datasets/ohspd_surgery_2015_report_card.csv",
       row.names = FALSE)
