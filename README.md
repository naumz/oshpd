# oshpd
Analysis of financial and utilization performance of surgical departments of hospitals in CA

This repository contains code to compare hospitals across CA in terms of relative financial performances of their surgical departments.

**Features**

1. Computes several useful metrics related to financial performance from raw data.
2. Clusters similar hospitals based on payor mix of patients seen in surgical departments.
3. Determines most similar hospital based on payor mix for each hospital.
4. Develops a model for predicting net profit of the surgery department at each hospital (model accuracy very good).
5. Creates an easy-to-read dataset with relevant hospital metrics (report card)

**Miscellaneous**

Data preparation and handling is done with R's data.table package to ensure speed and readability.

**Files**

1. oshpd_annual_data_surgery.R - prepares raw data for analysis: removes duplicate rows, columns; handles missing data; extracts surgery relevant fields into surgery dataset and surgery summary dataset.
2. oshpd_analysis.R - performs analysis: computes several useful metrics from prepared data, performs clustering analysis to identify similar hospitals, predicts net profit of surgery department, creates easy-to-read repot card dataset

**Input**

1. CA OSHPD Annual Financial complete dataset - http://oshpd.ca.gov/HID/Hospital-Financial.asp#Complete
2. CA OSHPD Annual Utilization complete dataset - http://oshpd.ca.gov/HID/Hospital-Utilization.html#Complete (only needed for getting accurate count of operating rooms as the information in the financial dataset is inaccurate)

Each of these Excel files are converted into .csv files. Individual sheets are renamed as '\_part1.csv' and '\_part2.csv'.

**Output**

_Final_

ohspd_surgery_2015_report_card.csv - contains easy-to-read report card of performances of surgical departments for hospitals in CA

_Intermediate_

1. ohspd_surgery_2015.csv - surgery dataset with all surgery relevant fields
2. ohspd_surgery_2015_summary.csv - summary surgery dataset with important financial and utilization metrics
3. ohspd_surgery_2015_summary_added.csv - adds several useful metrics to summary surgery dataset
