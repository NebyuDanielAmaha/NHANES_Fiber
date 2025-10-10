# NHANES_Fiber

## Overview
This repository contains analysis of the National Health and Nutrition Examination Survey (NHANES) exploring the association between **dietary fiber intake** and **depressive symptoms** among adults aged 20 years and older.

## Data
- NHANES 24-hour dietary recall data (`DR1TOT`, `DR2TOT`)
- Demographics (`DEMO_L`)
- Depression (PHQ-9, `DPQ_L`)
- Physical activity (`PAQ_L`)
- Body measures (`BMX_L`)
- Prescribed medications (`RXQ_RX_L`)

> **Note:** Raw NHANES data are not included in this repository due to data use agreements. Users can download from [NHANES website](https://www.cdc.gov/nchs/nhanes/index.htm).

## Analysis
- Data cleaning and variable recoding in R.
- Main exposure: **Average daily fiber intake** (grams/day).
- Outcome: **Depression**, measured with PHQ-9 (both binary and ordinal categories).
