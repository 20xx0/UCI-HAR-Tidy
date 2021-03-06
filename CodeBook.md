---
title: "CodeBook"
author: "Silver"
date: "7/26/2020"
output: html_document
---

## The script

The script called "run_analysis.R" is written with the objective of downloading the **Human Activity Recognition Using Smartphones Data Set** and tidying it up for further analysis

## Raw data

The data can be downloaded from the UCI website [here](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

## Tidy data explained

The tidy data set contains 180x66=11880 observations in total. It has been formatted to show the average of each variable for each activity and each subject.

Each row represents a combination of a certain activity (6 in total) of a certain subject (30 in total), creating 6*30=180 factor levels.
Each column represents the mean value for each observation (66 in total) of the corresponding factor.
