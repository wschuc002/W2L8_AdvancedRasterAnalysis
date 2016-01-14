# Student: William Schuch & Rik van Berkum
# Team: Geodetic Engineers of Utrecht
# Institute: Wageningen University and Research
# Course: Geo-scripting (GRS-33806)
# Date: 2016-01-12
# Week 2, Lesson 8: Advanced Raster Analysis


rm(list = ls())  # Clear the workspace!
ls() ## no objects left in the workspace

# Installing/updating packages (for the R Markdown)
#install.packages(c("knitr", "yaml", "htmltools", "caTools", "bitops", "rmarkdown"))

# Installing/updating packages (for the R Markdown publish)
#install.packages(c("RCurl", "PKI", "packrat", "rstudioapi"))

# The rsconnect package might be installed after being asked.
# It will create a new folder: rsconnect

# load librarys
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(knitr)
library(yaml)
library(htmltools)
library(caTools)
library(bitops)
library(rmarkdown)
library(RCurl)
library(PKI)
library(packrat)
library(rstudioapi)
#library(rsconnect)

# Open the web page of my created Document Report
shell.exec("http://rpubs.com/wschuc002/143014")

# referring to functions in R folder
#source("./R/Preprocessing.R")


## load data
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")

# check out the attributes
GewataB2
# some basic statistics using cellStats()
cellStats(GewataB2, stat=max)
cellStats(GewataB2, stat=mean)
# This is equivalent to:
maxValue(GewataB2)
# what is the maximum value of all three bands?
max(c(maxValue(GewataB2), maxValue(GewataB3), maxValue(GewataB4)))
# summary() is useful function for a quick overview
summary(GewataB2)

# put the 3 bands into a rasterBrick object to summarize together
gewata <- brick(GewataB2, GewataB3, GewataB4)
# 3 histograms in one window (automatic, if a rasterBrick is supplied)
hist(gewata)
?graphics::hist
par(mfrow = c(1, 1)) # reset plotting window
hist(gewata, xlim = c(0, 5000), ylim = c(0, 750000), breaks = seq(0, 5000, by = 100))

pairs(gewata)
