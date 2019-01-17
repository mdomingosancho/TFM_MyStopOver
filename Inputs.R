library(TSP)
library(leaflet)
library(sp)
library(geosphere)
library(shiny)
library(dplyr)
library(shinyTime)

load("Data_MyStopOver.RData")
source("GeoCode.R")

key <- "AIzaSyDketUNiJG3LMyMygdlofsXmOsDrdR0_5k"  # Key de Google Cloud
