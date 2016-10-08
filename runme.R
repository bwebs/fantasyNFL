#install.packages(c('rvest','stringr','tidyr','RCurl','XML','jsonlite','httr'), requirements=TRUE)

library(shiny)
library(shinydashboard)
library(plyr)
library(RCurl)
library(XML)
library(jsonlite)
require(httr)
require(XML)

source('CreateBuckets_shiny.R')
source('madden_adjustments.R')
source('2015_FP.R')
source('injuryTest.R')
source('functions.R')
source('deepLearningBonus.R')
source('prompts.R')


prompts()


