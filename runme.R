#install.packages(c('rvest','stringr','tidyr','RCurl','XML','jsonlite','httr','h2o'), requirements=TRUE)

fanduelFile = 'FanDuel-NFL-2016-11-13-16864-lineup-upload-template.csv'

library(shiny)
library(shinydashboard)
library(plyr)
library(RCurl)
library(XML)
library(jsonlite)
require(httr)
require(XML)
library(h2o)

source('CreateBuckets_shiny.R')
source('madden_adjustments.R')
source('2015_FP.R')
source('injuryTest.R')
source('functions.R')
source('deepLearningBonus.R')
source('prompts.R')

prompts()




