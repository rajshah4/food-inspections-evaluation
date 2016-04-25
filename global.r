library(shiny)
library(data.table)
library(glmnet)
library(ggplot2)
library(scales)
library(randomForest)
library(ROCR)
library(plotly)
library(caret)
source("CODE/functions/logLik.R")
source("CODE/functions/gini.R")
source("CODE/functions/calculate_confusion_values.R")

ggplot <- function(...) {
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                 "#0072B2", "#D55E00", "#CC79A7")
  ggplot2::ggplot(...) +
    theme_grey()+
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    theme(plot.title = element_text(size = 15))
  
}

dat <- readRDS("DATA/dat_model.Rds")

## Only keep "Retail Food Establishment"
dat <- dat[LICENSE_DESCRIPTION == "Retail Food Establishment"]
## Remove License Description
dat[ , LICENSE_DESCRIPTION := NULL]
dat <- na.omit(dat)

## Add criticalFound variable to dat:
dat[ , criticalFound := pmin(1, criticalCount)]

## Set the key for dat
setkey(dat, Inspection_ID)

dat[Inspection_Date < "2014-07-01", range(Inspection_Date)]
dat[Inspection_Date > "2014-07-01", range(Inspection_Date)]

iiTrain <- dat[ , which(Inspection_Date < "2014-07-01")]
iiTest <- dat[ , which(Inspection_Date > "2014-07-01")]
