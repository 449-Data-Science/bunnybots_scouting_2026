library(shiny)
library(shinycssloaders)
library(bslib)

library(tidyverse)
library(DT)
library(ggplot2)

source("api-interface.R")
source("helper-functions.R")

POLL_INTERVAL <- 5000
