library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)

library("readxl")   # funkcije za branje Excelovih datotek 
library("openxlsx")   # funkcije za pisanje Excelovih datotek
library("dplyr")    # funkcije za lažjo manipulacijo operacij na razpredelnicah
require(dplyr)
require(tidyr)
require(readr)
library(knitr)
library(rvest)
library(gsubfn)
library(reshape2)
library(shiny)
library(readr)
library(dplyr)
library(rmarkdown)
library(tidyr)
library(ggplot2)
library(ggvis)
library(dplyr)
library(rgdal)
library(mosaic)
library(maptools)
library(sp)
library(httpuv)
library(munsell)
library(plotly)


options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
