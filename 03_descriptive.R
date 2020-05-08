

# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools, foreach, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
source('01_functions.R')

# Functions to use --------------------------------------------------------
mySummarise <- function(gc){
  # gc <- gcm[1]
  tb <- tbl %>% 
    filter(gcm == gc) %>% 
    gather(var, value, -x, -y, -gcm, -year) %>% 
    group_by(gcm, var) %>% 
    summarise(value = mean(value)) %>% 
    ungroup() %>% 
    mutate(var = factor(var, levels = paste0('bio_', 1:33))) %>% 
    arrange(var)
  print('Done!')
  return(tb)
}

# Load data ---------------------------------------------------------------
'../rData/tbl_gcms_bios.rds'
tbl <- readRDS('../rData/tbl_gcms_bios.rds')
tbl <- bind_rows(tbl)
gcm <- unique(tbl$gcm)

# Cluster analysis --------------------------------------------------------
tbls <- map(.x = gcm, .f = mySummarise)
tbls <- bind_rows(tbls)
tbls <- spread(tbls, gcm, value)



