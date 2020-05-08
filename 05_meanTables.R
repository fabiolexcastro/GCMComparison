
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools, foreach, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
occ_dff <- readRDS(file = '../rData/diff_ocurrences_gcms.rds')
occ_crn <- readRDS(file = '../rData/crn_ocurrences.rds')
occ_ftr <- readRDS(file = '../rData/ftr_ocurrences.rds')

smp_dff <- readRDS(file = '../rData/dff_sample_gcms.rds')
smp_crn <- readRDS(file = '../rData/crn_sample.rds')
smp_ftr <- readRDS(file = '../rData/ftr_sample.rds')

# Summarise ocurrences ------------------------------------------------------------
smm_occ_crn <- occ_crn %>% 
  group_by(variable) %>% 
  summarise(current = mean(value)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels = paste0('bio_', 1:33))) %>% 
  arrange(variable)
smm_occ_ftr <- occ_ftr %>% 
  group_by(variable, gcm) %>% 
  summarise(future = mean(value)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels = paste0('bio_', 1:33))) %>% 
  arrange(variable)
smm_occ_dff <- occ_dff %>% 
  group_by(gc, variable) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels = paste0('bio_', 1:33))) %>% 
  arrange(variable)

saveRDS(object = smm_occ_crn, file = '../rData/summary/smm_occ_crn.rds')
saveRDS(object = smm_occ_ftr, file = '../rData/summary/smm_occ_ftr.rds')
saveRDS(object = smm_occ_dff, file = '../rData/summary/smm_occ_dff.rds')

# Summarise sample --------------------------------------------------------
smm_smp_crn <- smp_crn %>% 
  group_by(variable) %>% 
  summarise(current = mean(value)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels = paste0('bio_', 1:33))) %>% 
  arrange(variable)
smm_smp_ftr <- smp_ftr %>% 
  group_by(variable, gcm) %>% 
  summarise(future = mean(value)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels = paste0('bio_', 1:33))) %>% 
  arrange(variable)
smm_smp_dff <- smp_dff %>% 
  group_by(gc, variable) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels = paste0('bio_', 1:33))) %>% 
  arrange(variable) %>% 
  mutate(gc = gsub('dff_', '', gc))

dir.create('../rData/summary')

saveRDS(object = smm_smp_crn, file = '../rData/summary/smm_smp_crn.rds')
saveRDS(object = smm_smp_ftr, file = '../rData/summary/smm_smp_ftr.rds')
saveRDS(object = smm_smp_dff, file = '../rData/summary/smm_smp_dff.rds')













