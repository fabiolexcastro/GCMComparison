
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools, foreach, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
source('01_functions.R')
vrs <- paste0('bio_', 1:33, '$')

# Load data ---------------------------------------------------------------
pth <- 'Z:/_cocoa_cam/_data/_tif/_climate/_current/_cut_smp/'
fls <- list.files(pth, full.names = TRUE) %>% 
  grep(paste0(vrs, collapse = '|'), ., value = TRUE) %>% 
  mixedsort()
stk <- stack(fls)

# Mask --------------------------------------------------------------------
msk <- raster('../raster/bse/msk_smp.asc')
pnt <- rasterToPoints(msk) %>% as_tibble()
dir.create('../tbl/pnt', recursive = TRUE)
write.csv(pnt, '../tbl/pnt/points_sample.csv', row.names = FALSE)

# Extract values to points ------------------------------------------------
cl <- makeCluster(17)
registerDoSNOW(cl)
vls <- foreach(i = 1:nlayers(stk), .packages = c('raster', 'rgdal', 'dplyr', 'tibble'), .verbose = TRUE) %dopar% {
  myExtract(pos = i)
} 
stopCluster(cl)
vl2 <- do.call(cbind, vls) %>% 
  as_tibble() %>% 
  setNames(paste0('bio_', 1:33)) %>% 
  mutate(gcm = 'current',
         year = 'current', 
         id = 1:nrow(.),
         x = pull(pnt, 1),
         y = pull(pnt, 2)) %>% 
  dplyr::select(id, x, y, year, gcm, bio_1:bio_33)
write.csv(vl2, '../tbl/clm/sample_current.csv', row.names = FALSE)

# Extraction values from presences ----------------------------------------
load('Z:/_cocoa_cam/_rds/_run12/clustereddata.rData')
write.csv(occ, '../tbl/clm/occ_current.csv')









