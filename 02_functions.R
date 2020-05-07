
myExtract <- function(gc){
  
  process <- function(pos){
    r <- st[[pos]]
    v <- raster::extract(r, pnt[,1:2])
    print(length(v) == nrow(pnt))
    d <- data.frame(variable = names(r), values = v) %>% 
      as_tibble() %>% 
      dplyr::select(values) %>% 
      setNames(names(r))
    return(d)
  }
  
  # gc <- gcm[1]
  pnt <- read.csv('../tbl/pnt/points_sample.csv') %>% as_tibble()
  gc <- paste0(gc, '/')
  st <- grep(gc, fls, value = TRUE) %>% 
    stack()
  print('To make the process')
  df <- map(.x = 1:33, .f = process)
  df <- do.call(cbind, df) %>% 
    as_tibble() %>% 
    mutate(gcm = gc, 
           year = '2050',
           id = 1:nrow(.),
           x = pull(pnt, 1),
           y = pull(pnt, 2)) %>% 
    dplyr::select(id, x, y, year, gcm, bio_1:bio_33)
  print('Done!')
  write.csv(df, paste0('../tbl/clm/sample_2050_', gsub('/', '', gc), '.csv'), row.names = FALSE)
  return(df)
}

myCrop <- function(pos){
  x <- fls[pos]
  print(basename(x))
  x <- raster(x)
  x <- raster::crop(x, rst.smp) %>% raster::mask(., rst.smp)
  y <- rasterToPoints(x)
  y <- as_tibble(y)
  ids <- 1:nrow(y)
  z <- y %>% mutate(id = ids)
  print('Done!')
  return(z)
}
viewData <- function(gc){
  # gc <- gcm[1]
  yr <- '2050'
  rst2tbl <- function(pos){
    x <- stk[[pos]]
    x <- rasterToPoints(x)
    x <- as_tibble(x)
    return(x)
  }
  fls <- list.files(paste0(pth, '/', gc), full.names = TRUE) %>% 
    mixedsort() %>% 
    grep(paste0(vrs, collapse = '|'), ., value = TRUE)
  stk <- raster::stack(fls)
  cl <- makeCluster(17)
  registerDoSNOW(cl)
  x <- foreach(i = 1:length(fls), .packages = c('raster', 'rgdal', 'dplyr', 'tibble'), .verbose = TRUE) %dopar% {
    rst2tbl(pos = i)
  } 
  stopCluster(cl)
  y <- Reduce(f = inner_join, x = x)
  y <- y %>% mutate(gcm = gc, year = yr)
  print('Done!')
  return(y)
}

addID <- function(pos){
  x <- dfm[[pos]]
  x <- x %>% mutate(id = 1:nrow(.))
  print(paste0('Done'))
  return(x)
}
extractOcc <- function(gc){
  # gc <- gcm[1]
  # yr <- '2050'
  load('Z:/_cocoa_cam/_rds/_run12/clustereddata.rData')
  occ <- occ[,c(1, 2)]
  occ$id <- 1:nrow(occ)
  
  extractPnt <- function(pos){
    f <- fls[pos]
    r <- raster(f)
    v <- raster::extract(r, occ[,1:2])
    d <- data.frame(variable = names(r), values = v) %>% 
      dplyr::select(values) %>% 
      setNames(names(r))
    return(d)
  }
  fls <- list.files(paste0('Z:/_cocoa_cam/_data/_tif/_climate/_future/_rcp60/_2050/', gc), full.names = TRUE) %>% 
    mixedsort() 
  stk <- raster::stack(fls)
  cl <- makeCluster(17)
  registerDoSNOW(cl)
  x <- foreach(i = 1:length(fls), .packages = c('raster', 'rgdal', 'dplyr', 'tibble'), .verbose = TRUE) %dopar% {
    extractPnt(pos = i)
  } 
  stopCluster(cl)
  y <- do.call(cbind, x) %>% 
    as_tibble() %>% 
    mutate(gcm = gc,
           year = yr)
  print('Done!')
  return(y)
}
myWrite <- function(pos){
  tbl <- rsl[[pos]]
  g <- unique(tbl$gcm)
  write.csv(tbl, paste0('../tbl/clm/occ_', g, '.csv'), row.names = FALSE)
  print('Done!')
}


# msk <- paste(pth, '_2050', gcm[1], 'bio_1.asc', sep = '/') %>% raster()
# msk <- msk * 0 + 1
# msk <- raster('../raster/bse/msk.asc')
# msk.tbl <- rasterToPoints(msk) %>% as_tibble()
# writeRaster(msk, '../raster/bse/msk.asc', overwrite = TRUE)
# smp <- dplyr::sample_n(tbl = msk.tbl, size = nrow(msk.tbl) * 0.2, replace = FALSE)
# rst.smp <- rasterFromXYZ(xyz = smp)
# writeRaster(rst.smp, '../raster/bse/msk_smp.asc', overwrite = TRUE)
# rst.smp <- raster('../raster/bse/msk_smp.asc')
# shp_msk <- shapefile('../shp/bse/msk_smp.shp')
# 
# # Applying the function ---------------------------------------------------
# dfm <- map(.x = gcm, .f = viewData)
# saveRDS(object = dfm, file = '../rData/tbl_gcms_bios.rds')
# dfm <- readRDS('../rData/tbl_gcms_bios.rds')
# dfm <- map(.x = 1:length(dfm), .f = addID)
# df2 <- bind_rows(dfm)
# df2 <- gather(df2, var, value, -x, -y, -gcm, -year)
# saveRDS(object = df2, file = '../rData/tbl_gcms_bios_gth.rds')
