

# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools, foreach, doSNOW)


# Functions to use --------------------------------------------------------
readTbl <- function(pattern, period){
  # pattern <- 'occ'; period <- '2050'
  tbl <- grep(pattern, fls, value = TRUE) %>% 
    grep(period, ., value = TRUE) %>% 
    map(.x = ., .f = read_csv)
  return(tbl)
}
calcDifference <- function(var){
  # var <- 'bio_1'
  delta <- function(gc){
    # gc <- gcm[1]
    x <- dfm %>% dplyr::select(2, gc) 
    x$delta <- NA
    x[,3] <- x[,2] - x[,1]
    x <- x %>% 
      setNames(c(colnames(x)[1],
                 colnames(x)[2],
                 paste0('dff_', gc)))
    x <- x[,3]
    print('Done!')  
    return(x)
  }
  crn <- smp_crn %>% 
    dplyr::filter(variable == var) %>% 
    dplyr::select(id, value) %>% 
    setNames(c('id', paste0('crn_', var)))
  ftr <- smp_ft2 %>% 
    dplyr::filter(variable == var) %>% 
    mutate(gcm = gsub('/', '', gcm)) %>% 
    spread(gcm, value) %>% 
    dplyr::select(-id, -variable, -year)
  dfm <- cbind(crn, ftr) %>% 
    as_tibble()
  gcm <- ftr %>% dplyr::select(3:ncol(.)) %>% colnames()
  dfm <- map(.x = gcm, .f = delta)
  dfm <- Reduce(cbind, dfm) %>% 
    as_tibble() %>% 
    mutate(id = 1:nrow(.)) %>% 
    gather(gc, value, -id) %>% 
    mutate(variable = var)
  print('Done!')
  return(dfm)
}

addID <- function(pos){
  x <- smp_ftr[[pos]] %>% 
    mutate(id = 1:nrow(.)) %>% 
    print('Done!')
  return(x)
}
