#' @name download.SNODAS
#' @title Downloads SNODAS data from the UC Boulder FTP server
#' @param dates A vector of date objects or strings that can be converted to dates by lubridate::ymd()
#' @param path A path to save the files locally, with a trailing slash (e.g. the default is './SNODAS/'). If the directory does not exist it will be created.
#' @param overwrite Should extant files be overwritten?
#' @param masked Do you want CONUS snodas data or unmasked North America (NB: unmasked has much more restricted dates available)
#' @param parallel  Do you want to download and process files in parallel (for greatly enhanced performance?) via the doParallel package.
#' @param ncores If parallel=TRUE, how many cores should R use for the process. Default is 3 as a "safe" number, but can be adjusted higher if your computer has more cores available.
#' @param unzip Should the files be unzipped or will they be accessed in their compressed state? Unzipping dramatically increases read performance but will take up much more hard drive space. If you're only going to make one spatial subset of the data, leaving the files comopressed will be faster and take up less storage. But if you're going to use the SNODAS data over and over again, unzip the data (storage space permitting)
#'
#'
#' @return NULL (This function is only used for its side effects. SNODAS data for the specified dates will be downloaded and untarballed in the specified path.)
#'
#'@examples download.SNODAS(dates=c('2010-02-20', '2015-02-10'), path='./SNODAStest/')
#'


#Package check fails without marking i as global due to nonstandard eval of foreach()
utils::globalVariables('i')

#'@export

download.SNODAS <- function(dates,
                            path = './SNODAS/',
                            overwrite=FALSE,
                            masked=TRUE,
                            parallel=TRUE,
                            ncores=2,
                            unzip=TRUE){

  #Contstructs URLs given specified dates
  #Good god these are annoying URLs to construct.
  if(masked==TRUE){

    URLpaths <- sapply(dates, function(x){
      x <- lubridate::ymd(x)
      URLpath <- paste0('ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/',
                        lubridate::year(x),
                        '/',
                        stringr::str_pad(lubridate::month(x), width=2, side='left', pad='0'),
                        '_',
                        as.character(lubridate::month(x, label=TRUE, abbr=TRUE)),
                        '/')
      return(URLpath)
    }, USE.NAMES = FALSE)

    filenames <- sapply(dates, function(x){
      x <- lubridate::ymd(x)
      filename <- paste0('SNODAS_',
                         lubridate::year(x),
                         stringr::str_pad(lubridate::month(x), width=2, side='left', pad='0'),
                         stringr::str_pad(lubridate::day(x), width=2, side='left', pad='0'),
                         '.tar')
      return(filename)
    }, USE.NAMES = FALSE)

    URLs <- paste0(URLpaths, filenames)

  } else { #if unmasked rasters are wanted

    URLpaths <- sapply(dates, function(x){
      x <- lubridate::ymd(x)
      URLpath <- paste0('ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/unmasked/',
                        lubridate::year(x),
                        '/',
                        stringr::str_pad(lubridate::month(x), width=2, side='left', pad='0'),
                        '_',
                        as.character(lubridate::month(x, label=TRUE, abbr=TRUE)),
                        '/')
      return(URLpath)
    }, USE.NAMES = FALSE)

    filenames <- sapply(dates, function(x){
      x <- lubridate::ymd(x)
      filename <- paste0('SNODAS_unmasked_',
                         lubridate::year(x),
                         stringr::str_pad(lubridate::month(x), width=2, side='left', pad='0'),
                         stringr::str_pad(lubridate::day(x), width=2, side='left', pad='0'),
                         '.tar')
      return(filename)
    }, USE.NAMES = FALSE)

    URLs <- paste0(URLpaths, filenames)

  }

  #Create path if it doesn't exist.
  if(!(dir.exists(path))){
    dir.create(path)
  }


  #Does the actual downloading, either in parallel or not.
  if(parallel==TRUE){
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)

    foreach::"%dopar%"(foreach::foreach(i = 1:length(URLs)), {
      if(!file.exists(paste0(path,filenames[i])) | overwrite==TRUE){
        tryCatch({
          utils::download.file(URLs[i], paste0(path,filenames[i]), mode='wb')
        },error = function(e){warning(e)})
        if(file.exists(paste0(path,filenames[i]))){
          utils::untar(paste0(path,filenames[i]), exdir=path)
        }

      } else {
        warning(paste(filenames[i], 'exists in the directory specified. Did you want overwrite=TRUE?'))
      }
    })

    files_to_unzip <- list.files(path, pattern='.gz', full.names = TRUE)

    if(unzip==TRUE){
    foreach::"%dopar%"(foreach::foreach(i = 1:length(files_to_unzip)), {
    R.utils::gunzip(files_to_unzip[i])
    })
    }

    parallel::stopCluster(cl)



  } else {
    foreach::"%do%"(foreach::foreach(i = 1:length(URLs)),  {
      if(!file.exists(paste0(path,filenames[i])) | overwrite==TRUE){
        tryCatch({
          utils::download.file(URLs[i], paste0(path,filenames[i]), mode='wb')
        },error = function(e){warning(e)})
        if(file.exists(paste0(path,filenames[i]))){
          utils::untar(paste0(path,filenames[i]), exdir=path)
        }
      } else {
        warning(paste(filenames[i], 'exists in the directory specified. Did you want overwrite=TRUE?'))
      }
    })

    if(unzip==TRUE){
      foreach::"%do%"(foreach::foreach(i = 1:length(files_to_unzip)), {
        R.utils::gunzip(files_to_unzip[i])
      })
    }
  }
  return()
}
