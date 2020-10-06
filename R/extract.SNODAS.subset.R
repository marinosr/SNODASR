#' @title Extracts spatial and temporal subsets of SNODAS data and returns them as multiband rasters (one layer per date, one raster per dataset)
#'
#' @param dates Either a vector of date objects or a character vector that can be interpreted by lubridate::ymd(), specifying the dates wanted
#' @param values_wanted A vector of character strings giving the names of the datasets to create subsets of. Possible values are: 'SWE','Depth','Runoff','Sublim_Pack', 'Sublim_Blow', 'P_Solid', 'P_Liquid', 'T_Mean'. The default is to return all.
#' @param extent a 2x2 matrix of the extent desired, following  the format format of input to extent(), or an Extent object. Must be latlong (negative degrees E, degrees N) with WGS84 datum. No modular arithmetic will be performed on the coordinates.
#' @param masked Logical. Are the source files masked to CONUS or unmasked?
#' @param read_path A character string specifying the source directory of SNODAS data, with a trailing slash (e.g. './SNODAS/')
#' @param compressed Logical. Are the datasets compressed? If so, they will be uncompressed before data extraction, then deleted. This will save a LOT of storage space but will blow up processing times approximately 10-fold.
#' @param nodata_handling A character string. If data does not exist, do you want NAs ('NA'), an error ('error') or just no band in the returned rasters ('skip') for that day?
#' @param write_file Logical. Should the extracted data be written to storage or returned in memory. If returned in memory, the function will return a list of rasters with one object for each values_wanted.
#' @param write_path A character string specifying the path to write data, if write_file=TRUE. With trailing slash (e.g. './SNODAS/')
#' @param filename_prefix A character string that will be prepended to the files written to storage. Helpful for including a unique identifier (e.g. Site ID) in the file names.
#' @param write_extension #A character string. File extension to save outputted data. Will be parsed by raster::writeRaster do determine appropriate format (see ?writeRaster for possible formats). Including leading period (e.g. '.img')
#'
#' @return NULL if write_file=TRUE, otherwise a list of the desired multiband rasters.
#'
#' @examples download.SNODAS(dates=c('2010-02-20', '2015-02-10'), path='./SNODAStest/')
#'

#' @export
extract.SNODAS.subset <- function(dates,
                                  values_wanted = c('SWE','Depth','Runoff','Sublim_Pack', 'Sublim_Blow', 'P_Solid', 'P_Liquid', 'T_Mean'),
                                  extent, #
                                  masked=TRUE,
                                  read_path = './SNODAS/',
                                  compressed = FALSE,
                                  nodata_handling='NA',
                                  write_file = TRUE,
                                  write_path = './SNODAS/extracted', #With trailing slash
                                  filename_prefix = '',
                                  write_extension='.img'
) {

  #Translates desired extent into pixel values desired (grid_extent). Note that for grid_extent ymin>ymax because grid index increases N to S, but lat increases S to N.

  dates <- lubridate::ymd(dates)

  #Create path if it doesn't exist.
  if(!(dir.exists(write_path))){
    dir.create(write_path)
  }

  #This code translates the desired geographic coordinates into grid indices for data extraction, and also calculates the extent of the retrieved grid.
  {
    extent <- as.matrix(extent)

    SNODAS_extent <- matrix(c(-124.733749999998366,-66.942083333334011,24.949583333333454,52.874583333332339), nrow=2, byrow = TRUE)

    if(!(identical(as.logical(SNODAS_extent<extent),c(T,T,F,F)))){
      stop('Specified extent exceeds SNODAS data spatial extent.')
    }


    temp <- abs(SNODAS_extent-extent)/0.008333333333333
    #Size of the grid, USING ZERO-BASED NUMBERING (easier for indexing, as seen later)
    grid_extent <- matrix(c(floor(temp[1,1]),
                            6935-floor(temp[1,2]), #The grids are 6935 x 3351, which should have this code make more sense.
                            3351-floor(temp[2,1]),
                            floor(temp[2,2])),
                          nrow=2,
                          byrow=T)

    #Note, grid_extent_latlong will be slightly larger than extent because the grid includes all pixels partially in extent.
    grid_extent_latlong <- matrix(c(SNODAS_extent[1,1]+(grid_extent[1,1])*0.008333333333333,
                                    SNODAS_extent[1,1]+(grid_extent[1,2])*0.008333333333333,
                                    SNODAS_extent[2,2]-(grid_extent[2,1])*0.008333333333333,
                                    SNODAS_extent[2,2]-(grid_extent[2,2])*0.008333333333333), nrow=2, byrow=T)
  }

  #Initialize list if needed.
  if(write_file==FALSE){
    list_to_return <- list()
  }

  for(l in 1:length(values_wanted)){
    #This constructs the file names of the data desired.
    {
      name_template <- switch(values_wanted[l],
                              SWE= 'YY_ssmv11034tS__T0001TTNATSXXXXXXXX05HP001.dat',
                              Depth='YY_ssmv11036tS__T0001TTNATSXXXXXXXX05HP001.dat',
                              Runoff='YY_ssmv11044bS__T0024TTNATSXXXXXXXX05DP000.dat',
                              Sublim_Pack='YY_ssmv11050lL00T0024TTNATSXXXXXXXX05DP000.dat',
                              Sublim_Blow='YY_ssmv11039lL00T0024TTNATSXXXXXXXX05DP000.dat',
                              P_Solid='YY_ssmv01025SlL01T0024TTNATSXXXXXXXX05DP001.dat',
                              P_Liquid='YY_ssmv01025SlL00T0024TTNATSXXXXXXXX05DP001.dat',
                              T_mean='YY_ssmv11038wS__A0024TTNATS2004112405DP001.dat')

      if(masked==FALSE){
        name_template <- stringr::str_replace(name_template, 'YY', 'zz')
      } else{
        name_template <- stringr::str_replace(name_template, 'YY', 'us')
      }

      filenames <- stringr::str_replace(name_template, 'XXXXXXXX', stringr::str_remove_all(as.character(dates), '-'))
      print(filenames)

    }

    extracted <- array(dim=c(grid_extent[2,1]-grid_extent[2,2], ncol=grid_extent[1,2]-grid_extent[1,1], length(filenames)))
    toskip <- integer()

    for(k in 1:length(filenames)){

      #Handlings for missing data.
      if(!file.exists(paste0(read_path,filenames[k]))){
        if(nodata_handling=='error'){
          stop(paste(values_wanted[l], 'data for', dates[k], ' not found.'))
        } else if (nodata_handling=='skip'){
          warning(paste(values_wanted[l], 'data for', dates[k], ' not found.'))
          toskip <- c(toskip, k)
        } else if (nodata_handling=='NA'){
          warning(paste(values_wanted[l], 'data for', dates[k], ' not found.'))
        }else {
          stop('Invalid value for argument nodata_handling.')
        }
      }else {

        #The meat of the data reading.

        if(compressed==TRUE){
          #Reads the compressed bytestream up until the last row needed in the raster (to save loading e.g. southeast Texas every time into memory) and then extracts the values needed.
          #seek() doesn't play nice with compressed files is why this code is necessary.
          zipped_file <- gzfile(paste0(read_path, filenames[k],'.gz'), open='rb')
          current_file <- matrix(readBin(zipped_file, 'integer', n = (grid_extent[2,1]*6935), size=2), ncol=6935)
          extracted[,,k] <- current_file[grid_extent[1,1]:grid_extent[1,2],grid_extent[2,2]:grid_extent[2,1]]

        } else {
          current_file <- file(paste0(read_path, filenames[k]), open='rb')
          j <- 1
          for(i in grid_extent[2,2]:(grid_extent[2,1]-1)){ #Iterate over raster rows, going N to S
            seek(con=current_file, (i*6935 + grid_extent[1,1])*2) #Set the file pointer to the byte representing the xmin pixel in the ith row. pointer byte is multiplied by 2 because each pixel in the raster has length of 2 bytes.
            extracted[j,,k] <- readBin(con=current_file, what='integer', n=(grid_extent[1,2]-grid_extent[1,1]), size=2, endian='big') #Read the desired # of pixels from each row.
            j <- j+1
          }
          close(current_file)
        }

      }
    }


    #Clean up and write out data.
    {
      extracted[extracted==-9999] <- NA
      if(length(toskip)>0){
        extracted <- extracted[,,-toskip]
      }
      #Convert extracted data to a georeferenced raster brick.
      raster_output <- raster::brick(extracted, xmn=grid_extent_latlong[1,1], xmx=grid_extent_latlong[1,2], ymn=grid_extent_latlong[2,1], ymx=grid_extent_latlong[2,2], crs= "+proj=longlat +datum=WGS84")
      if(length(toskip>0)){
        names(raster_output) <- dates[-toskip]
      } else {
        names(raster_output) <- dates
      }



      if(write_file==FALSE){
        list_to_return[l] <- raster_output
      }else{
        raster::writeRaster(raster_output, paste0(write_path, filename_prefix,'_',values_wanted[l],'_from_', min(dates), '_to_', max(dates),write_extension))
      }
    }


  }

  if(write_file==TRUE){
    print('Processing completed.')
    return()
  } else {
    print('Processing completed.')
    return(list_to_return)
  }
}
