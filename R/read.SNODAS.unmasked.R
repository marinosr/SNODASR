
#' @title Converts a raw SNODAS file into a georeferenced raster, saving the file if desired,
#' @param filename A character string specifying the .dat file to convert
#' @param read_path A character string specifying the path to the location of filename, with a trailing slash (e.g. './SNODAS/')
#' @param write_file Logical. Should the converted file be written to storage or returned in memory?
#' @param write_path  A character string specifying the path to write to, with a trailing slash (e.g. './SNODAS/') Directory will be created if it does not exist.
#' @param write_extension  #A character string specifying the file extension to save outputted data. Will be parsed by raster::writeRaster do determine appropriate format (see ?writeRaster for possible formats). Including leading period (e.g. '.img')
#' @return If write_file=FALSE, returns a RasterLayer object, otherwise only used for its side effect of writing a raster file to storage.
#' @examples read.SNODAS('us_ssmv01025SlL01T0024TTNATS2010022005DP001.dat', read_path='./SNODAStest/')
#' @export
read.SNODAS.unmasked <- function(filename,
                        read_path = './SNODAS/',
                        write_file = TRUE,
                        write_path = './SNODAS/',
                        write_extension='.img'){
  file_opened <- file(paste0(read_path, filename), open='rb')
  rawdat <- readBin(file_opened, n=4096*8192, what='integer', size=2, endian='big')
  close(file_opened)
  rawdat[rawdat==-9999]  <- NA
  rawmatrix <- matrix(data=rawdat, nrow=4096, ncol=8192, byrow=TRUE)
  image <- raster::raster(rawmatrix, xmn=-130.512499999995, xmx=-62.2499999999977, ymn=24.0999999999990, ymx=58.2333333333310, crs="+proj=longlat +datum=WGS84")
  if(write_file==TRUE){
    if(!(dir.exists(write_path))){
      dir.create(write_path)
    }
    raster::writeRaster(image, paste0(write_path, unlist(strsplit(filename, '.dat')),write_extension))
    return()
  } else{
  return(image)
  }
}








