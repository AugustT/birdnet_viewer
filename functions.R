# Read these all in and set to a data.frame
read.csv.row <- function(file){
  x <- data.table::fread(file, header = TRUE, stringsAsFactors = FALSE)
  if(nrow(x) > 0){
    return(x)
  } else {
    return(NULL)
  }
}

## Function for extracting the time from the file name
file_to_time <- function(filename){
  
  i <- substr(x = filename, 
              start = nchar(filename) - 18, 
              stop = nchar(filename) - 4)  
  
  return(strptime(as.character(i), format = "%Y%m%d_%H%M%S"))
  
}

# Given a clip ID extract it from the wav
clip_audio <- function(df, index){
  
  print(df[df$ID == index, 1])
  wav <- readWave(as.character(df[df$ID == index, 1]), 
                  from = df$start[df$ID == index] - 1,
                  to = df$end[df$ID == index] + 1,
                  units = 'seconds')
  return(wav)
  
}