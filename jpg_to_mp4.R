# merge directory full of jpgs into mp4
library(av)

iteration = 2
setwd("C:\\Users\\Arthur\\Dropbox\\projects\\python\\arlo\\snapshots")


make_movie <- function(target){
  cat("making movie of",target)
  target_files <- dir(pattern = paste0("^",target))
  av::av_encode_video(target_files,
                      output = paste0("ferncliff_",target,"_timelapse_",iteration,".mp4"),framerate = 20)
}

cams <- c("garage","balcony","House","lake")

for (cam in cams) make_movie(cams)
