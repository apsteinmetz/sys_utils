# mp4 to jpeg
#r code to read an mp4 video file and save still frames from the video at specified intervals
library(av)
interval <- 5.8 # seconds per slide

av::av_video_images("C://Users//Apste//Videos//Carrie Headshot Slideshow.mp4",
                    destdir = "C://Users//Apste//Videos//carrie_headshots",
                    format = "jpg",
                    fps = 1/interval)


# for (frame_no in frame_intervals) {
#   frame <- av::av_seek_frame(video, stream = 0, timestamp = frame_no)
#   if (!is.null(frame)) {
#     jpeg_filename <- sprintf("carrie_headshots//frame_%04d.jpg", frame_no)
#     av::av_write_image(jpeg_filename, frame)
#   }
# }
