#scan pdftk batch

#template
#pdftk A=M2Sprint_000001.pdf B=M2Sprint_000002.pdf shuffle A Bend-1 output M2Sprint_Guide_tab01.pdf
library(tidyverse)

base_name <- "M2Sprint_0000"
max_num <- 28
setwd("C:\\Users\\Arthur\\Pictures\\ControlCenter4\\Scan")
odds <- seq(1,max_num,by=2)
evens <- seq(2,max_num,by=2)

for (n in 1:length(odds)){
  
  write_lines(paste0("pdftk A=",base_name,str_pad(odds[n],2,"left","0"),".pdf ",
        "B=",base_name,str_pad(evens[n],2,"left","0"),".pdf ",
        "shuffle A Bend-1 output ",
        "M2Sprint_Prog_Guide_",str_pad(n,2,"left","0"),".pdf"),
  path="m2pdf.bat",
  append = TRUE,
  sep="\r\n")
}
