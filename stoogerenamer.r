#rename ripped DVD titles
# extract episodes from wikipedia and build a table of meta data
#rename files according to template

library(stringr)
library(zoo)
library(tidyverse)
mapnums<-list("Eight"=8,"Five"=5, "Four"=4, "One"=1, "Seven"=7, "Six"=6, "Three"=3, "Two"=2)
#----------------------------------------------------------------------
#three Stooges pasted from wikipedia
tracks<-readLines('TSUCtracks.txt')

#parse into an episode database
years<-str_extract(tracks,"[0-9]{4}")%>%na.locf()
volume<-str_trim(str_extract(tracks,"Volume [A-Za-z]+"))%>%word(2)%>%na.locf()%>%as.character()
disc<-str_trim(str_extract(tracks,"Disc [A-Za-z]+"))%>%word(2)%>%na.locf()%>%as.character()
episodesRaw<-str_trim(str_extract(tracks,"[0-9]{3} [0-9a-zA-Z-!,. ']+"))
episodes<-data.frame(Volume=volume[-1],
                     Disc=disc,
                     Num=as.integer(word(episodesRaw[-1],1)),
                     Name=word(episodesRaw[-1],2,-1),
                     Year=as.integer(years[-1]))%>%na.omit()
episodes$Volume=as.character(mapnums[as.character(episodes$Volume)])
episodes$Disc=as.character(mapnums[as.character(episodes$Disc)])
#-------------------------------------------------------
# form new file names
newtitles<-data.frame(num=episodes$Num,
                      newName=paste("Three Stooges ", episodes$Name," ",episodes$Num," [",episodes$Year,"].mp4",
                                    sep = ""))

newtitles$newName<-as.character(newtitles$newName)
#------------------------------------------------------
#since episodes$Year is a factor, as.integer returns the ordinal index of the factor.
#so we use it as the season count, starting with 1.
plextitlesA<-data.frame(num=episodes$Num,
                       fName=paste("Three Stooges - ",
                                   "s",str_pad(as.integer(episodes$Year),2,"left",pad="0"),
                                   "e",str_pad(episodes$Num,3,"left",pad="0"),
                                   " - ",
                                   episodes$Name,
                                   " (",episodes$Year,")",
                                   ".mp4",
                                   sep = ""))
                       
plextitlesA$fName<-as.character(plextitles$fName)


curryear <-0
trackNum<-1
plextitlesB<-data.frame(num=NULL,fName=NULL)

for (n in 1: nrow(episodes)) {
  if (curryear!=episodes[n,]$Year) {
    trackNum<-1
    curryear<-episodes[n,]$Year
  }
  plextitlesB<-rbind(plextitlesB,data.frame(num=episodes[n,]$Num,
                                            fName=paste("Three Stooges - ",str_pad(episodes[n,]$Num,3,"left",pad="0"),
                                                        " - ",
                                                        episodes[n,]$Name,
                                                        " (",episodes[n,]$Year,") ",
                                                        "(s",str_pad(as.integer(episodes[n,]$Year),2,"left",pad="0"),
                                                        "e",str_pad(trackNum,2,"left",pad="0"),
                                                        ").mp4",sep="")))
  trackNum<-trackNum+1
}

plextitlesB$fName<-as.character(plextitlesB$fName)

#-----------------------------------------------
# form ripped file names  according to convention used when ripped
# I used "TS <volume>-<disk> (trackNum).mp4"
#(tracknum) is created by windows when I batch renamed the files.  Make sure the order of the files
#in the folder is the same as the order of the tracks on the disk.
discflag <-"1"
currdisc<-"1"
trackNum<-1
ripTitles<-data.frame(num=NULL,oldName=NULL)
for (n in 1: nrow(episodes)) {
  if (currdisc!=episodes[n,]$Disc) {
    trackNum<-1
    currdisc<-episodes[n,]$Disc 
  }
  ripTitles<-rbind(ripTitles,data.frame(num=episodes[n,]$Num,
                      oldName=paste("TS ",episodes[n,]$Volume,"-",episodes[n,]$Disc," (",trackNum,").mp4",
                            sep='')))
  trackNum<-trackNum+1
}
ripTitles$oldName<-as.character(ripTitles$oldName)
# end of stooges




#-----------------------------------------------------
# DO THIS THING
fromName<-as.character(plextitlesB$fName)
toName<-fromName
season0year<-episodes$Year[1]-1
setwd("V:\\TV Shows\\The Three Stooges")
list.files()
for (n in 1: nrow(episodes)) {
  season<-episodes$Year[n]-season0year
  path<-paste("Season",str_pad(season,width=2,side="left",pad="0"))
  if (dir.exists(path)==FALSE){
    print(paste('Creating',path))
    dir.create(path)
  }
  if (file.exists(fromName[n])) {
    pathtoname <- paste(path,"\\",toName[n],sep='')
    print(pathtoname)
    file.rename(from=fromName[n],to=pathtoname)
  }
  else {
    print(paste(fromName[n],'not found'))
  }
}

  
# #  PLEX Style
# for (n in 1: nrow(episodes)) {
#   if dir.exists(paste('Season',as.integer(episodes[1,]$Year))){
#     #make dir routines
#   }
#   if (file.exists(newtitles[n,]$newName)) {
#     print(paste(newtitles[n,]$newName,plextitles[n,]$fName))
#     #file.rename(from=newtitles[n,]$newName,to=plextitles[n,]$fName)
#   }
#   else {
#     print(paste(newtitles[n,]$newName,"not found"))
#   }
# }

setwd("C:\\Users\\Arthur\\Documents\\R Projects\\DOSRenamer")
