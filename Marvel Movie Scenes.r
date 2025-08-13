# import image as text
library(tidyverse)
library(tesseract)
library(tokenizers)
library(lubridate)
library(hms)


parse_scenes_from_png <- function(file){
  
  text_raw <- tesseract::ocr(file)
  text <- text_raw %>% 
    tokenize_regex(pattern = "\\n") %>% 
    unlist()
  
  text2 <- list()
  n2 <- 1
  for (n in 1:length(text)){
    # detect start of entry
    if (str_detect(text[n],"^[0-9]{1,3}\\.")){
      text2[n2] <- text[n]
      n2 <- n2 + 1
    } else{
      # add wrapped text to previous line
      #    print(paste(text2[n2-1],text[n]))
      text2[n2-1] <- paste(text2[n2-1],text[n])
    }
  }
  
  text3 <- text2 %>% 
    unlist() %>% 
    enframe(name="chron_position") %>%
    # some temp changes to make my regexes works
    mutate(value = str_replace(value,"\\bA ","A_")) %>% 
    mutate(value = str_replace(value,"\\bAbit","A_bit")) %>% 
    # now do real work
    mutate(chron_position = as.numeric(str_extract(value, "^[0-9]{1,3}"))) %>%
    mutate(value = str_remove(value, "^[0-9]{1,3}\\. ")) %>%
    mutate(value = str_replace(value, "\\(pre-credits\\)","pre-credits")) %>%
    mutate(movie_name = str_extract(value,"\\b[A-Z][A-Z \\.\\-\\:]+[0-9]*")) %>% 
    mutate(movie_name = str_remove(movie_name,"[\\(][[:alnum:]]*")) %>% 
    mutate(movie_name = as.factor(str_trim(movie_name))) %>% 
    mutate(location = str_remove(value,"\\b[A-Z][A-Z \\-\\:]+[0-9]*")) %>% 
    mutate(location = str_remove(location,"\\([[:alnum:]: \\-,\\)]*$")) %>% 
    mutate(location = str_remove(location,"(of|of \\. 2|in|at|from)[ ]*$")) %>% 
    mutate(location = str_replace(location,"^A_","A ")) %>% 
    mutate(time_window = str_extract(value,"\\([[:alnum:] ;:,-]+\\)")) %>% 
    mutate(time_window = str_remove_all(time_window,"[\\(\\)]")) %>% 
    mutate(time_window = str_replace_all(time_window,"end at","stop at")) %>% 
    # mutate(time_window = str_remove_all(time_window,";.")) %>% 
    separate(time_window,into = c("start","stop"),sep=",",remove = FALSE) %>% 
    mutate(stop = ifelse(str_starts(start,"stop"),start,stop)) %>% 
    mutate(stop = str_replace(stop,"end","stop")) %>% 
    mutate(start = ifelse(str_starts(start,"stop"),NA,start)) %>%
    mutate(start = ifelse(is.na(start)&!is.na(stop),"0:00:00",start)) %>%
    mutate_if(is.character,str_remove,"(start at |stop at)") %>%
    mutate_if(is.character,str_trim) %>%
    mutate_if(is.character,str_trim) %>%
    # case of 'optional scenes' Ignore them
    mutate(stop = if_else(str_detect(stop,"^[0-9:]+;"),str_extract(stop,"^[0-9:]+"),stop)) %>% 
    mutate(time_start =as_hms(parse_date_time(start,orders=c("HMS","MS")))) %>%
    mutate(time_stop = as_hms(parse_date_time(stop,orders=c("HMS","MS")))) %>%
    mutate(duration_mins = round(difftime(time_stop,time_start,units="mins"),1)) %>%
    {.}
  return(text3)
}

scenes2 <- parse_scenes_from_png("data/mcu2.png")
scenes1 <- parse_scenes_from_png("data/mcu.png")
#So, REVISION! Replace #20-44 with all THIS, and move everything below it down six spaces. 
scenes3 <- parse_scenes_from_png("data/mcu3.png")

scenes <- scenes1[c(1:19,45:100),] %>% 
  bind_rows(scenes2) %>% 
  mutate(chron_position = ifelse(chron_position>44,chron_position+6,chron_position)) %>% 
  bind_rows(scenes3) %>% 
  arrange(chron_position) %>%
  mutate(movie_name = as.factor(movie_name))

# ----------------------------------------
# get release dates from wikipedia
if (!file.exists("data/MCU_release_dates.rdata")){
  library(rvest)
  movie_table <- read_html("https://en.wikipedia.org/wiki/List_of_Marvel_Cinematic_Universe_films") %>% 
    html_nodes("table") %>% .[2] %>%   
    html_table(fill=TRUE) %>% 
    .[[1]] %>% 
    . [,1:2] %>%
    filter(!str_detect(Film,"Phase")) %>% 
    mutate(movie_name = toupper(Film)) %>% 
    mutate(movie_name = str_remove(movie_name,"MARVEL'S ")) %>%
    mutate(movie_name =  as.factor(movie_name)) %>% 
    mutate(US_release_date = parse_date(`U.S. release date`,format = "%B %d, %Y")) %>% 
    select(movie_name,US_release_date) %>% 
    arrange(US_release_date) %>% 
    rowid_to_column(var = "release_order") %>% 
    as_tibble() %>% 
    {.}
  save(movie_table,file="data/MCU_release_dates.rdata")
} else load("data/MCU_release_dates.rdata")
# ---------------------
# get movie chron
movie_chron <- read_csv("data/MCU_movie_chron.csv") %>% 
  mutate(movie_name = as.factor(toupper(movie_name)))


run_times_raw <- "Iron Man (2 hours, 6 minutes), The Incredible Hulk (1 hour, 52 minutes), Iron Man 2 (2 hours, 4 minutes), Thor (1 hour, 55 minutes), Captain America: The First Avenger (2 hours, 4 minutes), The Avengers (2 hours, 23 minutes), Iron Man 3 (2 hours, 10 minutes), Thor: The Dark World (1 hour, 52 minutes), Captain America: The Winter Soldier (2 hours, 16 minutes), Guardians of the Galaxy (2 hours, 1 minute), Avengers: Age of Ultron (2 hours, 21 minutes), Ant-Man (1 hour, 57 minutes), Captain America: Civil War (2 hours, 27 minutes), Doctor Strange (1 hour, 55 minutes), Guardians of the Galaxy Vol. 2 (2 hours, 16 minutes), Spider-Man: Homecoming (2 hours, 13 minutes), Thor: Ragnarok (2 hours, 10 minutes), Black Panther (2 hours, 14 minutes), Avengers: Infinity War (2 hours, 29 minutes), Ant-Man and the Wasp (1 hour, 58 minutes), Captain Marvel (2 hours, 3 minutes), Avengers: Endgame (3 hours, 1 minute), Spider-Man: Far From Home (2 hours, 9 minutes)"
run_times <- tokenize_regex(run_times_raw,"\\),") %>%
  unlist() %>% 
  enframe(name = NULL) %>% 
  separate(value,into=c("movie_name","run_time"),sep = "\\(") %>% 
  mutate(movie_name =as_factor(toupper(str_trim(movie_name)))) %>% 
  mutate(run_time = str_replace(run_time," hour[s]*, ",":")) %>% 
  mutate(run_time = str_replace(run_time," minute[s]*",":00")) %>% 
  mutate(run_time = as_hms(parse_date_time(run_time,orders=c("HMS"))))

# assume (wrongly but usefully) that end credit scene starts two minutes before end of running time
end_scene_time <- 120

scenes_merged <- scenes %>% 
  left_join(movie_table) %>%
  left_join(movie_chron) %>% 
  left_join(run_times) %>% 
  arrange(chron_position) %>% 
  mutate(movie_name = as_factor(str_squish(as.character(movie_name))))

#fudge missing end times and credit roll scene times
# NOT ACCURATE, JUST USEFUL
scenes_final <- scenes_merged %>%  
  # mutate(time_stop = if_else(is.na(stop),run_time,time_stop)) %>% 
  # assume time_stop based on different labels in stop. INNACURATE BUT REASONABLE?
  mutate(time_stop = if_else(str_starts(str_squish(stop),"credits"),as_hms(run_time - 300),time_stop)) %>% 
  mutate(time_stop = if_else(str_detect(str_squish(stop),"before second"),as_hms(run_time - 240),time_stop)) %>% 
  mutate(time_stop = if_else(str_detect(str_squish(stop),"before post"),as_hms(run_time - 120),time_stop)) %>% 
  mutate(time_stop = if_else(is.na(time_stop),run_time,time_stop)) %>% 
  mutate(time_start=if_else(str_detect(movie_name,"SPIDER-MAN: FAR FROM HOME"),as_hms(0),time_start)) %>% 
  mutate(time_start=if_else(str_detect(location,"Post-credits"),as_hms(run_time - 120),time_start)) %>% 
  mutate(time_start=if_else(str_detect(location,"Second post"),as_hms(run_time - 120),time_start)) %>% 
  mutate(time_start=if_else(str_detect(location,"First post"),as_hms(run_time - 480),time_start)) %>% 
  mutate(time_stop=if_else(str_detect(location,"First post"),as_hms(run_time - 420),time_stop)) %>% 
  mutate(fudged_times = if_else(is.na(duration_mins),"yes","no")) %>% 
  mutate(duration_mins = as_hms(time_stop - time_start)) %>% 
  {.}

# TEST SUITE ----------------------------------------------------
test1 <- scenes_final %>% group_by(movie_name) %>% 
  summarise(scene_times = sum(duration_mins)) %>% 
  left_join(run_times) %>% 
  mutate(discrepency = run_time - scene_times)
mean(test1$discrepency)
sum(test1$discrepency==0)/nrow(test1)

scenes_final <- scenes_final %>% 
  select(-value,-time_window,-start,-stop)
  
write.csv(scenes_final,file="data/MCU_scene_chron.csv")
# VISUALIZE ------------------------------------------
plot_scene_chron <- function(ordering = c("release","earliest","latest","canon")){
  if (ordering == "canon"){
    ylab = "Canon Movie Order"
    scenes_final <- scenes_final %>% 
      arrange(movie_chron) %>% 
      mutate(movie_name = as_factor(as.character(movie_name)))
  }  
  if (ordering == "release"){
    ylab = "Movie Release Order"
    scenes_final <- scenes_final %>% 
      arrange(release_order) %>% 
      mutate(movie_name = as_factor(as.character(movie_name)))
  }  
  if (ordering == "latest"){
    ylab = "Latest Scene Order"
    scenes_final <- scenes_final %>% 
      arrange(desc(chron_position)) %>% 
      mutate(movie_name = as_factor(as.character(movie_name)))
  }
  if (ordering == "earliest"){
    ylab = "Earliest Scene Order"
    scenes_final <- scenes_final %>% 
      arrange(chron_position) %>% 
      mutate(movie_name = as_factor(as.character(movie_name)))
  }
  ggplot(scenes_final,aes(chron_position,movie_name,color=movie_name)) + geom_point(size=2) + 
    theme(legend.position = "none") + 
    labs(title = "Marvel Cinematic Universe Scene Chronology",
         subtitle = ylab,
         x = "Scene Chronology",y="") + 
    geom_line()
}

# ---------------------------------------
# assign in and out points for each chron position
time_line <- scenes_final %>%
  # line break on long movie names
  mutate(movie_name = str_replace(as.character(movie_name),": ",":\n")) %>% 
  mutate(movie_name = str_replace(as.character(movie_name),"AND THE","AND\nTHE")) %>% 
  mutate(movie_name = str_replace(as.character(movie_name),"OF THE","OF\nTHE")) %>% 
  # movies in canon order
  arrange(movie_chron) %>% 
  mutate(movie_name = as_factor(as.character(movie_name))) %>% 
  select(chron_position,movie_name,time_start,time_stop) %>%
  # set stop time earlier by one second to prevent collision with start times
  mutate(time_stop = time_stop - 1 ) %>% 
  pivot_longer(cols = c(time_stop,time_start),names_to = "start_stop",values_to = "time") %>% 
  group_by(movie_name) %>% 
  arrange(movie_name,time,chron_position)

  
time_line %>%
#  filter(movie_name=="THE AVENGERS") %>% 
  ggplot(aes(time,chron_position,color=movie_name)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = "none") + 
  facet_wrap(~movie_name) + 
  scale_x_time(breaks = c(0,3600,7200),labels = c("0:00,","1:00","2:00")) + 
  labs(title = "MCU Flash-back/forward Map",
       x="Location in Movie (Hour)",y="Scene Chronological Position (Entire Canon)",
       caption="source:@tonygoldmark,@adababbage")
# ----------------------------------------------
plot_scene_chron("canon")
