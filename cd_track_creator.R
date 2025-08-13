# create file of track names from image of track list
# Workflow:
# 1.  Scan image as photo
# 2. in PHOTOSHOP create sufficient text contrast for OCR
# 3. in R import image as text
# 4. parse into song title and artist
# 5. create track list with arbitrary delimiter between artist and song title
# 6. export track list to clipboard
# 7. in EAC import metadata from clipboard
# 8. separate song title and artist 

library(tidyverse)
library(tesseract)
library(tokenizers)
library(clipr)

file = "data/crayons.png"
text_raw <- tesseract::ocr(file)
text <- text_raw %>% 
  tokenize_regex(pattern = "\\n") %>% 
  unlist() %>% 
  enframe()



fix(text)
# all of this will vary depending on format
tracks <- text[complete.cases(text),] %>% 
  mutate(track_no = str_extract(value,"^[0-9]{1,2}")) %>% 
  mutate(title = str_squish(str_remove(value,"^[0-9 .)()]{1,4}"))) %>%
  mutate(artist = ARTIST) %>% 
  select(track_no,title,artist)

fix(tracks)

# all of this will vary depending on format
tracks2 <- tracks %>% 
  separate(title,into = c("artist","title"),sep = ":") %>% 
  mutate_all(str_to_title) %>% 
  mutate_all(str_squish) %>% 
  filter(str_length(title) > 0) %>% 
#  select(track_no,title,artist,metadata) %>% 
  {.}


#final check
fix(tracks2)

tracks2 <- tracks2 %>% 
  mutate(metadata = paste(title,artist,sep="/"))

tracks2$metadata %>% write_clip()
# now go to EAC and import


