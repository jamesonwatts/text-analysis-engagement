library(tidyverse)
library(tidytext)
library(pdftools) 
library(tm)
library(readxl)
library(SnowballC)

# Read the text from pdf's with pdftools package
dir <- "../Corpus/"
ids <- str_remove(list.files(dir, pattern = "*.pdf", recursive = FALSE),pattern = ".pdf")
files <- paste0(dir, list.files(dir, pattern = "*.pdf", recursive = FALSE)) #adds directory to front
pdfs <- map(files, pdftools::pdf_text)

## Get rid of last three paragraphs (references etc.)
#TODO: figure out better way of determining references
trim = 2 # number of paragraphs to trim from the end
for(i in 1:length(pdfs)){
  if(length(sample)>trim){
    pdfs[[i]] <- pdfs[[i]][1:(length(pdfs[[i]])-trim)]
  }
  pdfs[[i]] <- paste(sample[[i]],sep = " ", collapse = "")
}

# Create token db
data(stop_words)
engagement <- tibble(document=ids, text=pdfs) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% # get rid of stop words
  mutate(term = wordStem(word)) %>%
  count(document, term) %>%
  filter(!str_detect(string = term, pattern = "[0-9+]")) %>%  # get rid of numbers
  filter(str_detect(string = term, pattern = "[a-z+]")) %>%  # get rid of numbers
  filter(str_length(term)>3) %>%  # get rid of strings shorter than 3 characters
  right_join(y=read_excel(str_c(dir,"Database.xlsx")),by=c("document"="ID NUMBER")) %>% 
  rename("Author"="Author, editor or organization") %>% 
  mutate(year=as.numeric(`Year derived`)) %>% 
  filter(year < 2019) %>% 
  rename_all(tolower) 

names(engagement)<-str_replace_all(names(engagement), c(" " = "_" , "," = "" ))

engagement <- engagement %>% 
  group_by(term) %>% 
  summarize(total=sum(n)) %>% 
  right_join(engagement) %>% 
  filter(total > 10) %>% # only include terms that show up more than 10 times overall
  mutate(periodical_short=str_extract(periodical,"^[^\\(]+")) %>% # get rid of periodical in parentheses
  mutate(periodical_short=str_replace_all(periodical_short,"[Jj]ournal","J.")) %>% 
  mutate(periodical_short=str_replace_all(periodical_short,"[Ii]nternational","Int.")) %>% 
  mutate(periodical_short=str_replace_all(periodical_short,"[Ee]uropean","E.")) %>% 
  mutate(periodical_short=str_replace_all(periodical_short,"[Mm]anagement","Mgmt.")) %>% 
  mutate(periodical_short=str_replace_all(periodical_short,"[Oo]rganization[al]+","Org.")) %>%
  mutate(periodical_short=str_replace_all(periodical_short,"[Oo]rganizations","Orgs.")) %>%
  mutate(periodical_short=str_replace_all(periodical_short,"[Aa]ccounting","Acct.")) %>%
  mutate(
    time_period=case_when(
      year < 2005 ~ '< 2005',
      year >= 2005 & year < 2010 ~ '2005-2009',
      year >= 2010 & year < 2015 ~ '2010-2014',
      year >= 2015 ~ '2015 <'
    )
  ) %>% 
  mutate(
    research_field=case_when(
      research_field=="Marketing and Service Research" ~ "Marketing",
      research_field=="Tourism and Leisure Research" ~ "Tourism",
      research_field=="Organisational Studies and Management Research" ~ "Management",
      research_field=="Public Management Research" ~ "Public"
    )
  ) %>% 
  rename("count"="n") %>% 
  write_rds(path = "engagement.rds")

