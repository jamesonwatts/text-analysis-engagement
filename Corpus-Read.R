library(tidyverse)
library(tidytext)
library(pdftools) 
library(tm)
library(readxl)
library(SnowballC)

# Read in the excel database and clean it up
dir <- "../Corpus/"
dbx <- as_tibble(read_excel(str_c(dir,"Database.xlsx")))
names(dbx)<-str_replace_all(names(dbx), c(" " = "_" , "," = "" ))
dbx <- dbx %>% 
  rename_all(tolower) %>% 
  rename("author"="author_editor_or_organization") %>% 
  rename("document"="id_number") %>% 
  mutate(year=as.numeric(year_derived)) %>% 
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
  write_rds(path = "articles.rds")

# Read the text from pdf's with pdftools package
ids <- str_remove(list.files(dir, pattern = "*.pdf", recursive = FALSE),pattern = ".pdf")
files <- paste0(dir, list.files(dir, pattern = "*.pdf", recursive = FALSE)) #adds directory to front
pdfs <- map(files, pdftools::pdf_text)

#TODO: figure out better way of determining references
trim = 2 # number of paragraphs to trim from the end
for(i in 1:length(pdfs)){
  if(length(pdfs[[i]])>trim){
    pdfs[[i]] <- pdfs[[i]][1:(length(pdfs[[i]])-trim)]
  }
  pdfs[[i]] <- paste(pdfs[[i]],sep = " ", collapse = "")
}

# Create text db
txt <- tibble(document=ids, text=pdfs) %>% 
  mutate(text=as.character(text)) %>% 
  mutate(text = str_replace_all(text,"[\n\t\r\v\f]"," ")) %>% 
  mutate(text = str_replace_all(text,"\\s+"," "))

for(i in 1:nrow(txt)){
  fileConn<-file(str_c("../Corpus_txt/",txt[i,'document'],".txt"))
  writeLines(as.character(txt[i,'text']), fileConn)
  close(fileConn)
}

# Create token db
data(stop_words)
engagement <- tibble(document=ids, text=pdfs) %>% 
  mutate(text=as.character(text)) %>% 
  mutate(text = str_replace_all(text,"[\n\t\r\v\f]"," ")) %>% 
  mutate(text = str_replace_all(text,"\\s+"," ")) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% # get rid of stop words
  filter(!str_detect(string = word, pattern = "[0-9+]")) %>%  # get rid of numbers
  filter(str_detect(string = word, pattern = "[a-z+]")) %>%  # get rid weird non alphas
  filter(str_length(word)>3) %>%  # get rid of strings shorter than 3 characters
  mutate(term = wordStem(word)) %>%
  count(document, term) %>%
  rename("count"="n") %>% 
  group_by(term) %>% 
  mutate(total=sum(count)) %>% 
  ungroup() %>% 
  filter(total > 10) %>% # only include terms that show up more than 10 times overall
  write_rds(path = "terms.rds")

# Create bigram db
engagement_bi <- tibble(document=ids, text=pdfs) %>% 
  mutate(text=as.character(text)) %>% 
  mutate(text = str_replace_all(text,"[\n\t\r\v\f]"," ")) %>% 
  mutate(text = str_replace_all(text,"\\s+"," ")) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  filter(!str_detect(string = bigram, pattern = "[0-9+]")) %>%  # get rid of numbers
  filter(str_detect(string = bigram, pattern = "[a-z+]")) %>%  # get rid weird non alphas
  count(document, bigram) %>% 
  rename("count"="n") %>% 
  group_by(bigram) %>% 
  mutate(total=sum(count)) %>% 
  ungroup() %>% 
  filter(total > 5) %>% # only include bigrams that show up more than 5 times overall
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(str_length(word1)>3) %>%  # get rid of strings shorter than 3 characters
  filter(str_length(word2)>3) %>%  # get rid of strings shorter than 3 characters
  mutate(word1 = wordStem(word1)) %>%
  mutate(word2 = wordStem(word2)) %>% 
  write_rds(path = "bigrams.rds")

