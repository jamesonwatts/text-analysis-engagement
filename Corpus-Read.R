
library(tidyverse)
library(tidytext)
library(pdftools) 
library(tm)
library(readxl)


### Read in file names from directories

dir <- "../Corpus/"
names <- list.files(dir, pattern = "*.pdf", recursive = FALSE)
pdfs <- paste0(dir, list.files(dir, pattern = "*.pdf", recursive = FALSE)) #adds directory to front


# create a tibble of document names and ids
details <- names %>%
  str_remove(pattern = ".pdf") %>%
  as.tibble() %>%
  mutate(ids = as.character(seq_along(names))) %>% 
  right_join(y=read_excel(str_c(dir,"Database.xlsx")),by=c("value"="ID NUMBER"))

details 

### Read the text from pdf's with pdftools package

# Read the text from pdf's with pdftools package
pdfs_text <- map(pdfs, pdftools::pdf_text)

# Convert to document-term-matrix
# add cleaning process
converted <-
  Corpus(VectorSource(pdfs_text)) %>%
  DocumentTermMatrix(
    control = 
      list(removePunctuation = TRUE,
           stopwords = TRUE,
           tolower = TRUE,
           stemming = TRUE,
           removeNumbers = TRUE,
           bounds = list(global = c(5, Inf)))) # term must exist in at least 5 documents


### Now I want to convert this to a tidy format
#add names of documents (note that this is possible because list.files always returns files in same order)

engagement <- converted %>%
  tidy() %>%
  arrange(desc(count)) %>% 
  left_join(y = details, by = c("document" = "ids")) %>% 
  filter(term != "use" & term != "can" & term!= "also") %>% # don't care about these terms
  rename("Author"="Author, editor or organization") %>% 
  mutate(year=as.numeric(`Year derived`)) %>% 
  filter(year < 2019)

set_names(engagement,tolower(names(engagement)))
names(engagement)<-str_replace_all(names(engagement), c(" " = "_" , "," = "" ))

engagement <- engagement %>% 
  group_by(term) %>% 
  summarize(total=sum(count)) %>% 
  arrange(total) %>% 
  left_join(engagement, by=c("term"="term")) %>% 
  filter(total > 10) %>% # only include terms that show up more than 10 times overall
  rename_all(tolower) %>%
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
  write_rds(path = "engagement.rds")

