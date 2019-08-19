library(rvest)
library(plyr) 
library(purrr)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate) 

get.transcripts <- function(url){
  
  urls <- read_html(url) %>% 
    html_nodes("a") %>%
    html_attr("href") # Extract the url attributes
  
  # Relevant links have the 'show_interview.php?' tag
  relevant.urls <- urls[grepl(urls,pattern="show_interview.php?",fixed=T)] 
  
  # Run text extraction function on each link
  transcripts <- rbind.fill(map(relevant.urls,extract.interview.text))
  
  return(transcripts)
}

extract.interview.text <- function(url){
  
  text <- read_html(url) %>%
    html_nodes("td") %>% 
    html_text() # Extract text
  
  # Relevant text 
  text.clean <- text[grepl(text,pattern="FastScripts Transcript by ASAP Sports",fixed=T)]
  
  # Split information into separate strings, remove whitespace at the beginning and end of each
  text.clean.split <- str_trim(str_split(text.clean,pattern="\n")[[2]])
  
  # Remove empty strings, remove the ASAP sports tag from strings
  raw.text <- gsub(text.clean.split[text.clean.split != ""],pattern="FastScripts Transcript by ASAP Sports",replacement="")
  
  # Extract date (in second string)
  date <- mdy(raw.text[2])
  
  # Third string contains the interview subject, though everything is squashed together
  # Put spaces between capital letters, split based on whitespace, then combine the second and fourth token as the subject
  tokens <- unlist(strsplit(gsub('([[:upper:]])',' \\1',raw.text[3]),"[[:space:]]"))
  subject <- paste(tokens[2],tokens[4])
  
  # Find index of the string where the interview starts, which is the first string where 'Q.' serves as a prompt
  interview.starts <- min(which(str_detect(raw.text,"Q.")))

  
  # Identify the interview type by looking for the following keywords in the third string 
  if (str_detect(raw.text[3],"Postgame")) {
    interview.type<-"Postgame"
    
    # Determine winner of game
    tor.pat <- "^.*Toronto -.*?([0-9]+).*"
    tor.pat2 <- "^.*Raptors -.*?([0-9]+).*"
    tor.score <- gsub(tor.pat, "\\1", raw.text[4])
    tor.score2 <- gsub(tor.pat2, "\\1", raw.text[4])
    
    gs.pat <- "^.*State -.*?([0-9]+).*"
    gs.pat2 <- "^.*Warriors -.*?([0-9]+).*"
    gs.score <- gsub(gs.pat, "\\1", raw.text[4])
    gs.score2 <- gsub(gs.pat2, "\\1", raw.text[4])
    
    as.numeric(tor.score)
    as.numeric(gs.score)
    as.numeric(tor.score2)
    as.numeric(gs.score2)
    
    if (gs.score > tor.score | gs.score2 > tor.score2) {
      winner <- "Golden State Win"
    } else if (tor.score > gs.score | tor.score2 > gs.score2){
      winner <- "Toronto Win"
    } else {
      winner <- "Unknown"
    }
    
    
  }else if (str_detect(raw.text[3],"Practice")){
    interview.type<-"Practice"
    winner <- "N/A"
  }else {
    interview.type<-"Pregame"
    winner <- "N/A"
  }
  
  # Actual text is from the above found index onward
  interview.text <- raw.text[interview.starts:length(raw.text)]
  
  # Remove general prompts
  relevant.interview.text <- gsub("^[^Q.]*Q.", "",paste(interview.text))
  
  # Combine information into data frame
  interview.data <- data.frame(Date=date,Person=subject,InterviewType=interview.type,Winner = winner, Text=relevant.interview.text,stringsAsFactors=F)
  
  return(interview.data)
  
}

nba.finals.url <- paste0("http://www.asapsports.com/show_events.php?category=11&year=2019&title=NBA+FINALS%3A+WARRIORS+VS.+RAPTORS")

# Extract links
transcript.urls <- read_html(nba.finals.url) %>%
  html_nodes("a") %>%
  html_attr("href")

# Relevant links have the 'show_event.php?' tag
relevant.transcript.urls <- transcript.urls[grepl(transcript.urls,pattern="show_event.php?",fixed=T)]

# Run function on links and clean up some values
transcripts <- rbind.fill(map(relevant.transcript.urls,get.transcripts)) %>% 
  mutate(Person = gsub(Person,pattern="De",replacement="DeMarcus Cousins")) %>% 
  mutate(Person = gsub(Person,pattern="O",replacement="OG Anunoby")) %>% 
  mutate(Person = gsub(Person,pattern="Alfonzo Mc",replacement="Alfonzo McKinnie")) %>% 
  mutate(Person = gsub(Person,pattern="Patrick Mc",replacement="Patrick McCaw")) %>% 
  mutate(Person = gsub(Person,pattern="Fred Van",replacement="Fred VanVleet")) %>% 
  mutate(Text = gsub(".Q.*", "", Text)) %>%
  filter(grepl(':', Text)) %>%
  filter(!grepl('THE MODERATOR', Text))

# Add column to identify if coach or not
transcripts$PersonType <- ifelse(transcripts$Person %in% c("Nick Nurse","Steve Kerr"),"Coach",ifelse(transcripts$Person %in% c("Masai Ujiri","Bob Myers"),"President","Player"))

# Add index column
transcripts <- cbind(Index = seq.int(nrow(transcripts)), transcripts)
#transcripts$Index <- seq.int(nrow(transcripts))

# Export data frame to csv
#write.csv(transcripts,".csv",row.names=F)