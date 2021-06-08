# This script extracts keywords in a topic where there is no a priori knowledge about which words are important.
# Good keywords will 1) be frequent in a given document AND 2) will also be frequent in other documents of the same topic.

#Viktoria, S2DS Aug 2020, adapted for a new problem in June 2021


# Steps:
# 1. Calculate the n most frequent words in a given document 
# 2. Calculate how many times these words occur in the document
# 3. Get the proportion of these words over the number of words in the document (Prop_in_doc)
# 4. Get the number of times these words appear in ALL documents
# 5. Get the proportion of these words in ALL documents, given the total number of words in the corpus (Prop_in_total)
# 6. Do this with all the documents
# 7. Check the relative frequency of Prop_in_doc and Prop_in_total: Good words will be frequent in the doc AND also in other docs of the same topic
# 8. Pick the first 10 words from each doc based on relative frequency and plot them in a word cloud

#Output:
# csv file containing the word frequencies
# wordcloud


rm(list=ls())

library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(tidyverse)
library(readtext)
library(textstem)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

base <- getwd()

#Set the number of words you want to consider from each document
n = 50

#Set the number of topics you have (each topic will have a folder with texts in Documents)
topics = c('Topic1', 'Topic2')

#Set the colour palettes for the plots in each topic
palette1 = 'YlOrRd'
palette2 = 'Blues'
#palette3 = 'Purples'
palettes = c(palette1, palette2)

for (topic in topics){
  
  rm(list= ls()[!(ls() %in% c("topic", "base", "n", "topics", "palettes"))])
  
  index = match(c(topic),topics)
  
  #Set the working directory to source file location
  
  #Read in the documents
  dest <- file.path(base, 'Documents', topic)
  myfiles <- list.files(path = dest, pattern = "pdf",  full.names = TRUE)
  myfiles
  
  
  #I. Find the n most frequent words in each text.
  #   Create a table where each document is listed with the n most frequent words and the ratios of these words in the text
  
  #Output table to be populated
  frequencies <- tibble(
    Title = character(),
    Word = character(),
    Num_in_doc = numeric(), #occurrences of this word in this doc
    Num_in_total = numeric(), #occurrences of this word in all docs combined
    Prop_in_doc = numeric(), #(occurrences of this word in this doc/number of words in this doc)*100 
    Prop_in_total = numeric() #(occurrences of this word in all docs/number of words in all docs)*100
  )
  
  #Go through the files in each topic
  for (m in 1:length(myfiles)){
    
    doc <- readtext(myfiles[m])
    text <- doc$text
    
    #this table is for local use to find most frequent words in this doc
    this_doc <- tibble(
      numrows = numeric(),
      doc_name = character(),
      textrows = character()
    )
    
    rows = 1:length(text)
    title = str_remove_all(myfiles[m], dest)
    title = str_remove_all(title, "\\.pdf")
    title = str_replace_all(title, "[[:punct:]]", " ")
    text = str_remove_all(text, " \\([a-z]\\)") #rm letters in brackets, i.e. (a)
    text = str_remove_all(text, "[^a-zA-Z $]") #now remove everything that's not a legit word
    text = tolower(text)
    lemtext <- lemmatize_strings(text)
    
    this_doc <- this_doc %>%
      add_row(numrows = rows, doc_name = title, textrows = lemtext)
    
    #Isolate words
    by_freq <- this_doc %>%
      unnest_tokens(word, textrows)
    num_words <- nrow(by_freq)-1
    
    word_counts <- by_freq %>%
      anti_join(stop_words) %>%
      count(doc_name, word, sort = TRUE) %>%
      arrange(doc_name)
    
    #Words in order of frequency
    word_counts
    
    #Put the x most common words into a table
    frequencies <- frequencies %>%
      add_row(Title = word_counts$doc_name[1:n], Word = word_counts$word[1:n], Num_in_doc = word_counts$n[1:n])
    
    #Add ratio of a word in this doc compared to all words in this doc
    for (r in 1:nrow(frequencies)){
      frequencies$Prop_in_doc[r] <- (frequencies$Num_in_doc[r]/num_words)*100
    } 
    
    rm(word_counts)
    
  }
  
  frequencies <- frequencies %>%
    arrange(Title)
  
  
  #II. Now find the frequencies of the same words in all the documents combined
  #If you are comparing documents in the same topic: Useful words will be frequent in a given doc AND in all the docs combined
  #If you are comparing documents from different topics: Useful words will be frequent in the dic BUT NOT in all the docs combined
  
  all_docs <- tibble(
    Title = character(),
    NumRows = numeric(),
    TextRows = character()
  )
  
  #Put all docs into the same tibble
  for (m in 1:length(myfiles)){
    
    
    #try
    
    doc <- readtext(myfiles[m])
    text <- doc$text
    
    rows = 1:length(text)
    text = str_remove_all(text, " \\([a-z]\\)") #rm letters in brackets, i.e. (a)
    text = str_remove_all(text, "[^a-zA-Z $]") #now remove everything that's not a legit word
    text = tolower(text)
    lemtext <- lemmatize_strings(text)
    
    all_docs <- all_docs %>%
      add_row(Title = str_remove(myfiles[m], dest), NumRows = rows, TextRows = lemtext)
    
    #except
    
  }
  
  #Now get out the frequencies of the words in all docs combined
  by_doc_word <- all_docs %>%
    unnest_tokens(word, TextRows)
  num_all_words <- nrow(by_doc_word)-1
  
  #These are all the words in the docs in descending order of frequency
  tot_words <- by_doc_word %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE) %>%
    arrange(-n)
  
  tot_words
  
  # Add the total number of occurrences to the relevant word in the frequencies/doc table
  for (w in 1:nrow(tot_words)) {
    #Look for exact matches
    pos <- grep(paste("^",tot_words[w,1],"$", sep=""), frequencies$Word)
    for (p in pos) {
      frequencies[p, 4] <- tot_words[w,2]
    }
  }
  
  #Add ratio of a word in all docs compared to all words in all doc
  for (r in 1:nrow(frequencies)){
    frequencies$Prop_in_total[r] <- (frequencies$Num_in_total[r]/num_all_words)*100
  } 
  
  #Get relative frequency (the absolute difference between document frequency and corpus frequency)
  
  #If you are comparing words from the same topic, the most useful words will be common in a single doc AND in a collection of docs
  #If you are comparing docs from different topics, the most useful docs will be common in a single topic BUT NOT in the rest of the docs
  # ---> in this case sort them in descending order of relative frequency: arrange(Title, -Relative_freq)
  
  frequencies <- frequencies %>% 
    mutate(Relative_freq = abs(Prop_in_doc-Prop_in_total)) %>%
    arrange(Title, Relative_freq)
  
  
  write.csv(frequencies, paste('RelativeFrequencies', index, '.csv'))
  
  
  #Now plot the top 10 words from each document
  #These are the best keywords for the topic
  
  words <- frequencies %>%
    group_by(Title) %>%
    top_n(10) %>%
    arrange(-Num_in_total)
  
  #Each word should only be listed once
  #Keep the instance with the largest relative frequency
  #The largest relative frequency is an indication as to how generalizable this keyword is: the smaller the value, the more likely that this word is frequent in ALL documents of the topic 
  words = words[order(words[,'Word'],words[,'Relative_freq']),]
  words = words[!duplicated(words$Word),]
  
  
  
  set.seed(1234)
  wordcloud(words = words$Word, freq = words$Num_in_total, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, palettes[index]))
  
  dev.copy(pdf, paste('Topic', index, '.pdf'))
  Sys.sleep(5)
  while (!is.null(dev.list()))  dev.off()
  
  print('end of a topic')
  
}

