# GREENPEACE WORDS
# code was adapted and edited from 
# https://www.r-bloggers.com/creating-a-word-cloud-on-r-bloggers-posts/
# by Mari Feldhaus
#
#       ___           ___           ___          _____          ___     
#      /__/\         /  /\         /  /\        /  /::\        /  /\    
#     _\_ \:\       /  /::\       /  /::\      /  /:/\:\      /  /:/_   
#    /__/\ \:\     /  /:/\:\     /  /:/\:\    /  /:/  \:\    /  /:/ /\  
#   _\_ \:\ \:\   /  /:/  \:\   /  /:/~/:/   /__/:/ \__\:|  /  /:/ /::\ 
#  /__/\ \:\ \:\ /__/:/ \__\:\ /__/:/ /:/___ \  \:\ /  /:/ /__/:/ /:/\:\
#  \  \:\ \:\/:/ \  \:\ /  /:/ \  \:\/:::::/  \  \:\  /:/  \  \:\/:/~/:/
#   \  \:\ \::/   \  \:\  /:/   \  \::/~~~~    \  \:\/:/    \  \::/ /:/ 
#    \  \:\/:/     \  \:\/:/     \  \:\         \  \::/      \__\/ /:/  
#     \  \::/       \  \::/       \  \:\         \__\/         /__/:/   
#      \__\/         \__\/         \__\/                       \__\/    


# load packages
library(rvest)
library(stringr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(tidyverse)
library(htmlwidgets)
library(webshot)
library(ggplot2)

scrape_text <- function(site)
{
  # scrape HTML from input site
  source_html <- read_html(site)
  
  # grab the text attribute of each page
  texts <- source_html %>% html_nodes("div") %>% html_nodes("p")
  
  # parse out just the article title (removing the words "Permalink to ")
  texts <- gsub("\n", "", texts)
  texts <- gsub("\t", "", texts)
  texts <- gsub("\r", "", texts)
  texts <- gsub("/r", "", texts)
  texts <- gsub("<p>", " ", texts) 
  texts <- gsub("</p>", " ", texts)
  texts <- gsub("strong", " ", texts)
  texts <- gsub("gstrong", " ", texts)
  texts <- gsub("mlstrong", " ", texts)
  texts <- gsub("pstrong", " ", texts)
  texts <- gsub("  ", "", texts) #remove double spaces to only get text
  
  # filter out any texts that are NA (where no text was found)
  texts <- texts[!nchar(texts) < 2]
  
  # delete duplicated texts
  texts <- texts[!duplicated(texts)]
  
  # return vector of texts
  return(texts)
}

root <- read_html("https://www.greenpeace.ch/de/handeln/")

#find links on the root webpage to use as other sources
links <- root %>% html_nodes("a") %>% html_attr("href")
used_links1 <- links[startsWith(links, "https://www.greenpeace.ch/de/handeln/")][c(3:9,11:15)] # did not use recipes

root2 <- read_html("https://www.greenpeace.ch/de/erkunden/")

#find links on the root webpage to use as other sources
links2 <- root2 %>% html_nodes("a") %>% html_attr("href")
used_links2 <- links2[startsWith(links2, "https://www.greenpeace.ch/de/tag/")]
used_links2 <- used_links2[!duplicated(used_links2)]

used_links <- c(used_links1, used_links2)

# use our function to scrape the title of each post
all_texts  <- lapply(used_links, scrape_text)

# collapse the titles into a vector
all_texts <- unlist(all_texts)

# delete duplicated texts
all_texts <- all_texts[!duplicated(all_texts)]

#delete text parts which are also included in other texts
for (i in 1:length(all_texts)){
  for (j in 1:length(all_texts)){
    if (i != j){
      if (!is.na(charmatch(all_texts[i], all_texts[j]))){
        paste(i)
        all_texts <- all_texts[-i]
      }
    }
  }
}

## Clean up the titles vector
#############################

# convert all titles to lowercase
cleaned <- tolower(all_texts)

# remove any numbers from the titles
cleaned <- removeNumbers(cleaned)

# remove English stopwords
cleaned <- removeWords(cleaned, stopwords("german"))

# remove punctuation
cleaned <- removePunctuation(cleaned)

# remove spaces at the beginning and end of each title
cleaned <- str_trim(cleaned)

# convert vector of titles to a corpus
cleaned_corpus <- Corpus(VectorSource(cleaned))

cleaned_corpus <- tm_map(cleaned_corpus, stemDocument)

doc_object <- TermDocumentMatrix(cleaned_corpus)
doc_matrix <- as.matrix(doc_object)

# get counts of each word
counts <- sort(rowSums(doc_matrix),decreasing=TRUE)

# filter out any words that contain non-letters
counts <- counts[grepl("^[a-z]+$", names(counts))]

# create data frame from word frequency info
frame_counts <- data.frame(word = names(counts), freq = counts)
frame_counts[order(frame_counts$word),][startsWith(levels(frame_counts$word), "dmd"),2] = 0
frame_counts[order(frame_counts$word),][startsWith(levels(frame_counts$word), "class"),2] = 0
frame_counts[order(frame_counts$word),][startsWith(levels(frame_counts$word), "noopen"),2] = 0
frame_counts[order(frame_counts$word),][startsWith(levels(frame_counts$word), "relno"),2] = 0
frame_counts[order(frame_counts$word),][startsWith(levels(frame_counts$word), "arial"),2] = 0
frame_counts[order(frame_counts$word),][startsWith(levels(frame_counts$word), "tab"),2] = 0
frame_counts[order(frame_counts$word),][startsWith(levels(frame_counts$word), "target"),2] = 0
frame_counts[order(frame_counts$word),][startsWith(levels(frame_counts$word), "time"),2] = 0
frame_counts[order(frame_counts$word),][startsWith(levels(frame_counts$word), "dass"),2] = 0
frame_counts[order(frame_counts$word),][startsWith(levels(frame_counts$word), "unsere"),2] = 0
levels(frame_counts$word)[levels(frame_counts$word) == "greenpeac"] <- "greenpeace"


# create a wordcloud using wordlcoud2

set.seed(1002)
wordcloud(words = frame_counts$word, freq = frame_counts$freq, min.freq = 1,
          max.words=100, random.order=FALSE, random.color = F, rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))

wordcloud2(frame_counts[1:100,], minRotation = pi/2, color=rep_len( c("green","blue"), nrow(frame_counts[1:100,])),
           maxRotation = pi/2, rotateRatio = 0.5,fontFamily = "Miso")



# install webshot package if it is not installed
# webshot::install_phantomjs()

fc <- colorRampPalette(c("darkgreen", "green"))
wordcloud2(frame_counts[1:130,], minRotation = pi/2, 
           color= rep_len( fc(20), nrow(frame_counts[1:100,])),
           maxRotation = pi/2, rotateRatio = 0.5,fontFamily = "Miso", size = 0.6)

saveWidget(wc,"wordcloud.html",selfcontained = F)
webshot::webshot("wordcloud.html","wordcloud.png",vwidth = 2000, vheight = 1744, delay = 5)

# lolipop plot of most used words / not flipped
frame_counts$sorti <- rep(1000,dim(frame_counts)[1])
k = 1
for (l in order(frame_counts$freq, decreasing = T)){
  frame_counts$sorti[l] <- seq(1,dim(frame_counts)[1])[k]
  k <- k+ 1
}

ggplot(data = frame_counts[order(frame_counts$freq, decreasing = T)[1:40],], aes(x = sort(factor(sorti), decreasing = F), y = freq)) +
  geom_point(aes(colour = freq), alpha = 1, shape = "circle", size = 2) + 
  geom_segment(aes(x = sort(factor(sorti), decreasing = F), 
                   xend = sort(factor(sorti), decreasing = F), 
                   y = 0, yend = freq, colour = freq)) + 
  geom_text(aes(y = freq + 12, label = freq), size = 3, 
            family = "TT Times New Roman", colour = "black") +
  scale_x_discrete(breaks=c(as.character(seq(1,40))),
                   labels=c(as.character(frame_counts[order(frame_counts$freq, decreasing = T)[1:40],1]))) +
  scale_color_gradient(low = "green", high = "darkgreen", space = "Lab" , guide = F) +
  xlab("") + ylab("frequency")  +
  theme_minimal() +
  theme(text=element_text(size=10,
                          family="TT Times New Roman", colour = "black"),
        axis.text.x = element_text(size=10,
                                   family="TT Times New Roman", colour = "black",
                                   angle = 90, hjust = 1, vjust = 0.3))
ggsave('~/lolipopWords.png', width = 8, height = 4, dpi = 300)

