setwd("C:/Users/abc/Downloads/wordcloud")

install.packages("RCurl")
install.packages("pbapply")
install.packages("tm")
install.packages("wordcloud")
install.packages("XML")
library(wordcloud) # For the visual treat
library(tm) # Definitely required to mine text data
library(pbapply) # Progress bars
library(RCurl) # All you need to grab webpages
library(XML) # All you need to parse HTML code

page.url <- paste0("http://www.mldb.org/artist-102-eminem.html")

curl <- getCurlHandle(useragent = "R", followlocation = TRUE)

urls.by.song <- unlist(pblapply(page.url, FUN = function(URL)
  {
  raw.html <- getURL(URL, curl = curl)
  parsed.html <- htmlParse(raw.html)
  links <- as.vector(xpathSApply(parsed.html,"//a[contains(@href,'song')]/@href"))
  if (!is.null(links))
        {
        ix = grepl("http://www.mldb.org/", links)
        links[!ix] <- paste0("http://www.mldb.org/", links[!ix])
        return(links)
        }
 }), use.names = FALSE)
  
for(i in 1:246)
        {
        raw.html <- getURL(urls.by.song[i], curl = curl)
        parsed.html <- htmlParse(raw.html)
        plain.text <- xpathSApply(parsed.html, "//p", xmlValue) #Lyrics of the song are inside the paragraph(/p)
        plain.text<-paste(plain.text, collapse = "\n")
        write(plain.text,file="data.txt",append=TRUE) #creates a file in current working directory
        }
        
        
workdir<-getwd() #passes the current working directory to workdir
b<-Corpus(DirSource(workdir), readerControl = list(language = "eng")) #creates corpus for all documents inside workdir


b<- tm_map(b, tolower) #Changes case to lowercase
b<- tm_map(b, stripWhitespace) #Strips white space
b<- tm_map(b, removePunctuation) #Removes punctuation
b<- tm_map(b, removeWords, stopwords("english")) # Removes stopwords
b<- tm_map(b, removeWords, c("and","the","that","for","eminem")) # Removes words which you feel are undesirable

tdm<- TermDocumentMatrix(b)
m1<- as.matrix(tdm)

v1<- sort(rowSums(m1),decreasing=TRUE)
d1<- data.frame(word = names(v1),freq=v1)

multi<- brewer.pal(8,"Dark2") #used to indicate multicolor word cloud
png("Eminem.png",width=1000,height=800) #Export wordcloud to .png file
wordcloud(d1$word,d1$freq,scale=c(8,.2),min.freq=1,color=multi,max.words=Inf, random.order=FALSE, rot.per=.15)
dev.off()


