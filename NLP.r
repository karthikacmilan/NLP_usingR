library(tm)

library(qdapDictionaries)
library(RColorBrewer)
library(data.table)
library(qdapTools)
library(chron)
library(XML)
library(dplyr)
library(gdata)
library(gtools)
library(gender)
library(NLP)
library(openNLP)
library(gridExtra)
library(openNLPdata)
library(rJava)
library(plotrix)
library(venneuler)
library(xlsxjars)
library(xlsx)
library(qdap)
dest = "C:/Users/Karthik/Documents/posts/"
files = list.files(path = dest, pattern = "pdf", full.names = TRUE)
sapply(files, FUN = function(i){
  file.rename(from = i, to = paste0(dirname(i),"/",gsub(" ", "",basename(i))))
})
files = list.files(path=dest,pattern = "pdf",full.names = TRUE)
lapply(files, function(i) system(paste("C:/Users/Karthik/Documents/pdftotext.exe", paste0('"', i, '"')), wait = FALSE) )
dest1 = "C:/Users/Karthik/Documents/posts"
docs <- Corpus(DirSource(dest1))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, content_transformer(tolower))
qdocs <- as.data.frame(docs)
qdocs$docs <- c("jim","JonArnold","Malcolm")
qdocs$text <- clean(Trim(qdocs$text))
qdocs$text <- replace_symbol(qdocs$text)
qdocs$text <- gsub("Cristiano Ronaldo","Ronaldo",qdocs$text)
qdocs$text <- replace_abbreviation(qdocs$text)
qdocs$text <- mgsub(c("goal","league","star","..","*","**","***","()","( )","-","(",")",",,","(.)",",,","-.*",". .",", ,","|","+","_","-","=","[","]",". ,",".:",":."," . ","<.","` ",", .",".,",",t ."," ., ",". .","^","'")," ",qdocs$text)

sent.docs <- sentSplit(qdocs,"text")
pdf("document.pdf")
plot(sent.docs, grouping.var = "docs")
graphics.off()
sent.docs$scaled <- unlist(tapply(wc(sent.docs$text), sent.docs$docs, scale))
sent.docs$ID <- factor(unlist(tapply(sent.docs$docs, sent.docs$docs, seq_along)))
library(ggplot2)
library(grid)
pdf("analysis.pdf")
ggplot(sent.docs, aes(x = ID, y = scaled, fill = docs)) + geom_bar(stat ="identity", position ="identity",size = 5) + facet_grid(.~docs) + ylab("Standard Deviations") + xlab("Sentences") + guides(fill = guide_legend(nrow = 1, byrow = TRUE,title="Author"))+ theme(legend.position="bottom",axis.text = element_blank(), axis.ticks = element_blank()) +  ggtitle("Standardized Word Counts\nPer Sentence")+coord_flip()

graphics.off()
library(SnowballC)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, removeWords,c("goal ", " league "," star ","()","()","( )","-","(",")",",,","(.)",",,","-.*",". .",".:",":.","[","]","<.","` ",", .",".,",",t ."," ., ",". .","^","'","this","the"))
dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- c("jim","JonArnold","Malcolm")
dtm
freq <- colSums(as.matrix(dtm))
ord <- order(freq)
freq[tail(ord,n=15)]
dtms <- removeSparseTerms(dtm, 0.9)
dtms
findFreqTerms(dtms, lowfreq=3)
pdf("graph.pdf")
plot(dtms,terms =findFreqTerms(dtms, lowfreq=3),corThreshold=0.6)
graphics.off()
library(reshape2)
dtm.mat <- as.matrix(dtm)
dtm.melt <- melt(dtm.mat)
dtm.melt <- as.data.frame(dtm.melt)
knitr::kable(head(dtm.melt,n=10))
term.table <- table(nchar(as.character(dtm.melt$Terms)))
term.mode <- as.numeric(names(term.table[which(term.table==max(term.table))]))
cond <- nchar(as.character(dtm.melt$Terms))*dtm.melt$value == term.mode
pdf("finalanalysis.pdf")
ggplot(dtm.melt) + geom_histogram(data=subset(dtm.melt,cond==FALSE),binwidth=1,aes(x=nchar(as.character(Terms))*value,fill=Docs))+
  facet_grid(Docs~.) + geom_histogram(data=subset(dtm.melt,cond==TRUE),binwidth=1,aes(x=nchar(as.character(Terms))*value),fill="red")+
  labs(x="Number of Letters",y="Number of Words") + xlim(1,20)  +
  guides(fill = guide_legend(nrow = 9, byrow = TRUE ,title="Author"))+ 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ggtitle("Length of each word \n by Author")+
  geom_text(data=data.frame(x=6.5, y=30, label="Mode", stat=c("ta")),aes(x,y,label=label),size=3, inherit.aes=TRUE)


graphics.off()



