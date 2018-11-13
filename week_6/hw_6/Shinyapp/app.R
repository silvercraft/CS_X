#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

knitr::opts_chunk$set(echo = TRUE)
#libraries
library(rvest)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(dplyr)
library(NLP)
library(ggplot2)
library(Matrix)
library(wordcloud)
library(htm2txt)
#data fetching
prefix = "http://www.hitler.org/writings/Mein_Kampf/mkv1ch0"
prefix2 = "http://www.hitler.org/writings/Mein_Kampf/mkv1ch"
prefix3 = "http://www.hitler.org/writings/Mein_Kampf/mkv2ch0"
prefix4 = "http://www.hitler.org/writings/Mein_Kampf/mkv2ch"
f=""
f2=""
data <- list()
for( id in 1:9 )
{
  url  <- paste0( prefix, as.character(id), ".html" )
  page.source2 <- read_html(url)
  kw2 = html_nodes(page.source2, "blockquote")
  f=paste(c(f,html_text(kw2)))
  
  
}
for( id in 10:12 )
{
  url  <- paste0( prefix2, as.character(id), ".html" )
  page.source2 <- read_html(url)
  kw2 = html_nodes(page.source2, "blockquote")
  f=paste(c(f,html_text(kw2)))
  
}
for( id in 1:9 )
{
  url  <- paste0( prefix3, as.character(id), ".html" )
  kw2 = gettxt(url)
  f=paste(c(f,kw2))
  
}
for( id in 10:13 )
{
  url  <- paste0( prefix4, as.character(id), ".html" )
  kw2 = gettxt(url)
  f=paste(c(f,kw2))
  
}
for( id in 14:15 )
{
  url  <- paste0( prefix4, as.character(id), ".html" )
  page.source2 <- read_html(url)
  kw2 = html_nodes(page.source2, "blockquote")
  f=paste(c(f,html_text(kw2)))
  
}
docs <- Corpus(VectorSource(f))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))}
)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, "even")
docs <- tm_map(docs, removeWords, "also")
docs <- tm_map(docs, removeWords, "will")
docs <- tm_map(docs, removeWords, "take")
docs <- tm_map(docs, removeWords, "made")
docs <- tm_map(docs, removeWords, "means")
docs <- tm_map(docs, removeWords, "may")
docs <- tm_map(docs, removeWords, "since")
docs <- tm_map(docs, removeWords, "thus")
docs <- tm_map(docs, removeWords, "always")
docs <- tm_map(docs, removeWords, "make")
docs <- tm_map(docs, removeWords, "can")
docs <- tm_map(docs, removeWords, "must")
docs <- tm_map(docs, removeWords, "\n")
docs <- tm_map(docs, removeWords, "</i>")
docs <- tm_map(docs, removeWords, "<br>")
docs <- tm_map(docs, removeWords, "\r")
#TDM
tdm <- TermDocumentMatrix(docs)
freq=rowSums(as.matrix(tdm))
tail(sort(freq),n=10)
high.freq=tail(sort(freq),n=10)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 
#frequency bar chart
ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")

#wordcloud
m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(d$word, d$freq, min.freq = 10,max.words = 50, random.order = F, ordered.colors = F, 
          colors = rainbow(length(row.names(m1))))
high.freq=tail(sort(freq),n=100)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 

# tf-idf computation
tf <- apply(tdm, 2, sum) # term frequency
idf <- function(word_doc){ log2( (length(word_doc)+1) / nnzero(word_doc) ) }
idf <- apply(tdm, 1, idf)
doc.tfidf <- as.matrix(tdm)
for(i in 1:nrow(tdm)){
  for(j in 1:ncol(tdm)){
    doc.tfidf[i,j] <- (doc.tfidf[i,j] / tf[j]) * idf[i]
  }
}
#matrix adjusting
library(ggfortify)
doc.tfidf<-doc.tfidf[,-c(1,13)] 
include_list <- hfp.df$names
newdoc=doc.tfidf[include_list, ]
colnames(newdoc) <- c("chapter 1", "chapter 2","chapter 3","chapter 4","chapter 5","chapter 6","chapter 7","chapter 8","chapter 9","chapter 10","chapter 11","chapter 12","chapter 13","chapter 14","chapter 15","chapter 16","chapter 17","chapter 18","chapter 19","chapter 20","chapter 21","chapter 22","chapter 23","chapter 24","chapter 25","chapter 26","chapter 27")
#pca+k means
pcat = prcomp(t(newdoc))
autoplot(pcat$x,data = newdoc, shape = FALSE, label.size = 3)
comp <- data.frame(pcat$x[,1:4])
library(rgl)
# Multi 3D plot
plot3d(comp$PC1, comp$PC2, comp$PC3)
plot3d(comp$PC1, comp$PC3, comp$PC4)
kkk=3
set.seed(1)
km=kmeans(comp, kkk,nstart=25, iter.max=1000)
plot(comp, col=km$cluster, pch=16)
plot3d(comp$PC1, comp$PC2, comp$PC3, col=km$cluster)
plot3d(comp$PC1, comp$PC3, comp$PC4, col=km$cluster)
autoplot(km, data = pcat, label = TRUE, label.size = 3)

#plot for each chapters

#nn2=data.frame(tfidf=head(sort(doc.tfidf[,i],decreasing = TRUE),10))
#nn2$names<- rownames(nn2) 
#print(ggplot(nn2, aes(names,tfidf)) +
#        geom_bar(stat="identity") + coord_flip() + 
#        xlab("Terms") + ylab("Frequency") +
#        ggtitle(paste("Chapter ",i)) )


# Define UI for application that draws a histogram
##UI =======================================================================
ui <- navbarPage(
  
  
  # Application title
  "Mein_Kampf",
  
  tabPanel(
    "Intro",
    tags$h2("Introduction"),br(),
    tags$h3("Presented by CYC"),br(),
    tags$h4("In this assignment,we will analyse Hitler's autography"),br(),
    tags$h4("Mein Kampf/My Struggle,which is often referred to as one of the most dangerous book ever written."),br(),
    tags$h4("This book described Hitler's life,showing severe Antisemitism , Militarism and Racism"),br()
  ),
  tabPanel(
    "Wordcloud",
    tags$h2("Word cloud for Mein_Kampf"),br(),
    sidebarPanel(
      sliderInput("wc_max",
                  "words count/freq",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      plotOutput("WordCloud_1")
    )
  ),
  tabPanel(
    "Bar chart",
    tags$h2("Bar chart for Mein_Kampf"),br(),
    sidebarPanel(
      sliderInput("bar_max",
                  "words count/freq",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    hr(),
    helpText("Analyse with a bar chart"),
    mainPanel(
      plotOutput("Bar_1")
    )
  ),
  tabPanel(
    "Every chapters",
    tags$h2("Words frequency in every chapter"),br(),
    sidebarPanel(
      selectInput("cptr_1", "chapter:", 
                  choices=c(1:27)),
      hr(),
      helpText("See what Hitler cared the most during every stage of his life")
      
    ),
    mainPanel(
      plotOutput("Plot_1")
    )
  ),
  tabPanel(
    "PCA",
    tags$h2("PCA"),br(),
    mainPanel(
      plotOutput("Plot_pca")
    )
  ),
  tabPanel(
    "K means 3D",
    tags$h2("Kmeans plot in 3D"),br(),
    mainPanel(
      plotOutput("Plot_3d")
    )
  ),
  tabPanel(
    "PCA and K means analysis",
    tags$h1("Relevance between each chapters"),
    sidebarPanel(
      numericInput("k1",
                   "Number of k:",
                   min = 1,
                   max = 27,
                   value = 5)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plotly_KM1")
    )
  )
)
##SERVER =====================================================================
server <- function(input, output) {
  output$Plot_1 <- renderPlot({
    i <- as.numeric(input$cptr_1)
    
    nn2=data.frame(tfidf=head(sort(doc.tfidf[,i],decreasing = TRUE),10))
    nn2$names<- rownames(nn2) 
    print(ggplot(nn2, aes(names,tfidf)) +
            geom_bar(stat="identity") + coord_flip() + 
            xlab("Terms") + ylab("Frequency") +
            ggtitle(paste("Chapter ",i) ))
  })
  output$WordCloud_1 <- renderPlot({
    wordcloud(d$word, d$freq, min.freq = 10,max.words = input$wc_max, random.order = F, ordered.colors = F, 
              colors = rainbow(length(row.names(m1))))
  })
  output$Bar_1 <- renderPlot({
    inppp = input$bar_max
    high.freq=tail(sort(freq),n=inppp)
    hfp.df=as.data.frame(sort(high.freq))
    hfp.df$names <- rownames(hfp.df) 
    ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
      geom_bar(stat="identity") + coord_flip() + 
      xlab("Terms") + ylab("Frequency") +
      ggtitle("Term frequencies")
  })
  output$Plot_pca <- renderPlot({
    autoplot(pcat$x,data = newdoc, shape = FALSE, label.size = 3)
  })
  output$Plot_3d <- renderPlot({
    kkk=input$k1
    set.seed(1)
    km=kmeans(comp, kkk,nstart=25, iter.max=1000)
    plot(comp, col=km$cluster, pch=16)
    plot3d(comp$PC1, comp$PC2, comp$PC3, col=km$cluster)
  })
  output$Plotly_KM1 <- renderPlot({
    kkk=input$k1
    set.seed(1)
    km=kmeans(comp, kkk,nstart=25, iter.max=1000)
    plot(comp, col=km$cluster, pch=16)
    autoplot(km, data = pcat, label = TRUE, label.size = 3)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

