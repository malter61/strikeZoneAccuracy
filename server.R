library(shiny)
require(devtools)
library(data.table)
library(rdrop2)
library(ggplot2)
library(repmis)
#library(plotly)

 #file <- 'strikeZone2016.ws.csv'
 #mykey <- '3xskeg72ignj2my'
  #sz <- source_DropboxData(file,key=mykey, sep=",", header=TRUE)
sz <- read.csv('C:/baseball/strikeZone2016.v3.csv',stringsAsFactors=F)
sz <- data.table(sz)
sz$count <- as.character(sz$count)
print(str(sz))
sz$balls <- substr(sz$count,1,1)
sz$strikes <- substr(sz$count,3,3)
#  sz <- read.csv('C:/baseball/strikeZone2016.ws.csv',stringsAsFactors=F)
#  sz <- data.table(sz)
#https://www.dropbox.com/s/3xskeg72ignj2my/strikeZone2016.ws.csv?dl=0

print(tail(sz))
#prin(str(sz))

ballCircum <- 9.25/12
ballRadius <- ballCircum/(2*pi)
xmin <- as.numeric(-8.5/12-ballRadius)
xmax <- as.numeric(8.5/12+ballRadius)
zone <- data.frame(matrix(0,2,4))
colnames(zone) <- c('Actual','CalledStrike','CalledBall','Accuracy')
zone$Actual<- c('Strike','Ball')
shinyServer(
  function(input,output){
    
    
    output$umpire.accuracy <- renderTable({
      
      if(input$viewType=='team' & input$team=='All' & input$month=='All'){
        df <- sz
      }
      
      if(input$viewType=='team' & input$team=='All' & input$month !='All' & input$day %in% c('','0')){
        df <- sz[sz$month==input$month]
      }
      
      if(input$viewType=='team' & input$team=='All' & input$month !='All' & !input$day %in% c('','0')){
        df <- sz[sz$month==input$month & sz$date==input$day]
      }
      
      if(input$viewType=='team' & input$team!='All' & input$month=='All'){
        df <- sz[((sz$vis==input$team & sz$inning_side=='bottom')  | (sz$home==input$team & sz$inning_side=='top'))]
      }
      
      if(input$viewType=='team' & input$team!='All' & input$month!='All' & input$day %in% c('','0')){
        df <- sz[((sz$vis==input$team & sz$inning_side=='bottom')  | (sz$home==input$team & sz$inning_side=='top')) & 
                   sz$month==input$month]
      }
      
      if(input$viewType=='team' & input$team!='All' & input$month!='All' & !input$day %in% c('','0')){
        df <- sz[((sz$vis==input$team & sz$inning_side=='bottom')  | (sz$home==input$team & sz$inning_side=='top'))
                 & sz$month==input$month & sz$date==input$day]
      }
      
      if(input$viewType=='pitcher' & input$month=='All'){
        df <- sz[tolower(sz$pitcher_name)==tolower(input$pitcher)]
      }
      
      if(input$viewType=='pitcher' & input$month!='All' & input$day %in% c('','0')){
        df <- sz[tolower(sz$pitcher_name)==tolower(input$pitcher) & sz$month==input$month]
      }
      
      if(input$viewType=='pitcher' & input$month!='All' & !input$day %in% c('','0')){
        df <- sz[tolower(sz$pitcher_name)==tolower(input$pitcher) & sz$month==input$month & sz$date==input$day]
      }
      if(input$balls==0){df <- df[df$balls==0,]}
      if(input$balls==1){df <- df[df$balls==1,]}
      if(input$balls==2){df <- df[df$balls==2,]}
      if(input$balls==3){df <- df[df$balls==3,]}
      if(input$strikes==0){df <- df[df$strikes==0,]}
      if(input$strikes==1){df <- df[df$strikes==1,]}
      if(input$strikes==2){df <- df[df$strikes==2,]}
      
      zone$CalledStrike <- c(nrow(df[df$actual=='strike' & df$des=='Called Strike']),
                             nrow(df[df$actual=='ball' & df$des=='Called Strike']))
      zone$CalledBall <- c(nrow(df[df$actual=='strike' & df$des=='Ball']),
                           nrow(df[df$actual=='ball' & df$des=='Ball']))
      zone$Accuracy <- c(zone$CalledStrike[1]/(zone$CalledStrike[1]+zone$CalledBall[1]),
                         zone$CalledBall[2]/(zone$CalledStrike[2]+zone$CalledBall[2]))
      zone
      
    })
    
    
    output$pitchPlot <- renderPlot({
      
      if(input$viewType=='team' & input$team=='All' & input$month=='All'){
        df <- sz[sz$month %in% c('September','October')]
      }
      
      if(input$viewType=='team' & input$team=='All' & input$month !='All' & input$day %in% c('','0')){
        df <- sz[sz$month==input$month]
      }
      
      if(input$viewType=='team' & input$team=='All' & input$month !='All' & !input$day %in% c('','0')){
        df <- sz[sz$month==input$month & sz$date==input$day]
      }
      
      if(input$viewType=='team' & input$team!='All' & input$month=='All'){
        df <- sz[((sz$vis==input$team & sz$inning_side=='bottom')  | (sz$home==input$team & sz$inning_side=='top'))]
      }
      
      if(input$viewType=='team' & input$team!='All' & input$month!='All' & input$day %in% c('','0')){
        df <- sz[((sz$vis==input$team & sz$inning_side=='bottom')  | (sz$home==input$team & sz$inning_side=='top')) & 
                   sz$month==input$month]
      }
      
      if(input$viewType=='team' & input$team!='All' & input$month!='All' & !input$day %in% c('','0')){
        df <- sz[((sz$vis==input$team & sz$inning_side=='bottom')  | (sz$home==input$team & sz$inning_side=='top'))
                 & sz$month==input$month & sz$date==input$day]
      }
      
      if(input$viewType=='pitcher' & input$month=='All'){
        df <- sz[tolower(sz$pitcher_name)==tolower(input$pitcher)]
      }
      
      if(input$viewType=='pitcher' & input$month!='All' & input$day %in% c('','0')){
        df <- sz[tolower(sz$pitcher_name)==tolower(input$pitcher) & sz$month==input$month]
      }
      
      if(input$viewType=='pitcher' & input$month!='All' & !input$day %in% c('','0')){
        df <- sz[tolower(sz$pitcher_name)==tolower(input$pitcher) & sz$month==input$month & sz$date==input$day]
      }
      
      if(input$balls==0){df <- df[df$balls==0,]}
      if(input$balls==1){df <- df[df$balls==1,]}
      if(input$balls==2){df <- df[df$balls==2,]}
      if(input$balls==3){df <- df[df$balls==3,]}
      if(input$strikes==0){df <- df[df$strikes==0,]}
      if(input$strikes==1){df <- df[df$strikes==1,]}
      if(input$strikes==2){df <- df[df$strikes==2,]}
      
      #df$accuracy <- ifelse(df$accuracy==1,'Good called strike',ifelse(df$accuracy==2,'Bad called ball',
       #                                                                ifelse(df$accuracy==3,'Good called ball','Bad called strike')))
      pp <- ggplot(df, aes(x = px, y = pz,color=accuracy)) + geom_point()
      pp <- pp + geom_segment(aes(x=-0.8310153,y=mean(sz_top),xend=0.8310153,yend=mean(sz_top)))
      pp <- pp + geom_segment(aes(x=-0.8310153,y=mean(sz_top)-((mean(sz_top)-mean(sz_bot))/3),
                                  xend=0.8310153,yend=mean(sz_top)-((mean(sz_top)-mean(sz_bot))/3)))
      pp <- pp + geom_segment(aes(x=-0.8310153,y=mean(sz_top),xend=0.8310153,yend=mean(sz_top)))
      pp <- pp + geom_segment(aes(x=-0.8310153,y=mean(sz_top)-((mean(sz_top)-mean(sz_bot))*2/3),
                                  xend=0.8310153,yend=mean(sz_top)-((mean(sz_top)-mean(sz_bot))*2/3)))
      pp <- pp + geom_segment(aes(x=-0.8310153,y=mean(sz_bot),xend=0.8310153,yend=mean(sz_bot)))
      pp <- pp + geom_segment(aes(x=-0.8310153,y=mean(sz_bot),xend=-0.8310153,yend=mean(sz_top)))
      pp <- pp + geom_segment(aes(x=0.8310153,y=mean(sz_bot),xend=0.8310153,yend=mean(sz_top)))
      pp <- pp + geom_segment(aes(x=-0.8310153/3,y=mean(sz_bot),xend=-0.8310153/3,yend=mean(sz_top)))
      pp <- pp + geom_segment(aes(x=0.8310153/3,y=mean(sz_bot),xend=0.8310153/3,yend=mean(sz_top)))
      pp <- pp + scale_y_continuous(limits = c(0, 4.5)) + scale_x_continuous(limits = c(-2, 2))
      pp <- pp + xlab("catcher's view") + ylab("height (feet)") + ggtitle("Home Plate Umpire Accuracy")
      pp    
      
      
    })
     
})
