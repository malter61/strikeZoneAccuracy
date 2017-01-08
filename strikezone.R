
library(pitchRx)
library(RSQLite)
library(dplyr)
library(data.table)
library(splitstackshape)
library(reshape2)
library(data.table)


masterList <- list()
atbat <- list()
pitch <- list()
action <- list()
runner <- list()
### If we download too many files, we get a runtime error, so I download only
### one week at a time.
### For each week, I'm rbinding the new tables all in one step
offseason <- FALSE
a <- as.Date('2015-04-05')
i <- 1
while(a < as.Date('2015-04-25')) {
  b <- a + 7
  masterList <- scrape(start = a, end = b)
  if(i==1){
    atbat <- masterList[['atbat']][,c('pitcher','batter','num','inning',
                                      'inning_side','stand','p_throws',
                                  'event','event_num','home_team_runs',
                                  'away_team_runs','batter_name','pitcher_name',
                                  'gameday_link','score','date','b','s','o')]
    pitch <- masterList[['pitch']]#[,c('des','type','event_num','start_speed',
                                  #'break_y','break_angle','pitch_type',
                              #'spin_rate','inning_side','inning','gameday_link',
                              #num','count','on_1b','on_2b','on_3b','px','pz')]
    action <- masterList[['action']][,c('des','event','player','num',
                                        'gameday_link')]
    runner <- masterList[['runner']][,c('id','start','end','event','num',
                                        'gameday_link','score','rbi','earned')]
  }else{
    atbat <- rbind(atbat,masterList[['atbat']][,c('pitcher','batter','num',
                                      'inning','inning_side','stand','p_throws',
                                      'event','event_num','home_team_runs',
                                      'away_team_runs','batter_name',
                                      'pitcher_name','gameday_link','score',
                                      'date','b','s','o')])
    pitch <- rbind(pitch,masterList[['pitch']])#[,c('des','type','event_num',
                            #'start_speed','break_y','break_angle','pitch_type',
                                          #'spin_rate','inning_side','inning',
                                          #gameday_link','num','count','on_1b',
                                          #'on_2b','on_3b')])
    action <- rbind(action,masterList[['action']][,c('des','event','player',
                                                     'num','gameday_link')])
    runner <- rbind(runner,masterList[['runner']][,c('id','start','end','event',
                                'num','gameday_link','score','rbi','earned')])
  }
  a <- a + 7
  i <- i+1
   #let's skip over the offseason dates
     if(a > '2015-11-02' & !offseason){
       a <- as.Date('2016-04-03')
       offseason <- TRUE
     }
}

pitch <- data.table(pitch)
atbat <- data.table(atbat)
action <- data.table(action)
runner <- data.table(runner)

### change the file paths to save to your own folder.
write.csv(pitch,'C:/baseball/pitch_2015_2016.csv',row.names=F)
write.csv(atbat,'C:/baseball/atbat_2015_2016.csv',row.names=F)
write.csv(action,'C:/baseball/action_2015_2016.csv',row.names=F)
write.csv(runner,'C:/baseball/runner_2015_2016.csv',row.names=F)


atbat$gameday_link_num <- paste(atbat$gameday_link,atbat$num)
pitch.event <- pitch[,c('des','sz_top','sz_bot','px','pz','zone','count',
                        'gameday_link','inning_side','inning','num'),with=F]
pitch.event$gameday_link_num <- paste(pitch.event$gameday_link,pitch.event$num)

pitch.event <- merge(pitch.event,atbat,by='gameday_link_num')
# pitch.event <- pitch.event[,c('gameday_link_num','des','type','start_speed',
#                               'end_speed','sz_top','sz_bot','px','pz','break_y',
#                               'break_angle','break_length','pitch_type',
#                               'spin_dir','spin_rate','inning_side.x','inning.x',
#                               'num.x','count','on_1b','on_2b','on_3b','pitcher',
#                               'batter','stand','p_throws','event',
#                               'home_team_runs','away_team_runs','batter_name',
#                               'pitcher_name','score','date'),with=F]
names(pitch.event) <- gsub('\\.x','',names(pitch.event))

ballCircum <- 9.25/12
ballRadius <- ballCircum/(2*pi)
minx <- -8.5/12-ballRadius
maxx <- 8.5/12+ballRadius

pitch.event$actual <- ifelse(pitch.event$px>minx & pitch.event$px<maxx & 
                               pitch.event$pz<pitch.event$sz_top & 
                               pitch.event$pz>pitch.event$sz_bot,'strike','ball')
gid <- data.frame(do.call('rbind', 
            strsplit(as.character(pitch.event$gameday_link),'_',
                     fixed=TRUE)))[,2:6]
colnames(gid) <- c('year','month','day','vis','home')
pitch.event <- cbind(pitch.event,gid)
gid <- NULL

pitch.event$year <- as.numeric(as.character(pitch.event$year))
pitch.event$month <- as.numeric(as.character(pitch.event$month))
pitch.event$day <- as.numeric(as.character(pitch.event$day))
pitch.event$vis <- as.character(pitch.event$vis)
pitch.event$home <- as.character(pitch.event$home)
pitch.event$vis <- substr(pitch.event$vis,1,3)
pitch.event$home <- substr(pitch.event$home,1,3)
pitch.event$month <- ifelse(pitch.event$month==3,'March',
                      ifelse(pitch.event$month==4,'April',
                      ifelse(pitch.event$month==5,'May',
                      ifelse(pitch.event$month==6,'June',
                      ifelse(pitch.event$month==7,'July',
                      ifelse(pitch.event$month==8,'August',
                      ifelse(pitch.event$month==9,'September',
                      ifelse(pitch.event$month==10,'October','November'))))))))
pitch.event <- pitch.event[!pitch.event$vis %in% c('aas','nas') & 
                             !pitch.event$home %in% c('aas','nas'),]

pitch.event$vis <- ifelse(pitch.event$vis=='ana','Angels',
                    ifelse(pitch.event$vis=='ari','Diamondbacks',
                    ifelse(pitch.event$vis=='atl','Braves',
                    ifelse(pitch.event$vis=='bal','Orioles',
                    ifelse(pitch.event$vis=='bos','Red Sox',
                    ifelse(pitch.event$vis=='cha','White Sox',
                    ifelse(pitch.event$vis=='cin','Reds',
                    ifelse(pitch.event$vis=='cle','Indians',
                    ifelse(pitch.event$vis=='col','Rockies',
                    ifelse(pitch.event$vis=='det','Tigers',
                    ifelse(pitch.event$vis=='hou','Astros',
                    ifelse(pitch.event$vis=='kca','Royals',
                    ifelse(pitch.event$vis=='lan','Dodgers',
                    ifelse(pitch.event$vis=='mia','Marlins',
                    ifelse(pitch.event$vis=='mil','Brewers',
                    ifelse(pitch.event$vis=='min','Twins',
                    ifelse(pitch.event$vis=='nya','Yankees',
                    ifelse(pitch.event$vis=='nyn','Mets',
                    ifelse(pitch.event$vis=='oak','Athletics',
                    ifelse(pitch.event$vis=='phi','Phillies',
                    ifelse(pitch.event$vis=='pit','Pirates',
                    ifelse(pitch.event$vis=='sdn','Padres',
                    ifelse(pitch.event$vis=='sea','Mariners',
                    ifelse(pitch.event$vis=='sfn','Giants',
                    ifelse(pitch.event$vis=='sln','Cardinals',
                    ifelse(pitch.event$vis=='tba','Rays',
                    ifelse(pitch.event$vis=='tex','Rangers',
                    ifelse(pitch.event$vis=='tor','Blue Jays',
                    ifelse(pitch.event$vis=='chn','Cubs','Nationals')
                                          ))))))))))))))))))))))))))))

pitch.event$home <- ifelse(pitch.event$home=='ana','Angels',
                    ifelse(pitch.event$home=='ari','Diamondbacks',
                    ifelse(pitch.event$home=='atl','Braves',
                    ifelse(pitch.event$home=='bal','Orioles',
                    ifelse(pitch.event$home=='bos','Red Sox',
                    ifelse(pitch.event$home=='cha','White Sox',
                    ifelse(pitch.event$home=='cin','Reds',
                    ifelse(pitch.event$home=='cle','Indians',
                    ifelse(pitch.event$home=='col','Rockies',
                    ifelse(pitch.event$home=='det','Tigers',
                    ifelse(pitch.event$home=='hou','Astros',
                    ifelse(pitch.event$home=='kca','Royals',
                    ifelse(pitch.event$home=='lan','Dodgers',
                    ifelse(pitch.event$home=='mia','Marlins',
                    ifelse(pitch.event$home=='mil','Brewers',
                    ifelse(pitch.event$home=='min','Twins',
                    ifelse(pitch.event$home=='nya','Yankees',
                    ifelse(pitch.event$home=='nyn','Mets',
                    ifelse(pitch.event$home=='oak','Athletics',
                    ifelse(pitch.event$home=='phi','Phillies',
                    ifelse(pitch.event$home=='pit','Pirates',
                    ifelse(pitch.event$home=='sdn','Padres',
                    ifelse(pitch.event$home=='sea','Mariners',
                    ifelse(pitch.event$home=='sfn','Giants',
                    ifelse(pitch.event$home=='sln','Cardinals',
                    ifelse(pitch.event$home=='tba','Rays',
                    ifelse(pitch.event$home=='tex','Rangers',
                    ifelse(pitch.event$home=='tor','Blue Jays',
                    ifelse(pitch.event$home=='chn','Cubs','Nationals')
                                              ))))))))))))))))))))))))))))
pitch.event <- pitch.event[order(pitch.event$year,pitch.event$month,
                          pitch.event$date,pitch.event$vis,pitch.event$inning)]
pitch.event$actual <- as.factor(pitch.event$actual)
pitch.event$des <- as.factor(pitch.event$des)
pitch.event$accuracy <- ifelse(pitch.event$actual=='strike' & 
                          pitch.event$des =='Called Strike',
                          'Good called strike',
                          ifelse(pitch.event$actual=='strike' & 
                                pitch.event$des=='Ball','Bad called ball',
                          ifelse(pitch.event$actual=='ball' & 
                                pitch.event$des=='Ball','Good called ball',
                                'Bad called strike')))


pitch.event$accuracy <- as.factor(pitch.event$accuracy)
pitch.event <- pitch.event[pitch.event$pz>0]

sale <- pitch.event[pitch.event$pitcher_name=='Chris Sale']
pitch.plot <- ggplot(sale, aes(x = px, y = pz,color=accuracy)) + geom_point()
pitch.plot <- pitch.plot + geom_segment(aes(x=minx,y=mean(sz_top),
                                            xend=maxx,yend=mean(sz_top)))
pitch.plot <- pitch.plot + geom_segment(aes(x=minx,
                                y=mean(sz_top)-((mean(sz_top)-mean(sz_bot))/3),
                            xend=maxx,
                            yend=mean(sz_top)-((mean(sz_top)-mean(sz_bot))/3)))
pitch.plot <- pitch.plot + geom_segment(aes(x=minx,y=mean(sz_top),
                                            xend=maxx,yend=mean(sz_top)))
pitch.plot <- pitch.plot + geom_segment(aes(x=minx,
                            y=mean(sz_top)-((mean(sz_top)-mean(sz_bot))*2/3),
                            xend=maxx,
                           yend=mean(sz_top)-((mean(sz_top)-mean(sz_bot))*2/3)))
pitch.plot <- pitch.plot + geom_segment(aes(x=minx,y=mean(sz_bot),
                                  xend=maxx,yend=mean(sz_bot)))
pitch.plot <- pitch.plot + geom_segment(aes(x=minx,y=mean(sz_bot),
                                    xend=minx,yend=mean(sz_top)))
pitch.plot <- pitch.plot + geom_segment(aes(x=maxx,y=mean(sz_bot),
                                    xend=maxx,yend=mean(sz_top)))
pitch.plot <- pitch.plot + geom_segment(aes(x=minx/3,y=mean(sz_bot),
                                    xend=minx/3,yend=mean(sz_top)))
pitch.plot <- pitch.plot + geom_segment(aes(x=maxx/3,y=mean(sz_bot),
                                    xend=maxx/3,yend=mean(sz_top)))
pitch.plot <- pitch.plot + scale_y_continuous(limits = c(0, 4.5)) + 
                            scale_x_continuous(limits = c(-2, 2))
pitch.plot <- pitch.plot + xlab("catcher's view") + ylab("height (feet)") + 
            ggtitle("Home Plate Umpire Accuracy\n for Indians Pitchers Game 7")
pitch.plot                             

dim(pitch.event)

write.csv(pitch.event,'C:/baseball/strikeZone2016.v2.csv',row.names=F)


