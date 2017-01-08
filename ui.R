library(shiny)


shinyUI(pageWithSidebar(
  
  headerPanel(h5("Pitch Tracker Umpire Accuracy, Mark Malter")),
  
  #sidebarLayout(
    sidebarPanel(   
       radioButtons('viewType','View one pitcher or an entire team?',c('pitcher','team'),inline=TRUE),
       br(),
       selectInput('team','Team:',c('All','Angels','Astros','Athletics','Blue Jays','Braves','Brewers','Cardinals','Cubs','Diamondbacks',
                                    'Dodgers','Giants','Indians','Mariners','Marlins','Mets','Nationals','Orioles','Padres',
                                    'Phillies', 'Pirates','Rangers', 'Rays','Red Sox','Reds','Rockies','Royals','Tigers','Twins',
                                    'White Sox','Yankees'),selectize=F,multiple=F),
       br(),
      textInput("pitcher","Pitcher (first and last name):",value="Kyle Hendricks"),
      br(),
      radioButtons('balls','Balls:',c('All','0','1','2','3'),inline=TRUE),
             br(),
      radioButtons('strikes','Strikes:',c('All','0','1','2'),inline=TRUE),
      br(),
      selectInput('score.diff','Score Differential:',c('All','Batting team down by 3 or more runs','Batting team down by 2 runs',
                                                       'Batting team down by 1 run', 'tied', 'Batting team up by 1 run',
                                                       'Batting team up by 2 runs', 'Batting team up by 3 or more runs'),selectize=F,multiple=F),
      br(),
#       radioButtons('stand','Batter Side:',c('Both','L','R'),inline=TRUE),
#       br(),
      selectInput("month","Month:",c('All','April','May','June','July','August','September','October','November'),selectize=F,multiple=F),
      br(),
      textInput("day","Day of Month (0 for entire month):",value="0"),
      br()

         
    ),

  
    
    mainPanel( 
      tabsetPanel(type="tab",
      tabPanel("Introduction",HTML("<div> 
                                   <br></br> Throughout the 2016 MLB playoffs, there has been a great deal of 
                                            discussion on the accuracy of ball/strike calls by home plate umpires.
                                            With this app, you can view for yourself the accuracy based on the 
                                            entire league, a particular team, or an individual pitcher.
                                            Regardless of which group you choose, you can also select your data to 
                                            contain the entire 2016 season, a given month, or even a game date.
                                            <br></br>

                                            You can then choose to view either a <b></b>Table View<b></b> of the accuracy, 
                                            or a Plot View.
                                            Note: If you choose the plot view, the top and bottom of the strike zone is
                                            determined by the height of the average batter.  The table view is therefore
                                            slightly more accurate, since it accounts for the actual batter for every 
                                            plate appearance.
                                            <br></br>
                                            Note: The pitches shown are only those pitches where the umpire had to make a decision.  
                                            I excluded all pitches swung at, whether missed, hit foul, or put in play; and I also excluded
                                            pitches in the dirt, pitch outs, and intentional walks, since there is not really any 
                                            decision for the umpire to make on those pitches.
                                            <br></br>
                                            Example 1: to view game four of the world series, select 'team', 'Cubs' or
                                            'Indians', 'October', and type '29' in the 'Day of Month' box.  Or you can 
                                             select 'pitcher', type in either 'Corey Kluber', 'John Lackey',
                                            or the name of any relief pitcher who appeared in that game. 
                                            <br></br>
                                            Example 2: To see how the Cubs have feared throughout the playoffs, select
                                            'team', 'Cubs', and 'October'.
                                            <br></br>
                                            Have fun. Now go ahead and click either 'Table View' or 'Plot View'.
                                            <br></br>
            
                          Mark
                          </div>")),
                  tabPanel('Table View',
      tableOutput('umpire.accuracy')),
                  tabPanel('Plot View',
      plotOutput('pitchPlot')))
                           
                  
      )
    )
)