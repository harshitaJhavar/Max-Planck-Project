library(shiny)
library(ggplot2)
library(shinysky)
library(Cairo)
list_of_characters <- c('Harry Potter','Ronald Weasley','Hermione Granger','Lord Voldemort','Draco Malfoy','Dursleys')

fluidPage(
  
  div(style="display: inline-block;vertical-align:top; width: 900px;",titlePanel("EMOFIEL: Building a Knowledge Base of Emotions in Story Character Relationships ")),
  
  div(style="display: inline-block;vertical-align:top; width: 300px;"),
  
  div(style="display: inline-block;vertical-align:top; width: 300px;",selectInput('x', label = 'Choose the book', choices = c('Harry Potter and the Philosopher\'s Stone',
                                                                                                                              'Harry Potter and the Chamber of Secrets',
                                                                                                                              'Harry Potter and the Prisoner of Azkaban',
                                                                                                                              'Harry Potter and the Goblet of Fire',
                                                                                                                              'Harry Potter and the Order of the Phoenix',
                                                                                                                              'Harry Potter and the Half-Blood Prince',
                                                                                                                              'Harry Potter and the Deathly Hallows'),
                                                                                  selected = 'Harry Potter and the Philosopher\'s Stone')),

 
  div(style="display: inline-block;vertical-align:top; width: 1200px;"),
  
  div(style="display: inline-block;vertical-align:top; width: 200px;",select2Input(inputId = "user_char1input", label = "Enter name of character 1",choices=c(list_of_characters),type = c("input", "select"))),
 div(style="display: inline-block;vertical-align:top; width: 200px;",select2Input(inputId = "user_char2input",label = "Enter name of character 2",choices=c(list_of_characters),type = c("input", "select"))),
  
  actionButton("goButton0", "Analyze Emotions"),

  mainPanel(
    
    tabsetPanel(
      tabPanel("About",
               fluidRow(
                 div(style="display: inline-block;vertical-align:top; width: 1500px;",column(width = 12, class = "well",
                 htmlOutput("About"))))                                                                    
               ),
      tabPanel("Categorical Emotions (Char1->Char2)",
               #textOutput("Character_pairs"),
               
               fluidRow(
                 div(style="display: inline-block;vertical-align:top; width: 1500px;",column(width = 12, class = "well",
                                                                                             
                                                                                             plotOutput("Emotion Mapping1", height = 250,
                                                                                                        dblclick = "plot1_dblclick",
                                                                                                        click="plot_click1",
                                                                                                        brush = brushOpts(
                                                                                                          id = "plot1_brush",
                                                                                                          resetOnNew = TRUE
                                                                                                        )
                                                                                             ),
                                                                                             verbatimTextOutput("info1")
                 )),
                 div(style="display: inline-block;vertical-align:right; width: 800px;",
                   #  sidebarPanel(
                       textOutput("Heading for Scene Info"),
                       textOutput("Total Number of Scenes in the Story"),
                       textOutput("Selected Scene ID"),
                       textOutput("Selected Scene Description"),
                       textOutput("Selected Scene Agents and Actions"),
                       textOutput("Selected Scene Objects and Actions")
                     #  textOutput("Selected Scene Emotion Analysis")
                   ),
                 div(style="display: inline-block;vertical-align:right; width: 400px;",
                     
                         plotOutput("Dimensional Mapping"))
                       
                     )
                 ),
      tabPanel("Dimensional Emotions (Char1->Char2)", 
               fluidRow(
        div(style="display: inline-block;vertical-align:top; width: 1500px;",column(width = 12, class = "well",
                                                                                    
                                                                                    plotOutput("Emotion Mapping2", height = 250,
                                                                                               click="plot_click2",
                                                                                               dblclick = "plot2_dblclick",
                                                                                               brush = brushOpts(
                                                                                                 id = "plot2_brush",
                                                                                                 resetOnNew = TRUE
                                                                                               )
                                                                                    ),
                                                                                    verbatimTextOutput("info2")
        )),
        
        div(style="display: inline-block;vertical-align:right; width: 800px;",
            #  sidebarPanel(
            textOutput("Heading for Scene Info2"),
            textOutput("Total Number of Scenes in the Story2"),
            textOutput("Selected Scene ID2"),
            textOutput("Selected Scene Description2"),
            textOutput("Selected Scene Agents and Actions2"),
            textOutput("Selected Scene Objects and Actions2")
            #  textOutput("Selected Scene Emotion Analysis")
        ),
        div(style="display: inline-block;vertical-align:right; width: 400px;",
            
            plotOutput("Dimensional Mapping2")))
      ),
      tabPanel("Categorical Emotions (Char2->Char1)", 
               #textOutput("Character_pairs_reverse"),
               # h4,
               fluidRow(
                 div(style="display: inline-block;vertical-align:top; width: 1500px;",
                 column(width = 12, class = "well",
                        
                        plotOutput("Emotion Mapping3", height = 250,
                                   click="plot_click3",
                                   dblclick = "plot3_dblclick",
                                   brush = brushOpts(
                                     id = "plot3_brush",
                                     resetOnNew = TRUE
                                   )
                        ),
                        verbatimTextOutput("info3")
                 )),
                 div(style="display: inline-block;vertical-align:right; width: 800px;",
                     #  sidebarPanel(
                     textOutput("Heading for Scene Info3"),
                     textOutput("Total Number of Scenes in the Story3"),
                     textOutput("Selected Scene ID3"),
                     textOutput("Selected Scene Description3"),
                     textOutput("Selected Scene Agents and Actions3"),
                     textOutput("Selected Scene Objects and Actions3")
                     #  textOutput("Selected Scene Emotion Analysis")
                 ),
                 div(style="display: inline-block;vertical-align:right; width: 400px;",
                     
                     plotOutput("Dimensional Mapping3")))) ,
    
    tabPanel("Dimensional Emotions (Char2->Char1)", 
      fluidRow(
        div(style="display: inline-block;vertical-align:top; width: 1500px;",
        column(width = 12, class = "well",
         
               plotOutput("Emotion Mapping4", height = 250,
                          click="plot_click4",
                          dblclick = "plot4_dblclick",
                          brush = brushOpts(
                            id = "plot4_brush",
                            resetOnNew = TRUE
                          )
               ),
               verbatimTextOutput("info4")
               
        )
      ),
      div(style="display: inline-block;vertical-align:right; width: 800px;",
          #  sidebarPanel(
          textOutput("Heading for Scene Info4"),
          textOutput("Total Number of Scenes in the Story4"),
          textOutput("Selected Scene ID4"),
          textOutput("Selected Scene Description4"),
          textOutput("Selected Scene Agents and Actions4"),
          textOutput("Selected Scene Objects and Actions4")
          #  textOutput("Selected Scene Emotion Analysis")
      ),
      div(style="display: inline-block;vertical-align:right; width: 400px;",
          
          plotOutput("Dimensional Mapping4"))
    ))
)
)#,
#textOutput("Copyright")
)
