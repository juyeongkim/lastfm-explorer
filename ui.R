
library(shiny)

shinyUI(
  navbarPage(
    "last.fm explorer", 
    tabPanel(
      "Search", 
      sidebarLayout(
        sidebarPanel(
          textInput("username", "Username", "platyjus"),
          submitButton("Search"), 
          p(), 
          p("GitHub: ",
            a(
              href = "https://github.com/juyeongkim/lastfm-explorer",
              "juyeongkim/lastfm-explorer"
            )
          )
        ), 
        mainPanel(
          verbatimTextOutput("distUser")
        )
      )
    ), 
    
    tabPanel(
      "Top", 
      plotOutput("distTopArtist"), 
      plotOutput("distTopAlbum"), 
      plotOutput("distTopTrack")
    ), 
    
    tabPanel(
      "Trends", 
      plotOutput("distHour"), 
      plotOutput("distWday")
    ), 
    
    tabPanel(
      "Data", 
      sidebarLayout(
        sidebarPanel(
          downloadButton("downloadData", "Download")
        ), 
        mainPanel(
          dataTableOutput("distTable")
        )
      )
    )
  )
)
