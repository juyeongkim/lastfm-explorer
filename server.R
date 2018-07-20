library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)
library(lastfmr)

shinyServer(function(input, output) {
  
  tracks <- reactive({
    username <- input$username
    
    n <- attr(user_getRecentTracks(username), "totalPages")
    recents <-map2_df(username, seq_len(n), function(user, i) {
      user_getRecentTracks(user, page = i)
    })
    
    recents$date <- recents$`date.#text`
    recents$artist <- recents$`artist.#text`
    recents$track <- recents$name
    recents$album <- recents$`album.#text`
    
    recents <- subset(
      recents, 
      subset = !is.na(date), 
      select = c(date, track, artist, album)
    )
    
    recents
  })
  
  output$distUser <- renderPrint({
    as.list(user_getInfo(input$username))
  })
  
  output$distTopAlbum <- renderPlot({
    tracks() %>% 
      count(album) %>% 
      top_n(5, n) %>% 
      ggplot(aes(x = reorder(album, n), y = n)) + 
      geom_bar(stat = "identity", fill = "skyblue3") + 
      geom_label(aes(label = n)) + 
      coord_flip() + 
      labs(title = "Top 5 Albums", x = "Album Title", y = "Count")
  })
  
  output$distTopArtist <- renderPlot({
    tracks() %>% 
      count(artist) %>% 
      top_n(5, n) %>% 
      ggplot(aes(x = reorder(artist, n), y = n)) + 
      geom_bar(stat = "identity", fill = "skyblue3") + 
      geom_label(aes(label = n)) + 
      coord_flip() + 
      labs(title = "Top 5 Artists", x = "Artists", y = "Count")
  })
  
  output$distTopTrack <- renderPlot({
    tracks() %>% 
      count(track) %>% 
      top_n(5, n) %>% 
      ggplot(aes(x = reorder(track, n), y = n)) + 
      geom_bar(stat = "identity", fill = "skyblue3") + 
      geom_label(aes(label = n)) + 
      coord_flip() + 
      labs(title = "Top 5 Tracks", x = "Tracks", y = "Count")
  })
  
  output$distHour <- renderPlot({
    tracks() %>% 
      mutate(hour = hour(dmy_hm(date))) %>% 
      count(hour) %>%
      ggplot(aes(x = hour, y = n)) + 
      geom_bar(stat = "identity", fill = "skyblue3") + 
      geom_label(aes(label = n)) + 
      labs(x = "Hour", y = "Count")
  })
  
  output$distWday <- renderPlot({
    tracks() %>% 
      mutate(wday = wday(dmy_hm(date), label = T)) %>% 
      count(wday) %>%
      ggplot(aes(x = wday, y = n)) + 
      geom_bar(stat = "identity", fill = "skyblue3") + 
      geom_label(aes(label = n)) + 
      labs(x = "Weekday", y = "Count")
  })
  
  output$distTable <- renderDataTable({tracks()})
  
  output$downloadData <- downloadHandler(
    filename = paste0(input$username, ".csv"), 
    content = function(file) {
      write.csv(tracks(), file)
    }
  )
})
