# Author: Ian C
# Date: Feb 12, 2017

# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

# Global functions used in the server part can be found in global.R file

# ===== Packages =====
library(shiny)
library(shinydashboard)

library(tidyverse) #
library(lubridate) # easier way to deal with dates
library(tm) # text mining
library(stringr) # easier way to deal with strings
library(highcharter) # interactive plots with Javascript library (Highcharts)
library(RColorBrewer) # palette color

# ===== Deprecated Packages =====
# library(scales) # change scales on plot
# library(wordcloud)

options(warn = -1) # disable warnings

source("global.R") # load global functions

# ===== Shiny Server =====
shinyServer(function(input, output, session) {
    
    # ===== React with action button =====
    reaction <- eventReactive(input$goButton, {
        
        # ===== Load chat file =====
        inFile <- input$file
        if (is.null(inFile)) return(NULL)
        chat <- read_lines(file = inFile$datapath)
        
        # ===== Bar progress with following tasks =====
        withProgress(message = 'Processing file', value = 0, {
            
            # ===== Clean chat from bad formatting messages =====
            incProgress(0.20, message = "Some data is being removed", detail = paste0(20, "%"))
            cleanChat <- removeBadOnes(chat, input$platform)
            
            # ===== Transform raw data into tidy data =====
            incProgress(0.30, message = "The messages are being processed", detail = paste0(50, "%"))
            dfChat <- createDataFrame(cleanChat$chat, input$platform, input$language)
            
            # ===== Clean and create corpus from text data =====
            incProgress(0.25, message = "Some magic here ;)", detail = paste0(75, "%"))
            dataCorpus <- makeCorpus(dfChat)
            
            # ===== Clean and create corpus from text data =====
            incProgress(0.25, message = "Almost ready!", detail = paste0(100, "%"))
            dataPlot <- createSummarizedData(dfChat)
            
            # ===== Extract members from data =====
            members <- unique(dfChat$user)
            
            # ===== Small or Large Group =====
            # Depending on the number of members, the plot color can be change
            # from discrete to continuous palette colors
            smallGroup <- length(members) < 8
            
            # ===== Create static plots =====
            # TODO: Create a reactive plot depending on the yTotals variable
            # summaryChat contains totalMessages, totalImages, totalAudios, 
            # totalVideos or totalGIFs
            summaryPlot <- makeBarPlot(dataPlot$summaryChat, 
                                       yTotals = "totalMessages", 
                                       xCategories = "user", 
                                       categories = members, 
                                       discrete = smallGroup,
                                       ordered = T)
            
            weekDayPlot <- makeBarPlot(dataPlot$weekDayChat, yTotals = "total",
                                       xCategories = "weekDay",
                                       categories = dataPlot$weekDayChat$weekDay)
            
            hourDayPlot <- makeBarPlot(dataPlot$hourDayChat, yTotals = "total",
                           xCategories = "hourDayChat",
                           categories = dataPlot$hourDayChat$hourDayChat, 
                           discrete = F)
            
        })
        
        # ===== Upload Select Input on UI =====
        updateSelectInput(session, "memberChoice", choices = members)
        
        # ===== Return reactive outputs =====
        list(check = cleanChat$check,
             originalLength = cleanChat$originalLength,
             dfChat = dfChat, 
             dailyChat = dataPlot$dailyChat, 
             dailyUserChat = dataPlot$dailyUserChat, 
             weekDayChat = dataPlot$weekDayChat, 
             hourDayChat = dataPlot$hourDayChat, 
             summaryChat = dataPlot$summaryChat,
             members = members,
             summaryPlot = summaryPlot,
             weekDayPlot = weekDayPlot, 
             hourDayPlot = hourDayPlot, 
             dataCorpus = dataCorpus
        )
    })
    
    # ===== Create a Top Emoji reactive table =====
    emojis <- reactive({
        
        user <- input$memberChoice
        corpus <- reaction()$dataCorpus
        
        # TODO: Create a reactive input in which one can change
        # the pattern to find (other than emojis). Can be specific words
        # or phrases, name users of whatever.
        dfEmojis <- str_locate_all(pattern = '[\U0001F0CF-\U0001F9C0]', 
                                   string = as.character(corpus[[user]]$content))[[1]]
        
        emojis <- c()
        for (i in 1:nrow(dfEmojis)) emojis <- c(emojis, substr(corpus[[user]]$content, 
                                                               dfEmojis[i, 1], 
                                                               dfEmojis[i, 2]))
        
        # TODO: Create a reactive input option to change the number of elements 
        # displayed 
        topEmoji <- data_frame(Emoji = names(sort(table(emojis), decreasing = T)[1:3]), 
                               Total = sort(table(emojis), decreasing = T)[1:3])
        
        list(topEmoji = topEmoji)
        
    })
    
    # TODO: Make wordcloud with the most common word in the entire chat
    # TODO: Make and intectative word cloud. Like this: https://github.com/adymimos/rWordCloud
    # TODO: Depending on the member, generate a word cloud with his/her messages.
    
    # ===== Create a Top Emoji reactive table =====
    output$topEmoji <- renderTable(emojis()$topEmoji)
    
    # ===== Create a table to show a sample from chat =====
    output$content <- renderTable(reaction()$dfChat[sample(1:nrow(reaction()$dfChat), 10), ])
    
    # ===== Generate warning message =====
    output$warning <- renderPrint({
        
        paste(round(100*length(reaction()$check)/reaction()$originalLength, 2),
              "% of messages were deleted because they weren't in the correct format",
              sep = "")
        
    })
    
    # ===== Generate value boxes =====
    output$members <- renderValueBox({
        valueBox(
            length(reaction()$members),
            "Members",
            icon = icon("users")
        )
    })
    output$messages <- renderValueBox({
        valueBox(
            nrow(reaction()$dfChat),
            "Messages",
            icon = icon("whatsapp"), 
            color = 'green'
        )
    })
    output$moreActive <- renderValueBox({
        
        dfChat <- reaction()$dfChat
        mostActive <- names(sort(table(dfChat$user), decreasing = T))[1]
        mostActive <- strsplit(mostActive, " ")[[1]][1]
        
        valueBox(
            mostActive, # 
            "Most Active Member",
            icon = icon("star"), 
            color = 'red'
        )
    })
    
    # ===== Generate interactive plots =====
    output$summaryPlot <- renderHighchart(reaction()$summaryPlot)
    output$weekDayPlot <- renderHighchart(reaction()$weekDayPlot)
    output$hourDayPlot <- renderHighchart(reaction()$hourDayPlot)
    
    # ===== Generate static ggplot2 plots =====
    output$dailyUserPlot <- renderPlot({
        
        ggplot(reaction()$dailyUserChat, aes(x = dateTime, y = total, colour = user)) +
            stat_smooth(se = F) + 
            geom_point() +
            labs(x = "Fecha", y = "Mensajes") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                  panel.background = element_blank(), 
                  legend.position=c(1,1),
                  legend.justification=c(1,1))
        
    })
    
    # ===== Generate Data Tables =====
    output$dailyChat <- renderDataTable(reaction()$dailyChat)
    output$dailyUserChat <- renderDataTable(reaction()$dailyUserChat)
    output$weekDayChat <- renderDataTable(reaction()$weekDayChat)
    output$hourDayChat <- renderDataTable(reaction()$hourDayChat)
    output$summaryChat <- renderDataTable(reaction()$summaryChat)
    
})
