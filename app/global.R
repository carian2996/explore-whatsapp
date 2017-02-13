# Author: Ian C
# Date: Feb 12, 2017

# Global functions made in Explore WhatsApp application. 

removeBadOnes <- function(chat, platform) {
    
    # Removes messages that don't have the correct format.
    # In general the format for chat message in WhatsApp is the following:
    
    # Datetime User Message
    
    # INPUT
    # chat (char) = Chat data readed from file
    # platform (char) = Depending on the platform, chat data may vary
    
    # OUTPUT
    # check (int) = Position that lines with bad format 
    # originalLength (int) = Number of messages before message deletion
    # chat (char) = Messages having the a correct format
    
    if (platform == 'iPhone'){
        
        # The possible message format to iPhone is:
        # Spanish - dd/mm/YYYY hh:mm:ss: User: Message! 
        # English - mm/dd/YYYY hh:mm:ss: User: Message!
        
        # TODO: Looking for more messages formats.
        # TODO: Develop a way to users can donate a small sample of this chat
        
        check <- c()
        originalLength <- length(chat)
        
        for (i in 1:length(chat)) {
            
            # The message should contains the following regular expression
            date_val <- grepl('^([0-9]+)/([0-9]+)/([0-9]+)', chat[i])
            
            # The message should be divided into two parts: Datetime and user/message
            iphone <- length(strsplit(chat[i], split = ":([0-9]+): ", perl = T)[[1]]) == 2
            
            # If both conditions are fulfilled, the message have the correct format, 
            # otherwise, save the index from message
            if (date_val == F | iphone == F) check <- c(check, i)
            
        }
        
        # Remove bad messages
        if (!is.null(check)) chat <- chat[-check]
        
    } else if (platform == 'Android') {
        
        # The possible message format to Android platform are:
        # Spanish (English: date format mm/dd/YY)
        # dd/mm/YY hh:mm AM - User: Message! 
        # dd/mm/YY hh:mm p.m. - User: Message! 
        
        # TODO: Looking for more messages formats.
        
        check <- c()
        originalLength <- length(chat)
        
        for (i in 1:length(chat)) {
            
            dtChat <- strsplit(chat[i], split = " - ", perl = T)[[1]][1]
            date <- strsplit(dtChat, split = ", ")[[1]][1]
            time <- strsplit(dtChat, split = ", ")[[1]][2]
            
            date_val <- grepl('([0-9]+)/([0-9]+)/([0-9]+)', date)
            time_val <- grepl('([0-9]+):([0-9]+)', time)
            
            datetime_val <- date_val & time_val
            
            userMessage <- strsplit(chat[i], split = " - ", perl = T)[[1]][2]
            android <- length(strsplit(userMessage, split = ": ", perl = T)[[1]]) == 2
            
            if (datetime_val == F | android == F) check <- c(check, i)
        }
        
        if (!is.null(check)) chat <- chat[-check]
        
    }
    
    list(check = check, 
         originalLength = originalLength, 
         chat = chat)
    
}

createDataFrame <- function(cleanChat, platform, language) {
    
    # Transforms and creates a data frame to store the chat data.
    # Depending on the platform origen, data may vary on format
    
    # INPUT
    # cleanChat (char) = Clean chat without any bad format message
    # platform (char) = Depending on the platform, chat data may vary
    # language (char) = Depending on the language, date formart my vary
    
    # OUTPUT
    # dfChat (data.frame) = Data frame with tidy data trasformed
    
    if (platform == "iPhone") {
        
        # Depending on the language, the function uses a different date treatment
        if (language == "Spanish") convertDate <- dmy_hms
        if (language == "English") convertDate <- mdy_hms
        
        # Create the data frame with and index and the original message
        dfChat = data_frame(idMessage = 1:length(cleanChat), originalChat = cleanChat)
        
        # Uses dplyr to extract information from original chat
        dfChat <- dfChat %>% 
            rowwise() %>% # Realize mutation processes row by row
            mutate(dateTimeChat = convertDate(strsplit(originalChat,
                                                   split = ": ",
                                                   perl = T)[[1]][1]), 
                   userMessage = strsplit(originalChat, 
                                          split = ":([0-9]+): ", 
                                          perl = T)[[1]][2], 
                   breakString = as.numeric(regexpr(pattern = ": ", 
                                                    userMessage, 
                                                    perl = T))) %>%
            filter(breakString > 0) %>%
            mutate(user = substr(userMessage, 1, breakString - 1), 
                   message = substr(userMessage, breakString + 2, 9999)) %>%
            # Only select id, datetime, user and message
            select(idMessage, dateTimeChat, user, message)
        
        # Remove any incomplete row (missing information)
        dfChat <- dfChat[complete.cases(dfChat),]
        
    } else if (platform == "Android"){
        
        if (language == "Spanish") convertDate <- dmy_hm
        if (language == "English") convertDate <- mdy_hm
        
        dfChat <- data_frame(idMessage = 1:length(cleanChat), originalChat = cleanChat)
        
        dfChat <- dfChat %>% 
            rowwise() %>% 
            mutate(dateTime = strsplit(originalChat, split = " - ", perl = T)[[1]][1],
                   userMessage = strsplit(originalChat, split = " - ", perl = T)[[1]][2]) %>%
            mutate(time = strsplit(dateTime, split = ", ", perl = T)[[1]][2]) %>%
            # We extract and indicator to sum (or not num) 12 hours to time
            mutate(amPm = strsplit(time, split = " ", perl = T)[[1]][2]) %>%
            mutate(amPm = str_replace_all(amPm, "\\.", "")) %>%
            mutate(amPm = tolower(amPm)) %>%
            # Remove puntuation to normalize am/pm indicator
            mutate(dateTimeChat = str_replace(dateTime, "([Aa].*|[Pp].*)[Mm].*", "")) %>%
            mutate(dateTimeChat = convertDate(dateTimeChat)) %>%
            mutate(breakString = as.numeric(regexpr(pattern = ": ", userMessage, perl = T))) %>%
            filter(breakString > 0) %>%
            mutate(user = substr(userMessage, 1, breakString - 1), 
                   message = substr(userMessage, breakString + 2, 9999)) %>%
            select(idMessage, dateTimeChat, amPm, user, message)
        
        dfChat <- dfChat[complete.cases(dfChat),]
        
        # Adding 12 hours in case the datetime contains pm
        dfChat$dateTimeChat[dfChat$amPm == 'pm'] = dfChat$dateTimeChat[dfChat$amPm == 'pm'] + hm("12:00")
        # Remove am/pm indicator
        dfChat <- dfChat[-3]
        
    }
    
    return(dfChat)
    
}

makeCorpus <- function(dfChat) {
    
    # Generates a corpus from message texts
    
    # INPUT
    # dfChat (data.frame) = Data frame with tidy data trasformed
    
    # OUTPUT
    # corpus (tm object) = Volatile corpora from messages
    
    # Group the messages by user
    textChat <- dfChat %>%
        # Remove all messages started with < (usually media files)
        filter(substr(message, 1, 1) != '<') %>%
        group_by(user) %>%
        summarise(message = paste(message, collapse = " "))
    
    # Create the corpus
    reader <- readTabular(mapping = list(id = "user", content = "message"))
    corpus <- Corpus(DataframeSource(textChat), 
                     readerControl = list(reader = reader, language = 'es'))
    
    return(corpus)
    
}

cleanCorpus <- function(corpus) {
    
    # Realizes a series of processes to clean corpus. Transforms all words to 
    # lower case, remove punctuation, numbers, whitespaces and stop words
    
    # INPUT
    # corpus (tm object) = Volatile corpora from messages
    
    # OUTPUT
    # c_corpus (tm object) = A volatile clean corpora from messages
    
    # TODO: Generate a customized stop words dictionary
    skipWords <- function(x) removeWords(x, stopwords("spanish"))
    
    funcs <- list(content_transformer(tolower), 
                  removePunctuation, 
                  removeNumbers, 
                  stripWhitespace, 
                  skipWords)
    c_corpus <- tm_map(corpus, FUN = tm_reduce, tmFuns = funcs)
    c_corpus <- tm_map(c_corpus, stemDocument, language = 'es')
    c_corpus <- tm_map(c_corpus, PlainTextDocument) 
    
    # Constructs a document-term matrix containing a sparse term-document 
    # matrix or document-term matrix.
    dtm <- DocumentTermMatrix(c_corpus)
    
    list(c_corpus = c_corpus, 
         dtm = dtm)
    
}

createSummarizedData <- function(dfChat) {
    
    # Summarizes the data from data frame. 
    
    # INPUT
    # dfChat (data.frame) = Data frame with tidy data trasformed
    
    # OUTPUT
    # dailyChat = Total messages by date
    # dailyUserChat = Total messages by date and member
    # weekDayChat = Total messages by week day
    # hourDayChat = Total messages by hour
    # summaryChat = Total messages, images, audio, video or GIF's
    
    # TODO: Search for more format to media file omitted in the chat
    
    dailyChat <- dfChat %>%
        # Uses as.factor to generate a nice plot with ggplot2
        group_by(dateTime = as.factor(date(dateTimeChat))) %>%
        summarise(total = n()) %>%
        arrange(dateTime)
    
    dailyUserChat <- dfChat %>%
        group_by(dateTime = as.Date(dateTimeChat), user) %>%
        summarise(total = n()) %>%
        arrange(dateTime)
    
    weekDayChat <- dfChat %>%
        group_by(weekDay = wday(dateTimeChat, label = T)) %>%
        summarise(total = n())
    
    hourDayChat <- dfChat %>%
        group_by(hourDayChat = hour(dateTimeChat)) %>%
        summarise(total = n())
    
    summaryChat <- dfChat %>%
        rowwise() %>%
        mutate(isImage =  grepl('image', message), 
               isAudio =  grepl('audio', message), 
               isVideo =  grepl('video', message), 
               isGIF = grepl('GIF', message)) %>%
        group_by(user) %>%
        summarise(totalMessages = n(), 
                  totalImages = sum(isImage), 
                  totalAudios = sum(isAudio), 
                  totalVideos = sum(isVideo), 
                  totalGIFs = sum(isGIF))
    
    list(dailyChat = dailyChat, 
         dailyUserChat = dailyUserChat, 
         weekDayChat = weekDayChat, 
         hourDayChat = hourDayChat, 
         summaryChat = summaryChat)
}

makeBarPlot <- function(summaryData, yTotals, xCategories, categories, 
                        ordered = F, discrete = T){
    
    # Generate an interactive bar plot with highcharter package
    
    # INPUT
    # summaryData (data.frame) = Data grouped by categories 
    # yTotals (string) = Name column of measure in the data frame
    # xCategories (string) = Name column of categories in the data frame
    # TODO: Extract categories from data frame
    # categories (vector) = Categories from the data frame
    # ordered = F (boolean) = Should the plot be presented in descending order?
    # discrete = T (boolean) = Should the plot be presented with a discrete color palette?
    
    # OUTPUT
    # hc (highcharter object) = Interactive plot to render
    
    categories <- c(" ", as.character(categories))
    
    summaryPlot <- summaryData %>%
        mutate_(value = yTotals, y = yTotals, name = xCategories) %>%
        mutate(name = as.character(name))
    
    if (ordered == T) summaryPlot <- summaryPlot[order(summaryPlot$y, decreasing = TRUE), ]
    
    summaryPlot['x'] <- 1:nrow(summaryPlot)
    
    if (discrete == T){
        colors <- data_frame(name = categories,
                             color = brewer.pal(length(categories), "Dark2"))
        
        summaryPlot <- left_join(summaryPlot, colors, by = "name")
        summaryPlot <- summaryPlot[c('x', 'y', 'value', 'name', 'color')]
        
        dataLabelsEnabled <- TRUE 
        
    } else {
        summaryPlot['color'] <- "#00A65A"
        summaryPlot <- summaryPlot[c('x', 'y', 'value', 'name', 'color')]
        
        dataLabelsEnabled <- FALSE
    }
    
    
    hc <- highchart() %>%
        hc_chart(type = "column", polar = FALSE) %>%
        hc_xAxis(categories = c(" ", summaryPlot$name)) %>%
        hc_yAxis(labels = list(format = "{value}")) %>%
        hc_add_series(data = summaryPlot,
                      type = "column",
                      name = "Total",
                      showInLegend = FALSE, 
                      dataLabels = list(enabled = dataLabelsEnabled))
    return(hc)
}
