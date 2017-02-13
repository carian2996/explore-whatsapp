# Author: Ian C
# Date: Feb 12, 2017

# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

# TODO: Better web design for application

library(shiny)
library(shinydashboard) # nicer view to the app
library(highcharter) # interactive Javascript charts

dashboardPage(skin = "green", 
              dashboardHeader(title = "Explore WhatsApp"),
              
              # ===== Side bar =====
              dashboardSidebar(
                  
                  # TODO: Write simple instructions for user. 
                  # Or even better: http://introjs.com/example/hello-world/index.html
                  
                  # ===== Input controls =====
                  # Depending of the platform the chat format can be different
                  # iPhone = dd/mm/YYYY hh:mm:ss: User: Message!
                  # Android = dd/mm/YY hh:mm [AM], [-] User: Message!
                  selectInput('platform', label = 'Platform', 
                              choices = c("iPhone", "Android"), 
                              selected = "Android"),
                  
                  # Only works to select the date fomart depending on the language
                  # Spanish = dd/mm/YYYY hh:mm:ss
                  # English = mm/dd/YYYY hh:mm:ss
                  selectInput('language', label = 'Language', 
                              choices = c("Spanish", "English"), 
                              selected = "Spanish"),
                  
                  # It Uploads depending on the chat data
                  selectInput('memberChoice', 'Member', ""),
                  
                  hr(),
                  
                  fileInput("file", label = "File input", accept = ".txt"),
                  
                  actionButton("goButton", "Go!", icon = icon('cogs')), 
                  tags$style(type='text/css', "button#goButton { margin-left: 12px; }"), # makes the button moves to the left
                  
                  hr(),
                  
                  # ===== Dashboard sections =====
                  sidebarMenu(
                      menuItem("Main Pannel", tabName = "dashboard"),
                      menuItem("Summary Data", tabName = "summaryData"),
                      menuItem("Sample Raw Data", tabName = "rawData")
                  )
              ),
              
              # ===== Dashboard body =====
              dashboardBody(
                  tabItems(
                      
                      # ===== Main Pannel =====
                      tabItem("dashboard",
                              
                              # ===== Value Boxes =====
                              fluidRow(
                                  valueBoxOutput("members", width = 4), 
                                  valueBoxOutput("messages", width = 4),
                                  valueBoxOutput("moreActive", width = 4)
                              ), 
                              
                              # ===== Highchart plots =====
                              fluidRow(
                                  box(
                                      status = "success",
                                      width = 4,
                                      title = "Messages per Member",
                                      highchartOutput(outputId = "summaryPlot", height = "250px")
                                  ), 
                                  box(
                                      status = "success",
                                      width = 4,
                                      title = "Messages per Weekday",
                                      highchartOutput(outputId = "weekDayPlot", height = "250px")
                                  ),
                                  box(
                                      status = "success",
                                      width = 4,
                                      title = "Messages per Hour",
                                      highchartOutput(outputId = "hourDayPlot", height = "250px")
                                  )
                              ),
                              
                              # ===== Emoji Table and Scatter Plot =====
                              fluidRow(
                                  box(
                                      status = "success",
                                      width = 3,
                                      title = "Top Emoji by Member",
                                      tableOutput("topEmoji")
                                  ),
                                  box(
                                      status = "success",
                                      width = 9,
                                      title = "Messages in Chat by Member",
                                      plotOutput("dailyUserPlot")
                                  )
                              ),
                              
                              # ===== Deleted messages warning =====
                              verbatimTextOutput("warning")
                      ), 
                      
                      # ===== Summary Data Pannel =====
                      tabItem("summaryData",
                              tabsetPanel(type = "tabs", 
                                          tabPanel("Summary Chat", dataTableOutput("summaryChat")),
                                          tabPanel("Daily Chat", dataTableOutput("dailyChat")),
                                          tabPanel("Daily User Chat", dataTableOutput("dailyUserChat")),
                                          tabPanel("Weekday Chat", dataTableOutput("weekDayChat")),
                                          tabPanel("Hour Day Chat", dataTableOutput("hourDayChat"))
                              )
                      ),
                      
                      # ===== Raw Data Pannel =====
                      tabItem("rawData",
                              tableOutput("content")
                      )
                  )
              )
)