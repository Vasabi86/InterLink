library(shiny)
library(visNetwork)
library(dplyr)
library(igraph)

shiny::shinyUI(shiny::navbarPage(
  title = "Дані про платіжну систему",

  shiny::tabPanel(
  title = "Візуалізація транзакцій - Графи",
  fluidRow(
    column(2,
	wellPanel(
	#   dateRangeInput('days', label = "Date range input (2016-06-20 to 2016-06-22)"),
		dateInput("days", label = h3("Date input (2016-06-20 to 2016-06-22)"), value = "2016-06-20", min = "2016-01-01", max = "2016-07-09"),
		
   sliderInput("perm", "операції у проміжку:", 1, 96, 1, step = 1, animate=animationOptions(interval=1800, loop=T)),
   tags$h3(uiOutput("volume_text")),
   checkboxGroupInput("checkBank", label = h3("Checkbox group"), choices = list("Privat" = 1, "Oschad" = 2, "Exim" = 3, "PIB" = 4, "Aval" = 5, "UkrSoc" = 6, "Sber" = 7,
				   	"Alfa" = 8, "Fuib" = 9, "Vtb" = 10, "UkrSib" = 11, "UkrGaz" = 12, "OTP" = 13, "AgriCole" = 14, 
					"Pivd" = 15, "Fido" = 16, "Ing" = 17, "Citi" = 18, "Kreschatyk" = 19, "KreditDnipro" = 20), selected = 1:20)
					
	# selectInput("color_n", "Color n:", c("blue", "red", "green")),
	# selectInput("color_e", "Color e:", c("blue", "red", "green")),
	# selectInput("focus_n", "Focus on node :", c(1:20))
      
    )),
    column(10,
      visNetworkOutput("network_vis", width = "100%", height = "700px")
    )
  ),
    verbatimTextOutput("c_base")
),
tabPanel(
  title = "Табл node",
  fluidRow(
  tableOutput('info_q_n')
  )),
  tabPanel(
  title = "Табл node_2",
  fluidRow(
  tableOutput('info_q_nn')
  )),
#  tabPanel(
#  title = "Табл node_con",
#  fluidRow(
#  tableOutput('info_q_nn_con')
#  )), 
  tabPanel(
  title = "Табл node_q_n_mod",
  fluidRow(
  tableOutput('info_q_n_mod')
  )), 
#   tabPanel(
#  title = "Табл dataup",
#  fluidRow(
#  tableOutput('dataup')
#  )), 
  tabPanel(
  title = "Табл edge",
  fluidRow(
  tableOutput('info_q_e')
  ))#,
#tabPanel(
# title = "Табл edge",      info_q_n_mod
#  fluidRow(
#  tableOutput('info_q_n_mod')
#  ))
))

