#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(googlesheets)
library(stringr)
library(plotrix)
library(lubridate)

gs_auth(new_user = TRUE, token = 'dah-token.rds')
sheet_name <- 'Copy of BARUG August Survey (Responses)'
questions <- character()
level_vals <- character()
sim_button_value <- 0
refresh_button_value <- -1

get_survey_data <- function(action) {
    message('get_survey_data action', action)
    survey <- gs_title(sheet_name) %>% gs_read()
    questions <<- names(survey)
    names(survey) <- c('Time', 'csvLevel', 'nBARUG', 'nMeetups', 'gs_freq', 'goodness', 'good_words', 'animal')
    
    level_vals <<- strsplit(survey$csvLevel, ',') %>% unlist() %>% unlist() %>% str_trim() %>% unique() %>% sort()
    level_vals <<- level_vals[! grepl('NANA', level_vals)] #kludge
    n_lev <- length(level_vals)
    
    level_mat <- matrix(data = FALSE, nrow = nrow(survey), ncol = n_lev)
    for (lev in 1:length(level_vals)) level_mat[ , lev ] <- 
      grepl(level_vals[lev], survey$csvLevel) %>%
      as.integer()
    colnames(level_mat) <- paste0('L', 1:n_lev)
    survey_aug <- bind_cols(survey, as.data.frame(level_mat))
    survey_aug$csvLevel <- NULL
    survey_aug
}

simulate_rows <- function(n) {
    message('simulate_rows action: nrows = ', n)
  
    #TODO - this section is copied from get_survey_data() ... should be a common function
    #update maybe just get rid of get_survey_data ?
    survey <- gs_title(sheet_name) %>% gs_read()
    questions <<- names(survey)
    names(survey) <- c('Time', 'csvLevel', 'nBARUG', 'nMeetups', 'gs_freq', 'goodness', 'good_words', 'animal')
    survey <- survey %>% filter(! grepl('I have used', csvLevel)) #remove old gs answer
    survey$csvLevel <- gsub('Notebook,', 'Notebook', survey$csvLevel) #unfortunate comma in question response
    level_vals <<- strsplit(survey$csvLevel, ',') %>% unlist() %>% unlist() %>% str_trim() %>% unique() %>% sort()
    level_vals <<- level_vals[! grepl('NANA', level_vals)] #kludge
    n_lev <- length(level_vals)
    
    level_mat <- matrix(data = FALSE, nrow = nrow(survey), ncol = n_lev)
    for (lev in 1:length(level_vals)) level_mat[ , lev ] <- 
      grepl(level_vals[lev], survey$csvLevel) %>%
      as.integer()    
    colnames(level_mat) <- paste0('L', 1:n_lev)
    survey_aug <- bind_cols(survey, as.data.frame(level_mat))
    survey_aug$csvLevel <- NULL
    ##### end of copy paste kludge/TODO
    
    if (n <= 0) return(survey_aug)
    
    set.seed <- nrow(survey)
    n_sim <- n
    clip <- function(x, min, max) ifelse(x > max, max, ifelse(x < min, min, x)) 
    cap_pois <- function(n, lambda, max) clip(rpois(n, lambda), 1, max)
    cap_norm <- function(n, m, s, max) clip(round(rnorm(n, m, s)), 1, max) 
    
    vtime <- (now() + hours(-n_sim:-1)) %>% as.character()
    vnbarug <- cap_pois(n_sim, 4, 10)
    vnmeet <- cap_pois(n_sim, 5, 10)
    freq_resp <- c('Multiple times daily', 'About once a day', 'A couple time a week', 'Rarely', 'Never')
    vfreq <- freq_resp[cap_pois(n_sim, 2, 5)]
    vanimal <- sort(unique(survey$animal))[cap_norm(n_sim, 3, 1.3, 5)]
    vgood <- rbinom(n_sim, 1, 0.8)
    vgood[sample(1:n_sim, n_sim %/% 10)] <- NA
    lev_prob <- c(.9, .4, .3, .6, .2, .4, .75)
    lev_sim <- matrix(nrow=n_sim, ncol = n_lev )
    for (i in 1:n_lev) lev_sim[ ,i] <- rbinom(n_sim, 1, lev_prob[i])
    lev_sim_df <- lev_sim %>% as.tibble() %>% set_names(paste0('L', 1:n_lev))
    for (i in 1:n_lev) lev_sim[ ,i] <- ifelse( lev_sim[ ,i] == 1, paste0(level_vals[i], ','), '')
    lev_str <- lev_sim %>% as.tibble() %>% 
        unite(levels, starts_with('V'), sep='') %>% 
        .[[1]] %>% str_replace(',$', '')
    
    
    sim <- data_frame(Time = vtime, csvLevel = lev_str, nBARUG = vnbarug, nMeetups = vnmeet, 
                      gs_freq = vfreq, goodness = vgood, good_words = "", animal = vanimal ) 
    bind_rows(survey, sim) %>% set_names(questions) %>% write.csv('tmp_survey.csv', row.names=FALSE)
    gs_upload('tmp_survey.csv', sheet_title=sheet_name, overwrite=T)

    sim2 <- data_frame(Time = vtime, nBARUG = vnbarug, nMeetups = vnmeet, 
                      gs_freq = vfreq, goodness = vgood, good_words = "", animal = vanimal ) %>%
        bind_cols(lev_sim_df)
    
    bind_rows(survey_aug, sim2)
    
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("BARUG User Responses - August 8th, 2017"),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(column(12, wellPanel(
        actionButton('refresh', 'Refresh'),
        actionButton('sim_rows', 'Simulate')
    ))),
    
    fluidRow(column(12,wellPanel( htmlOutput("nrows")))),
        
    fluidRow(
        column(6, wellPanel(
            plotOutput("rad_levels")
        )),
        column(3, wellPanel(
            plotOutput("hist_barug")
        )),
        column(3, wellPanel(
            plotOutput("hist_meetup")
    )))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    resp <- reactive({
        if (input$sim_rows > sim_button_value) {
            simulate_rows(10)
        } else if (input$refresh > refresh_button_value) {
            refresh_button_value <<- 0
            simulate_rows(0)
        }
    })
    #resp0 <- reactive({get_survey_data(input$refresh)})
    
    output$rad_levels <- renderPlot({
        levs <- 100 * colSums( resp()[which(grepl('^L', names(resp())))]) / nrow(resp())
        print(names(resp()))
        level_abbr <- c('basic\nscript', 'published\n markdown', 'googlesheets', 'R Markdown', 'CRAN\npackage', 'shared\nfunctions', '%>%' )
        par.orig <- par(ps=12)
        print(levs)
        radial.plot(levs, labels=level_abbr, rp.type='p', radial.lim=c(0,100), line.col='skyblue', start=3.14/6,poly.col = 'blue') 
    })
    
    output$hist_barug <- renderPlot({
        hist(resp()$nBARUG, breaks = 11, col = 'darkgray', border = 'white', xlab='', main = 'BARUG meetings attended')
    })
    
    output$hist_meetup <- renderPlot({
        hist(resp()$nMeetups, breaks = 11, col = 'darkgray', border = 'white', xlab='', main = 'Tech meetups in last 6 months')
    })
    
    output$nrows <- renderText({
        sprintf("<h4>%d responses</h4>", nrow(resp()))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

