# Game-day Simulator for Baseball (R)
library(lattice)  # graphics package for probability matrix visual  
simulator <- function(home_mean,away_mean,niterations) {   ## created a function named simulator and function has 3 parameters 
     # input runs scored means, output probability of winning for home team 
     set.seed(1234)  # set to obtain reproducible results ## used to obtain same random numbers across all users 
     away_game_score <- numeric(niterations) ## For all the iteratons for away_game_score, the data type is numeric
     home.game.score <- numeric(niterations) ## For all the iteratons for home.game.score, the data type is numeric
     home_win <- numeric(niterations) ## For all the iteratons for home_win, the data type is numeric 
     i <- 1 ## assigning i as 1
     while (i < niterations + 1) { 
         away_game_score[i] <- rnbinom(1,mu=away_mean, size = 4)    ## A random number is generated using rnbinom and it is stored in away_game_score
         home.game.score[i] <- rnbinom(1,mu=home_mean, size = 4)    ## A random number is generated using rnbinom and it is stored in home.game.score
         if(away_game_score[i] > home.game.score[i]) home_win[i] <- 1   ## If away_game_score is greater than home.game.score, assign home_win[i] =1.
         if(away_game_score[i] > home.game.score[i] || 
         away_game_score[i] < home.game.score[i]) i <- i + 1        ## If away_game_score is less than home.game.score, assign i = i+1
         }
     n_home_win <- sum(home_win) ## total home_win value is stored in n_home_win
     n_home_win/niterations  # return probability of away team winning 
     } 

niterations <- 100000  # use smaller number for testing   ##assigning number of iterations as 100000
# probability matrix for results... home team is rows, away team is columns
probmat <- matrix(data = NA, nrow = 9, ncol = 9,   ##matrix is named as probmat. Matrix is assigned with number of rows and columns as 9
  dimnames = list(c(as.character(1:9)), c(as.character(1:9)))) 
for (index_home in 1:9)
for (index_away in 1:9)
if (index_home != index_away) {    ## If the index_home value is not equal to index_away, we run the simulation 
     probmat[index_home,index_away] <- 
        simulator(index_home, index_away, niterations) ## Generated numbers for index_home and index_away from the simulation were assigned to the probability matrix table 
     }
pdf(file = "fig_sports_analytics_prob_matrix.pdf", width = 8.5, height = 8.5)  ##pdf file is created with the name "fig_sports_analytics_prob_matrix.pdf"
x <- rep(1:nrow(probmat),times=ncol(probmat)) ## each row and column is replicated
y <- NULL
for (i in 1:ncol(probmat)) y <- c(y,rep(i,times=nrow(probmat)))  ## ith iteration value is assigned to the y axis of the matrix
probtext <- sprintf("%0.3f", as.numeric(probmat))  # fixed format 0.XXX  ## ith iteration value will print with three decimal places
text_data_frame <- data.frame(x, y, probtext)  ## data frame is set with the parameters x,y, probtext
text_data_frame$probtext <- as.character(text_data_frame$probtext) 
text_data_frame$probtext <- ifelse((text_data_frame$probtext == "NA"),
    NA,text_data_frame$probtext)  # define diagonal cells as missing
text_data_frame <- na.omit(text_data_frame)  # diagonal cells
print(levelplot(probmat, cuts = 25, tick.number = 9,
    col.regions=colorRampPalette(c("violet", "white", "light blue")),
    xlab = "Visiting Team Runs Expected", 
    ylab = "Home Team Runs Expected",
    panel = function(...) {     ##save the functions in panel
        panel.levelplot(...)  
        panel.text(text_data_frame$x, text_data_frame$y, 
        labels = text_data_frame$probtext)
        }))
dev.off()        
# Suggestion for the student: Develop simulators for football or basketball.    

