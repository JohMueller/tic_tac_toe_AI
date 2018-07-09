### Write a neural net that determines the best move

# Evaluate the chance of winning for player 1 for a given board

# Create a training dataset

simulate_game <- function(){
  
  boards_history <- data.frame(matrix(NA, ncol = 9))
  #boards_history <- data.frame(X1 = numeric(), X2=numeric)
  board <-  c(0,0,0,
              0,0,0,
              0,0,0)
  winner = NULL
  player = 1
  
  while(is.null(winner)){
    
    next_move <- random_move(board)
    board <- make_move(board, player, next_move)
    if(player == 1){player = 2}else{player = 1}
    winner <- evaluate_win(board)
    boards_history <- rbind(boards_history, board)
    #print(board)
  }
  boards_history$winner <- winner
  return(boards_history)
}

simulate_game()

# Init new dataset
boards_df <- simulate_game()[-1,] # remove first row of NAs

for(i in 1:5000){
  new_game <- simulate_game()
  new_game <- new_game[-1,]
  boards_df <- rbind(boards_df, new_game)
}

#names(boards_df) <- c(paste0("X", 1:9), "winner")
