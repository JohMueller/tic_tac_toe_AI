### Game mechanics

# Init board
board <-  c(0,0,0,
            0,0,0,
            0,0,0)

### Helper Functions

# Make a new move
make_move <- function(board, player, move){
  if(board[move] == 0){
    board[move] <- player
  }else{
    print("illegal move")
  }
  return(board)
}

# Get all possible moves
possible_moves <- function(board){
  moves <- which(board == 0)
  return(moves)
}

# Decide on a random next move
random_move <- function(board){
  if(length(possible_moves(board)) >1){
    random_move <- sample(possible_moves(board), 1)
  }else{
    random_move <- possible_moves(board)
  }
  return(random_move)
}
  
# Evaluate board if someone has won
evaluate_win <- function(board){
  board_mat <- matrix(board, ncol = 3, byrow = T)
  
  win_conditions <- list(
    board_mat[1,], board_mat[2,], board_mat[3,], # check rows
    board_mat[,1], board_mat[,2], board_mat[,3], # check cols
    diag(board_mat), diag(board_mat[,c(3:1)])    # check diagonals
  )
  
  winner <- NULL
  if(list(c(1,1,1)) %in% win_conditions){         # player 1 wins
    winner <- 1
  }else if (list(c(2,2,2)) %in% win_conditions){  # player 2 wins
    winner <- 2
  }else if (0 %in% board_mat){                 # Game is over with TIE
    winner <- NULL
  }else {
    winner <- 0
  }
  return(winner)
}

# Print current version of the board
print_board <- function(board){
  print(matrix(board, nrow = 3, ncol=3))
}

### Simulate a game

game <- function(ai_on = T, ai_mode = "aggressive"){
  board <-  c(0,0,0,
              0,0,0,
              0,0,0)
  winner = NULL
  
  #print_board(board)
  while(is.null(winner)){
    
    #player 1:
    #next_move <- as.integer(readline(prompt = "Next Move Player 1: "))
    if(ai_on == T){
      next_move <- AI_move(board, player = 1, ai_mode)
    }else{
      next_move <- random_move(board)
      }
    
    board <- make_move(board, 1, next_move)
    winner <- evaluate_win(board)
    #print_board(board)
    
    if(is.null(winner)){
      #player 2: 
      next_move <- random_move(board)
      board <- make_move(board, 2, next_move)
      winner <- evaluate_win(board) 
      
      #print_board(board)
    }
  }
  #print(paste0("And the winner is ", winner))
  return(winner)
}

game(ai_on=T, ai_mode = "defensive")

### simulate 1000 games with and without AI
games_with_AI_aggressive <- c()
games_with_AI_defensive <- c()
games_without_AI <- c()

for(i in 1:70){
  games_with_AI_aggressive <- c(games_with_AI_aggressive, 
                                game(ai_on=T, ai_mode = "aggressive"))
  games_with_AI_defensive <- c(games_with_AI_defensive, 
                               game(ai_on=T, ai_mode = "defensive"))
  games_without_AI <- c(games_without_AI, game(ai_on=F))
}

prop.table(table(games_without_AI))
prop.table(table(games_with_AI_aggressive))
prop.table(table(games_with_AI_defensive))

