
AI_move <- function(board, player, ai_mode = "aggressive"){
  if(length(possible_moves(board)) >1){
    
    move_probas <- c()
    ### Iterate over all possible moves and calculate winning probability
    for(move in possible_moves(board)){
      potential_board <- make_move(board, player, move)
      
        # if aggressive it will maximize for winning probability
      if(ai_mode == "aggressive"){
        potential_board_proba <- evaluate_board_position(potential_board, 
                                                             boards_df, 
                                                             model)[2]
      }else{
        # if defensive it will maximze for "not loosing" probability
        potential_board_proba <- 1 - evaluate_board_position(potential_board, 
                                                             boards_df, 
                                                             model)[3]
      }
    
      move_probas <- c(move_probas, potential_board_proba)
    }
    
    ### find move which creates board with best winning probability
    best_move_proba <- max(move_probas)
    next_move <- possible_moves(board)[c(move_probas == best_move_proba)]
    
  }else{
    #if there is only on possible move left, do this move
    next_move <- possible_moves(board)
  }
  return(next_move)
}

#examples
AI_move(board, player = 1, ai_mode = "aggressive")



