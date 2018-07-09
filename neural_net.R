## Train a neural net
library(keras)
df <- boards_df

# one hot encoding

oneHotEncoding <- function(dataframe, variables= 1:9){
  df <- dataframe
  
  df[,variables] <- data.frame(apply(df[variables], 2, as.factor)) # to factor 
  
  for(i in variables){
    for(level in unique(df[,i])){
      df[paste0("X",i,"_",level)] <- ifelse(df[,i] == level, 1, 0)
    }
  }
  
  df <- df[,-c(variables)] # drop old variables
  df <- df[, !grepl("_0", (names(df)))] # drop all variables with level 0
  df <- as.matrix(df)
  return(df)
}


# split dataset in training and test data

train <- sample(1:nrow(df), nrow(df)*0.8)

x_train <- df[train,1:9]
y_train <- df[train, 10]
x_test <- df[-train,1:9]
y_test <- df[-train, 10]

y_train <- to_categorical(y_train, 3)
y_test <- to_categorical(y_test, 3)
x_train <- oneHotEncoding(x_train, 1:9)
x_test <- oneHotEncoding(x_test, 1:9)

# build neural network

#Initialize model
model <- keras_model_sequential() 

#Build layers of model
model %>% 
  layer_dense(units = 9, 
              kernel_initializer = "uniform",
              activation = 'relu',
              input_shape = c(18)) %>%
  layer_dense(units = 9,
              kernel_initializer = "uniform",
              activation = 'relu') %>%
  layer_dense(units = 3, activation = 'softmax')

# Compile Model
model %>% compile(loss = 'categorical_crossentropy',
                  optimizer = optimizer_rmsprop(),
                  metrics = c('accuracy'))

# Train Model
history <- model %>% fit(
  x_train, y_train, 
  epochs = 20, batch_size = 100, 
  validation_split = 0.2
)

model %>% evaluate(x_test, y_test)

### Function to evaluate any board position.
### --> It takes one board board as an input, e.g. c(0,0,0,0,1,0,0,0,0)
### --> Furthermore, a model is needed and the dataframe with the simulations

evaluate_board_position <- function(board_to_evaluate = c(0,0,0,0,1,0,0,0,0), boards_df, model){
  evaluation_boards <- boards_df[1:1000,1:9] # this is needed to not mess up the factor levels; bad coding I know
  evaluation_boards <- rbind(evaluation_boards, 
                             board_to_evaluate) 
  evaluation_boards <- oneHotEncoding(evaluation_boards, 1:9)
  evaluation_boards <- tail(evaluation_boards, 2)
  
  predictions <- model %>% predict_proba(evaluation_boards)
  return(predictions[2,])
}

#example
evaluate_board_position(c(2,0,0,0,1,0,0,0,0), boards_df, model)
