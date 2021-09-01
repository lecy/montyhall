#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title 
#' Selecting a door 
#' 
#' @description
#' `select_door()`Randomly select one door out of ( 1,2,3 ) set and return the
#'  selection
#' 
#' @details
#' Assume that a room is equipped with three doors. Behind two are goats,
#' and behind the third is a shiny new car. In this function the player is
#' picking a random door out of the three doors, and the game will continue
#' as the player will see below till he win or lose.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a number between 1 and 3
#' 
#' @examples
#'   select_door()
#'  
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host opens a goat door
#' 
#' @description
#' `open_goat_door(game,a.pick)` return a door that contain a goat behind it.
#' 
#' @details
#' Before the selected door is opened however, someone who knows what's 
#' behind the doors (Monty Hall) opens one of the other two doors, but it 
#' can't be a door the contestant has already selected . So it must be a door 
#' that is not a car and not a current contestant selection.
#' 
#' @param game Character, car or one of the goats
#' @param a.pick Numeric, door numbers
#' 
#' @return a number between 1 and 3 that contain a goat behind it
#' 
#' @examples
#' game <- create_game()
#' a.pick <- select_door()
#' open_goat_door ( game, a.pick )
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Change the door
#' 
#' @description
#' `change_door(stay=T, opened.door, a.pick)` generate an option for the player
#' to change his selection or remain on his current selection
#' 
#' @details
#' The contestant is given the option to change from their initial selection to 
#' the other door that is still closed. The function will represent the game-
#' playing strategy as the argument stay=TRUE or stay=FALSE
#' 
#' @param Stay= T or F, logical
#' @param opened.door Numeric, opened goat door
#' @param a.pick Numeric, Player's initial selection
#' 
#' @return return a number between 1 and 3, depending if the player stay or
#' remain on his selection.
#' 
#' @examples
#' opened.door <- open_goat_door( game, a.pick )
#' a.pick <- select_door()
#' change_door( stay=T, opened.door, a.pick )
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine winner
#' 
#' @description
#' This function determine if the player wins or lose.
#' 
#' @details
#' After the player decided his final selection, whether he wants to stay on
#' his first selection or choose to change, this function determine if he win
#' a car or lose it.
#' 
#' @param final.pick Numeric, Player's final pick door between 1&3 
#' @param game Character, Car or a Goat
#' 
#' @return Return Character, "WIN" or "Lose"
#' 
#' @examples
#' final.pick = 1
#' game <- create_game()
#' determine_winner <- function( final.pick, game )
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title 
#' Monty Hall Game
#' 
#' @description
#' `play_game()` is a function that tests the Monty Hall game and tells what
#' will happen if the CONTESTANT choose to stay or change his selection.
#' 
#' @details
#' THe function will play the  game will be played by running the functions 
#' in order in a script. The game script will be in run a functions in
#' the following order:
#' 1. One function that sets up the game (three doors, one car, two goats).
#' 2. One function that selects a door for your first guess.
#' 3. One function that reveals a goat for the host.
#' 4. One function that makes your final pick 
#' (depending upon if you intend to stay or switch).
#' 5.One function that decides if you win the car or not.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return Strategy of the CONTESTANT : Stay or Switch and in each case the 
#' outcome: WIN or Lose
#' 
#' @examples
#' play_game <- function( )
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Adding the Game to a Loop
#' 
#' @description
#' `play_n_games(n)` add the Monty Hall game to a loop consist of 100 iteration
#' and then tabulate the results of number of wins and loses out of this 
#' iteration
#' 
#' @details
#' When running simulations, at least we should run the game 100 time to get a 
#' result that we can rely on. In this function we simulate the game 100 time,
#' then the function will calculate the probability of winning and loosing  
#' according to the contestant choice/strategy (Stay or switch) out of 100
#' time iteration
#' 
#' @param n=100 Numeric, 100 times iteration
#' 
#' @return a table includes strategy (Switch or Stay), Outcome(WIN or Lose),
#' and the probability of each outcome according to the chosen stategy.
#' 
#' @examples
#' play_n_games()
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
