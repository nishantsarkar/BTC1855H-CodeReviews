# Assignment 3 - Hangman Game
# By: Sanari Wickramaratne
# REVIEWED by Nishant Sarkar
# Code review comments tagged with NS

#' NS: Your Github repository looks good, with frequent commits. I'd advise that
#' you label your commit messages a bit more descriptively in case you need to
#' revert back to a previous version. 

# Load word list found in txt. file within Assignment_3 Repository
# Must save this file within working directory
wordlist <- readLines("Dictionary.txt")

# Choose a random word from the word list
get_secret_word <- function(wordlist) {
  tolower(sample(wordlist, 1))
}
#' NS: Good use of sample() to load from Dictionary.txt. I'm not sure if having get_secret_word
#' as a separate function is particularly more efficient in this code since it is
#' only called once - the reset_game function could have just included a line to
#' sample from the wordlist instead. 

# Function to check if the user input is a valid letter
valid_letter <- function(user_input) {
  grepl("^[a-zA-Z]$", user_input)
}

#' NS: Great use of grepl and regex to check whether or not the input string consists
#' of alphabets using [a-zA-z] and checking whether there is only one input character
#' using the ^ and $ anchors. If you wanted to avoid regex here, a partial solution 
#' could be to check if user_input is present in the 'letters' vector in R, like so:
#'   valid_letter <- function(user_input) {
#'     tolower(user_input) %in% letters
#'   }
#' Still, your method is thorough and all-encompassing. I tried to break it and failed!

# Function to check if the user input is a valid command incase they want to type the whole word, restart the game or exit the game. 
# To do this, the user can type "word" and hit enter if they want to type out the whole word instead of guessing single letters, "restart" to restart the entire game, and "exit" to leave the game. 
valid_command <- function(input) {
  tolower(input) %in% c("word", "restart", "exit")
}

#' NS: This is a creative solution to also check for the additional commands you
#' implemnented in this game. By checking whether 'input' corresponds to one
#' of the commands in the vector, the program can recognize inputs other than
#' single letters. Very descriptive notation here as well.

# Function to reset the game variables
reset_game <- function() {
  secret_word <<- get_secret_word(wordlist) # To get the word from wordlist 
  word_length <<- nchar(secret_word) # Get the length of the secret word
  guessed_letters <<- character(0) # Initialize variables to store guessed letters during game and the number of wrong guesses
  wrong_guesses <<- 0
}

#' NS: Good use of double-headed arrows above to ensure that these assignments
#' occur in the global environment and not within the function. Having reset_game()
#' as a function also enables the additional functionality of letting the player
#' reset the game if they want, which is great. 

# Visually display the current state of the game for the user
display_state <- function(secret_word, guessed_letters, wrong_guesses, max_wrong_guesses) {
  revealed_word <- ""
  for (letter in strsplit(secret_word, "")[[1]]) {
    if (letter %in% guessed_letters) {
      revealed_word <- paste(revealed_word, letter, sep = " ")
    } else {
      revealed_word <- paste(revealed_word, "_", sep = " ")
    }
  }
  cat("Word:", revealed_word, "\n")
  cat("Guesses:", guessed_letters, "\n")
  cat("Wrong guesses:", wrong_guesses, "/", max_wrong_guesses, "\n")
}

#' NS: The display_state function is a great overall solution to providing a visual
#' representation of the game's progress to players by showing the partially revealed
#' word, the guessed letters, and the number of wrong guesses. If you wanted to avoid
#' using a for loop and running it every turn, a solution could be to display the
#' secret word as a vector of underscores corresponding to its length, and updating it
#' as the player guesses correct letters. This could look like this:
#'   revealed_word <- rep("_", word_length)
#'   (...if statement...)
#'     revealed_word[strsplit(secret_word, "")[[1]] == user_input] <- user_input

# Game loop control variable
game_over <- FALSE

# Introduction to the game
cat("Welcome to Sanari's Hangman Game! The category is cities within Ontario, Canada. I hope you enjoy playing! \n")
cat("You are allowed 10 guesses in total. If you would like to guess the whole word, simply type 'word'. If you would like to restart the game, type 'restart'. If you would like to exit the game, type 'exit'. Good luck!!\n")
#' NS: Super minor, but I would mention that the player immediately loses if they
#' fail to guess the whole word!

# Set the maximum number of wrong guesses
max_wrong_guesses <- 10

#' NS: Good work initializing all your variables and notating them properly. It
#' might be easier for you to keep initializations all in one place at the beginning
#' of the script instead of throughout the code, so you can change them quickly. 

# Reset the game initially
reset_game()

# Create a while loop to prompt user to enter a letter until the game is over or until they exit, or restart the game
# Note: the user can only play the game once if they have successfully won. 
#' NS: I really love tying your while loop to the game_over variable instead of
#' something like the number of wrong guesses. This way, you can employ several
#' different ways that the player can lose the game. Smart! 
while (!game_over) {
  display_state(secret_word, guessed_letters, wrong_guesses, max_wrong_guesses)
  #' NS: Super clean way to display the current state of the game and makes this
  #' section of the code VERY readable. 
  
  # Prompt user to enter a letter or command
  guess <- tolower(readline("Please enter a letter or command (word/restart/exit): "))
  #' NS: Minor, but I would name this variable something more descriptive as this
  #' implies that it's the player's letter or word guess, not input command. 
  
  # Check if the input is a valid command
  if (valid_command(guess)) {
    #' NS: valid_command comes in handy here as a great way to check if the input
    #' is valid. Again, contributes to the neatness and readability of this section
    #' of the code, while fulfilling the input validity requirement. Awesome!
    if (guess == "word") {
      
      # Prompt user to enter the entire word
      guessed_word <- tolower(readline("Enter the entire word: "))
      if (guessed_word == secret_word) {
        display_state(secret_word, guessed_letters, wrong_guesses, max_wrong_guesses)
        cat("Congratulations! You have successfully guessed the secret word! YAY!! \n")
      } else {
        cat("Wrong word! Oh no! You lost the game. The word was:", secret_word, "\n")
      }
      game_over <- TRUE
    } else if (guess == "restart") {
      
      # Reset the game
      #' NS: Again, great functionality to reset the game here!
      reset_game()
      cat("Game restarted. Good luck!!\n")
    } else if (guess == "exit") {
      
      # Exit the game
      cat("Exiting the game. Hope you had fun! Goodbye!\n")
      game_over <- TRUE
    }
    next
  }
  
  #' NS: The nested while/if-else statements here work well and suit your purpose.
  #' These statements are properly indented, which contribute to code readability.
  #' Consider leaving a comment on the closing bracket of these statements, so you
  #' know which brackets correspond to which blocks of code. 
  
  # Check if the input is a valid letter
  if (!valid_letter(guess)) {
    cat("Please enter a valid letter.\n")
    next
  }
  
  # Check if the letter has already been guessed
  if (guess %in% guessed_letters) {
    cat("You already guessed that letter. Please try again. \n")
    next
  }
  
  #' NS: These checks fulfil the requirements for input validity. The additional
  #' functionality to ensure that the player cannot re-guess already guessed letters
  #' is a nice touch, with a very simple line of code. 
  
  # Add the guess to the guessed letters
  guessed_letters <- c(guessed_letters, guess)
  
  # Check if the guess is in the secret word
  if (guess %in% strsplit(secret_word, "")[[1]]) {
    cat("Correct guess!\n")
  } else {
    cat("Wrong guess!\n")
    wrong_guesses <- wrong_guesses + 1
  }
  
  # Check if the user has guessed all the letters
  if (all(strsplit(secret_word, "")[[1]] %in% guessed_letters)) {
    display_state(secret_word, guessed_letters, wrong_guesses, max_wrong_guesses)
    cat("Congratulations! You have successfully guessed the secret word! YAY!! \n")
    game_over <- TRUE
  }
  #' NS: I like the use of all() here, which is applied to the result of the 
  #' %in% operation to check if all elements of the resulting logical vector
  #' are true. You could also do this by checking if there are no more blank
  #' spaces in the guessed_letters vector. For example, if the blank spaces are
  #' "_" characters, you could do:
  #'   if (("_" %in% guessed_letters == FALSE)) {
  #'     (...remaining code...)
}

# END OF GAME

#' NS: Great work! Here are some overall comments:
#' The code fulfills all of the requirements set out for us, and also adds some
#' additional functionality like the ability to replay the game and the ability
#' to guess the whole word if desired. The code is structured well and adheres
#' to our style guide. The code properly checks for incorrect inputs (numbers,
#' punctuation, guesses longer than one character, or some combination of these)
#' and ensures that they do not break the code. Also, the code did not throw an
#' error during my testing. Finally, the code is very readable: it is properly
#' commented/organized, and the use of plenty of functions keeps the code clean,
#' particularly during the main game loop, and contributes to its readability. 
#' The game was a lot of fun to play! Good work!