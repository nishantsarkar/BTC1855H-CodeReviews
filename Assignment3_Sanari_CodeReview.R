# Assignment 3 - Hangman Game
# By: Sanari Wickramaratne
# REVIEWED by Nishant Sarkar
# Code review comments tagged with NS

# Load word list found in txt. file within Assignment_3 Repository
# Must save this file within working directory
wordlist <- readLines("Dictionary.txt")

# Choose a random word from the word list
get_secret_word <- function(wordlist) {
  tolower(sample(wordlist, 1))
}

# Function to check if the user input is a valid letter
valid_letter <- function(user_input) {
  grepl("^[a-zA-Z]$", user_input)
}

# Function to check if the user input is a valid command incase they want to type the whole word, restart the game or exit the game. 
# To do this, the user can type "word" and hit enter if they want to type out the whole word instead of guessing single letters, "restart" to restart the entire game, and "exit" to leave the game. 
valid_command <- function(input) {
  tolower(input) %in% c("word", "restart", "exit")
}

# Function to reset the game variables
reset_game <- function() {
  secret_word <<- get_secret_word(wordlist) # To get the word from wordlist 
  word_length <<- nchar(secret_word) # Get the length of the secret word
  guessed_letters <<- character(0) # Initialize variables to store guessed letters during game and the number of wrong guesses
  wrong_guesses <<- 0
}

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

# Game loop control variable
game_over <- FALSE

# Introduction to the game
cat("Welcome to Sanari's Hangman Game! The category is cities within Ontario, Canada. I hope you enjoy playing! \n")
cat("You are allowed 10 guesses in total. If you would like to guess the whole word, simply type 'word'. If you would like to restart the game, type 'restart'. If you would like to exit the game, type 'exit'. Good luck!!\n")

# Set the maximum number of wrong guesses
max_wrong_guesses <- 10

# Reset the game initially
reset_game()

# Create a while loop to prompt user to enter a letter until the game is over or until they exit, or restart the game
# Note: the user can only play the game once if they have successfully won. 
while (!game_over) {
  display_state(secret_word, guessed_letters, wrong_guesses, max_wrong_guesses)
  
  # Prompt user to enter a letter or command
  guess <- tolower(readline("Please enter a letter or command (word/restart/exit): "))
  
  # Check if the input is a valid command
  if (valid_command(guess)) {
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
      reset_game()
      cat("Game restarted. Good luck!!\n")
    } else if (guess == "exit") {
      
      # Exit the game
      cat("Exiting the game. Hope you had fun! Goodbye!\n")
      game_over <- TRUE
    }
    next
  }
  
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
}

# END OF GAME