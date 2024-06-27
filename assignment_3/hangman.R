#'Assignment 3, June/26/2024
#'Ibrahim Emam
#'R version: 4.4.0

########################
## DEFINING FUNCTIONS ##
########################

#Function allows the user to choose between guessing a letter or guessing a word
guess_type <- function() {
  repeat {
    input <- tolower(readline("Would you like to guess the word or a letter? (w/l): "))
    if (input == "w" | input == "l") {
      return(input)
    } else {
      cat("ERROR: Must answer with 'w' or 'l'. Try again.")
    }
  }
}

#Function prompts user for their guess (full word) and splits it into individual characters
get_word_guess <- function() {
  repeat {
    input <- tolower(readline("Enter a word to guess: "))
    if(is_letters(input)) {
      input <- unlist(strsplit(input, ""))
      return(input)
    } else {
      cat("ERROR: Guess must not contain numbers or symbols. Try again.")
    }
  }
}

get_letter_guess <- function() {
  repeat {
    input <- tolower(readline("Enter a letter to guess: "))
    if(is_letters(input) & nchar(input) == 1) {
      return(input)
    } else {
      cat("ERROR: Must enter 1 letter with no numbers or symbols. Try again.")
    }
  }
}

#function checks that inputted string contains no numbers or symbols
is_letters <- function(input_string) {
  
  #returns TRUE if the inputted string only contains the letters a-z (no symbols/numbers)
  result <- grepl("^[a-z]+$", input_string)
  return(result)
}

update_user_answer <- function(letter) {
  if(letter %in% answer_characters) {
    indexes <- which(answer_characters == letter)
    user_answer[indexes] <- letter
    cat("Correct! The letter", letter, "is in the word!\n")
  } else {
    cat("Incorrect. The letter", letter, "is NOT in the word.\n")
  }
  return(user_answer)
}

#function checks if the user has won the game
check_for_win <- function(answer_to_check) {
  if(identical(answer_to_check, answer_characters)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#read txt file containing words into a vector
words <- readLines("words.txt")

#assign number of attempts allowed
attempts_allowed <- 20

#randomly select one of the words from the vector to be used in the game
#answer <- words[sample.int(length(words), 1)]
answer <- words[3]

#create vector containing individual characters of the answer
answer_characters <- unlist(strsplit(answer, ""))

#determine the length of the word
answer_length <- nchar(answer)

#create a vector containing the user's currently filled-in letters (initially blank)
user_answer <- rep("_", times = answer_length)

for (i in 1:attempts_allowed) {
  
  #print current user answer and number of attempts remaining
  cat("You have", attempts_allowed - i + 1, "attempt(s) remaining to guess the", answer_length, "letter word.\n")
  
  #print the user's current answer (with blanks for characters not guessed)
  cat(user_answer, sep = " ", "\n")
  
  #determine if the user wants to guess the whole word or just one letter
  if (guess_type() == "w") {
    #the user wants to guess the entire word

    #use get_word_guess() function to obtain the word the user would like to guess
    word_guess <- get_word_guess()
    
    #check if the user guessed the word correctly
    if(check_for_win(word_guess)) {
      
      #change user answer to match actual answer
      user_answer <- answer_characters
      
      #display the user's (correct) answer
      cat(user_answer, sep = " ", "\n")
      
      #inform the user they have won
      cat("YOU WIN!")
      
      #end the loop since the user has won
      break
      
    } else {
    
      #the user guessed the incorrect word
      cat("Incorrect Guess.")
      
    }
    
  } else {
    #the user wants to guess one letter
    
    letter_guess <- get_letter_guess()
    user_answer <- update_user_answer(letter_guess)
    if(check_for_win(user_answer)) {
      cat(user_answer, sep = " ", "\n")
      cat("YOU WIN!")
      break
    }
  }
}

if(!check_for_win(user_answer)) {
  cat(user_answer, sep = " ", "\n")
  cat("No attempts remaining. The word was:", answer)
}