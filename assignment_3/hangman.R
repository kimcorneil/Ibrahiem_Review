#'Assignment 3, June/26/2024
#'Ibrahim Emam
#'R version: 4.4.0

###########
## NOTES ##
###########

#'it is recommended to start with reading the main section of the code before the functions
#'the text file containing the words is all lowercase, so user input throughout the code will be forced to match
#'this program assumes the "words.txt" file is in your working directory
#'the cat() function was used in place of the print() function for visual consistency


########################
## DEFINING FUNCTIONS ##
########################

#function allows the user to choose between guessing a letter or guessing a word
guess_type <- function() {
  
  #loop is used to force the user to provide valid input
  repeat {
    
    #get user input and force to lowercase
    input <- tolower(readline("Would you like to guess the word or a letter? (w/l): "))
    
    #ensure input it either 'w' or 'l' corresponding to word or letter respectively
    if (input == "w" | input == "l") {
      
      #the input is valid
      return(input)
      
    } else {
      
      #the input is invalid, loop will restart to reprompt the user
      cat("ERROR: Must answer with 'w' or 'l'. Try again.")
      
    }
  }
}

#function prompts user for their guess (full word) and splits it into individual characters
get_word_guess <- function() {
  
  #use a loop to force the user to provide valid input
  repeat {
    
    #get user input and force to lowercase
    input <- tolower(readline("Enter a word to guess: "))
    
    #make sure the input only contains letters
    if(is_letters(input)) {
      
      #split input into individual characters for each comparison with the
      #answer_characters vector later
      input <- unlist(strsplit(input, ""))
      return(input)
      
    } else {
      
      #the input was invalid (contained non-letters) the loop will re-prompt the user
      cat("ERROR: Guess must not contain numbers or symbols. Try again.")
      
    }
  }
}

#function prompts user for their guess (single letter)
get_letter_guess <- function() {
  
  #use loop to force the user to provide valid input
  repeat {
    
    #get user input and force to lowercase
    input <- tolower(readline("Enter a letter to guess: "))
    
    #ensure the input contains no non-letters and only 1 letter was inputted
    if(is_letters(input) & nchar(input) == 1) {
      
      return(input)
      
    } else {
      
      #the input contained non-letters or more than one letter, user will be reprompted
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

#'function takes the user's letter guess and returns an updated version of the user's
#'answer (blanks "_" are replaced with the guessed letter if applicable)
update_user_answer <- function(letter) {
  
  #check if the gussed letter is one of the letters in the answer
  if(letter %in% answer_characters) {
    
    #'since the letter is present in the answer, find which of the indexes in the answer
    #'match the gussed letter
    indexes <- which(answer_characters == letter)
    
    #update the user's answer by replacing blanks with the letter at the appropriate indexes
    user_answer[indexes] <- letter
    cat("Correct! The letter", letter, "is in the word!\n")
    
  } else {
    
    #the guessed letter was not in the word
    cat("Incorrect. The letter", letter, "is NOT in the word.\n")
  }
  
  #return the updated user_answer to be used in the main code
  return(user_answer)
  
}

#function checks if the user has won the game. Returns TRUE if the user won
check_for_win <- function(answer_to_check) {
  
  #check if the answer vector and the submitted vector are identical
  if(identical(answer_to_check, answer_characters)) {
    
    #the vectors match, so the user has won
    return(TRUE)
    
  } else {
    
    #the vectors did not match, the user has not won yet
    return(FALSE)
    
  }
}

###############
## MAIN CODE ##
###############

#read text file containing words into a vector
words <- readLines("words.txt")

#allow 20 guesses (guessing words or guessing letters comes out of the same pool of 20)
attempts_allowed <- 20

#randomly select one of the words from the vector to be used in the game
#sample.int() is used to select 1 random integer between 1 and the number of words in the words vector
#the random integer is used as an index number to pick a random word
answer <- words[sample.int(length(words), 1)]

#create vector containing the individual characters of the answer
answer_characters <- unlist(strsplit(answer, ""))

#determine the length of the word
answer_length <- nchar(answer)

#create a vector containing the user's currently filled-in letters (initially all blanks)
user_answer <- rep("_", times = answer_length)

#loop to be repeated for each attempt (in this case up to 20 times)
for (i in 1:attempts_allowed) {
  
  #print number of attempts remaining and the length of the word
  cat("You have", attempts_allowed - i + 1, "attempt(s) remaining to guess the", answer_length, "letter word.\n")
  
  #print the user's current answer (with blanks for characters not guessed)
  #using cat() with sep parameter to avoid printing quotation marks around each letter
  cat(user_answer, sep = " ", "\n")
  
  #determine if the user wants to guess the whole word or just one letter
  if (guess_type() == "w") {
    #the user wants to guess the entire word

    #use get_word_guess() function to obtain the word the user would like to guess
    word_guess <- get_word_guess()
    
    #check if the user guessed the word correctly
    if(check_for_win(word_guess)) {
      #the user guessed the word correctly
      
      #change user answer to match actual answer (this is important because the user_answer is later used to check a condition)
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
    
    #obtain the user's guess
    letter_guess <- get_letter_guess()
    
    #update the user's current answer based on their guess (i.e. replace the blanks if applicable)
    user_answer <- update_user_answer(letter_guess)
    
    #check if the user has won (i.e. all blanks have been filled)
    if(check_for_win(user_answer)) {
      #user has won
      
      #print the user's (correct) answer and let them know they won
      cat(user_answer, sep = " ", "\n")
      cat("YOU WIN!")
      
      #break the loop since the game is over
      break
      
    }
  }
}

#the loop is over, meaning all attempts have been used
#the user would have already been informed if they won, so check if they lost (i.e. check_for_win is false)

#check if the user has lost
if(!check_for_win(user_answer)) {
  #the user has lost
  
  #inform the user of their current answer, the correct answer, and inform them that the game is over.
  cat(user_answer, sep = " ", "\n")
  cat("GAME OVER.No attempts remaining. The word was:", answer)
}