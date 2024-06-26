#'Assignment 3, June/26/2024
#'Ibrahim Emam
#'R version: 4.4.0

#FUNCTIONS DEFINED:

guess_type <- function() {
  repeat {
    input <- tolower(readline("Would you like to guess the word or a letter? (word/letter): "))
    if (input == "word" | input == "letter") {
      return(input)
    }
  }
}

#read txt file containing words into a vector
words <- readLines("words.txt")

#assign number of attempts allowed
attempts_allowed <- 10

#randomly select one of the words from the vector to be used in the game
answer <- words[sample.int(length(words), 1)]

#create vector containing individual characters of the answer
answer_characters <- unlist(strsplit(answer, ""))

#determine the length of the word
answer_length <- nchar(answer)

#create a vector containing the user's currently filled-in letters (initially blank)
user_answer <- rep("_", times = answer_length)

for (i in 1:attempts_allowed) {
  
  #print current user answer and number of attempts remaining
  cat("You have", attempts_allowed - i + 1, "attempts remaining to guess the", answer_length, "letter word.\n")
  
  #print the user's current answer (with blanks for characters not guessed)
  cat(user_answer, sep = " ", "\n")
  
  #determine if the user wants to guess the whole word or just one letter
  if (guess_type() == "word") {
    #the user wants to guess the entire word
    print("You picked WORD!")
  } else {
    #the user wants to guess one letter
    print("You picked LETTER!")
  }
}