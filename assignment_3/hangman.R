#'Assignment 3, June/26/2024
#'Ibrahim Emam
#'R version: 4.4.0

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
  
  #prompt the user to determine if they will guess a character or the whole word
  
}
