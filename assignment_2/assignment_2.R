#'Assignment 2, June/13/2024
#'Ibrahim Aly Emam
#'R version: 4.4.0

#get input from user
number_string <- readline(prompt = "Enter a three digit positive number: ")

#make a numeric object with the user's input
number_numeric <- as.numeric(number_string)

#'if the user did not provide input or the input was not numeric, print an error
#'message and quit
if(is.na(number_numeric)) {
  print("Error: input must be numeric.")
  #quit the program?
}





