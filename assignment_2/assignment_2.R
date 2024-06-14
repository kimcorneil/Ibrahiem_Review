#'Assignment 2, June/13/2024
#'Ibrahim Emam
#'R version: 4.4.0

#get input from user
number_string <- readline(prompt = "Enter a three digit positive number: ")

#make a numeric object containing the user's input
number_numeric <- as.numeric(number_string)

#'if the user did not provide input or the input was not numeric, print an error
#'message and quit
if(is.na(number_numeric) | number_numeric > 999 | number_numeric %% 1 != 0 | number_numeric < 0) {
  stop("Error: input must be a whole number with 3 digits.")
}

#split the number into its individual digits, then change from list to vector format
digits <- unlist(strsplit(number_string, ""))

#convert the digits vector from character to numeric
digits <- as.numeric(digits)

#cube each digit in the vector then sum the results
cube_sum <- sum(digits^3)

#check if the number entered by the user is equal to the sum of the digit's cubes
if(cube_sum == number_numeric) {
  cat(number_numeric, "is an Armstrong number.")
} else {
  cat(number_numeric, "is NOT an Armstrong number.")
}
