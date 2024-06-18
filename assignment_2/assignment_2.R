#'Assignment 2, June/13/2024
#'Ibrahim Emam
#'R version: 4.4.0

#get input from user, save as a character string
number_string <- readline(prompt = "Enter a three digit positive number: ")

#make a numeric object containing the user's input
#"NAs introduced by coercion" warning is suppressed because this possibility is handled later on
number_numeric <- suppressWarnings(as.numeric(number_string))

#check if the number is NA, as this would be the result of a non-numeric input
if(!is.na(number_numeric)) {
  
  #if the input was numeric (i.e. not NA), ensure the number is 3 digits, positive, and whole.
  if(number_numeric > 100 & number_numeric < 1000 & number_numeric %% 1 == 0) {
    
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
  } else {
    
    #the input of the user was numeric but not a whole, positive number with 3 digits
    print("Input must be a positive whole number with 3 digits.")
  }
} else {
  
  #the input was not numeric and therefore got converted to NA
  print("Input must be numeric.")
}