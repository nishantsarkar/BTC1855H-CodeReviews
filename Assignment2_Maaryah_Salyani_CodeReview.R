# Assignment-2
setwd("~/Desktop/coding_course")
#I have just set my working directory

three_digit <- as.numeric (readline(prompt = "enter a three digit positive number: "))

#I have prompted the user to enter a three digit positive number
# the readline function is used to read a line of text written by the user in the console

# ifelse statement to check if number is positive and three digits in the first condition 
if (three_digit > 0 && nchar(three_digit) == 3) { 
  print("Value is a three-digit positive number.")
} else { # if number is not positive and 3 digits, then provide an error message and quit the session
  print("Error..terminating session")
  quit()
}
# Check if the number is an Armstrong number
# will first create variables to extract the individual digits from the three digit number, then sum the cube of each number
digit_1 <- three_digit %/% 100
# separated the first digit (100s placeholder) from the three digit number by dividing it by 100
# the left and right operand return number as an integer
digit_2 <- (three_digit %% 100) %/% 10
#separated the second digit (10s) by diving the remainder of the three digit number when divided by 100
# divided the remainder by 10 to extract the 2nd digit in the 10s placeholder
digit_3 <- three_digit %% 10
#separated the last digit (ones digit) from the three digit number by using the operand to give the remainder when the number is divided by 10 

sum_of_cubes <- digit_1^3 + digit_2^3 + digit_3^3
# this will cube each of the separated digits individually, then sum them

# ifelse statement to check if three_digit is an Armstrong number
if (three_digit == sum_of_cubes) { #check if the three digit number = sum of the cubes
  print(paste(three_digit, "is a narcissistic number"))
} else {
  print(paste(three_digit, "is not a narcissistic number."))
}
