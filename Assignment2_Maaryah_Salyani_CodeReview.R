# Assignment-2
# REVIEWED by Nishant Sarkar
setwd("~/Desktop/coding_course")
#I have just set my working directory

#' REVIEW: I would include a short comment at the beginning of the code that clarifies
#' what the code does. Ex. "A script that takes input numbers and determines whether
#' or not they are narcissistic."

three_digit <- as.numeric (readline(prompt = "enter a three digit positive number: "))

#I have prompted the user to enter a three digit positive number
# the readline function is used to read a line of text written by the user in the console

#' REVIEW: Good work using Readline here to get user input. A couple notes: Immediately
#' converting the readline input to a numeric doesn't allow the detection of non-numeric
#' inputs - there doesn't appear to be a check for whether the input is a number, and if 
#' I input letters, the code breaks. This could be ameliorated by checking if the input
#' is a number using is.numeric and as.numeric, then converting the input to an integer
#' after it passes those conditions. This could also be integrated into the if statement
#' below. For example: 
  #' if ((is.numeric(as.numeric(userinput))) &
  #'    (nchar(userinput) == 3) &  
  #'    (as.numeric(userinput) >= 0)) { 
#' Furthermore, our style guide says that there should not be a space between a function call 
#' and the opening paranthesis - in this case, the as.numeric() function.

# ifelse statement to check if number is positive and three digits in the first condition 
if (three_digit > 0 && nchar(three_digit) == 3) { 
  print("Value is a three-digit positive number.")
} else { # if number is not positive and 3 digits, then provide an error message and quit the session
  print("Error..terminating session")
  quit()
}

#' REVIEW: Good solution to check if the number is positive and three-digit using nchar.
#' As above, this section should include a check to see if the input is a number or not.

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

#' REVIEW: This is a great solution to split the input number into individual digits,
#' by using floor division by 100 to isolate the first digit, mod/floor division
#' to isolate the second, and mod to isolate the third. Another possible way to accomplish
#' this goal could be to use the strsplit function, which could split the input into
#' three digits and store them in a vector. For example:
  #' digits <- as.numeric(strsplit(three_digit, "")[[1]])

sum_of_cubes <- digit_1^3 + digit_2^3 + digit_3^3
# this will cube each of the separated digits individually, then sum them

# ifelse statement to check if three_digit is an Armstrong number
if (three_digit == sum_of_cubes) { #check if the three digit number = sum of the cubes
  print(paste(three_digit, "is a narcissistic number"))
} else {
  print(paste(three_digit, "is not a narcissistic number."))
}

#' REVIEW: Good work summing the individual digit cubes then comparing them to the
#' original input to determine if the output is an Armstrong number.
#' 
#' OVERALL COMMENTS: The code follows a logical structure and is well-commented. You
#' do a great job of noting down what each piece of code does, so it is easy to follow.
#' Excellent solution to splitting the input into individual digits, and calculating 
#' whether the input number is an Armstrong number. Variable names are descriptive and
#' it is easy to follow along with them. However, the code should include a check to make 
#' sure the user input is numeric: Task 2 in the assignment guidelines. Also, make sure 
#' to adhere to the style guide set out for this course. Overall, great work!