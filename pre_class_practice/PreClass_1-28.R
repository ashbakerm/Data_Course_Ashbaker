mtcars

##1- what type of object is this?
str(mtcars)
class(mtcars)
 = 'data.frame'
typeof(mtcars)
 = 'list'

##2- find 4 cylinders cars with a mpg greater than 20, and save them to a new object
cyl4_mpg20_cars <- mtcars[mtcars$cyl == 4 & mtcars$mpg > 20, ]
cyl4_mpg20_cars

##3- convert mpg to a character data type
mtcars$mpg <- as.character(mtcars$mpg)
class(mtcars$mpg)

##4- convert the entire data frame to character data type
mtcars_character <- data.frame(lapply(mtcars, as.character))
mtcars_character
str(mtcars_character)

write.csv(mtcars_character, 'class_practice_28Jan25.csv')



