#RMED901 Second Day----
#Date:220905

#Author:Zahra----




#task2:A code to multiply numbers----

a<-5
b<-8
x<-5*8
x
#
#
#task3:A code that does not run because of wrong object definition----

1a<-5
b<-8
z<-1a*b
z

#Creating a vector----
c(1, 2, 3)

my_vector<- c("a", "b")
my_vector



#task4:Using the function "round"----

pi<-3.14159265359
round(pi, digits=2)#you can skip the argument name so
round (pi, 2)


#task5:Another example of wrong value assignment----

word2<- Hello
#show it in all lower case
tolower(word2)
#show it in all upper case
toupper(word2)

#find argument for "rnorm" function----
args(rnorm)
y<-rnorm (100, 100, 15)
y
mean(y)
sd(y)
#head()shows the first six values
#tail()shows the last six values
head(y)
tail(y)
#now that y is fixed, everytime you run this function, the numbers are same

head (y)
#but if you repeat the line bellow, every time you get different numbers
head(rnorm(100, 100, 15))

#now set.seed()function fixes this
set.seed(123)
head(rnorm(n=100, mean=100, 15))



#task 6:Mean and sd functions in a vector----


X<- C(3, 6, 2)
mean(x)
sd(x)

mean(3, 6, 2, 10)
sd(3, 6, 2, 10)
#you get error message because you need to define an array of numbers with c() function



#task7:Creating a range of values and doing some mathematical changes----
1:45-2
2*6:14
(2*6):14








