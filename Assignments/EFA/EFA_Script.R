
install.packages("psych")
library("psych")

#Import dataset big5.csv
Personality_data <- read.csv("big5.csv") 

View(Personality_data)

#Correlation matrix cor(). Create a correlation matrix for the dataset

cor_matrix <- cor(na.omit(Personality_data))

#Visual inspection cor.plot()
cor.plot(cor_matrix)

#Eigenvalues eigen(). 
eigen_values <- eigen(cor_matrix)

#Scree plot scree() & parallel analysis fa.parallel()
scree(cor_matrix, factors=FALSE)

fa.parallel(Personality_data, fa="fa")
# fa.parallel(Personality_data)

#Exploratory Factor Analysis fa(). Perform factor analysis on the dataset
Personality_6FA <- fa(cor_matrix, 6)

print(Personality_6FA)
# comm tells us how many latent constructs that explain-- above 1, a sign that cross loading...
#we can look at different dimmensions
Personality_6FA <- fa(cor_matrix, 5)
print(Personality_6FA)

# we want above .3
print(Personality_6FA$loadings, cutoff=.3, sort=TRUE)









