# Assign objects
x <- 1:5
x
y <- x
y
x <- y + 1

# Functions
mean(x, na.rm =TRUE)
mean
plot(x)

# Vectors
num   <- c(1, 10, 25, 1000)
char  <- c("cat", "dog", "cow", "cat")
log   <- c(TRUE, FALSE, FALSE, TRUE)

c(num, char)
num * 2
log * 2
num * log

# factors
as.factor(char)
dput(as.factor(char))
as.numeric(as.factor(char))

# Recoding factors
library(forcats)
fact      <- as_factor(char)
fct_recode(fact, cow = "dog", "Chickens and hens" = "cat")

# Matrixes
M <- matrix(1:12, ncol = 4)
M
M + 10

rbind(M, char)

cbind(M, log)

# Data.frames
d <- data.frame(num, char, log)
d
d + 1

# Lists
L <- list(char, num, M, data = d)
L

c(L, fct_recode)

# Subsetting
char[1]
num[1:2]
char[log]
which(log)




