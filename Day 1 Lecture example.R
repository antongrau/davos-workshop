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

# Subsetting vectors
char
char[1]
char[2:3]
log
which(log)
char[log]

# Subsetting data.frames - rows
d
d[1,]
d[c(1,3),]

# Subsetting data.frames - columns
d
d[, 2]
d[, -1]

# Subsetting data.frames - names
d
d$num
d[, c("log", "char")]

# Pipes
1:5 %>% sum()

1:4 %>% cbind(d, .)


