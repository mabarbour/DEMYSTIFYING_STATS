n <- 100
y <- rnorm(n) 
groups <- LETTERS[1:2]

x <- rep_len(groups, n)
library(dplyr)

df <- data.frame(y, x)
means <- df %>% group_by(x) %>% summarise(mean.y = mean(y))

df <- left_join(df, means) %>% mutate(grand.mean = mean(y), resid.mean = grand.mean - mean.y, resid.meanSq = resid.mean^2, resid = mean.y - y, Squares = resid^2)
sum(df$resid.meanSq)
SumOfSquares <- sum(df$Squares)
SumOfSquares
resid.DF <- n - length(groups)
Mean_Square <- SumOfSquares/resid.DF
Mean_Square

anova(lm(y~x))