
## LOAD REQUIRED LIBRARIES ----

library(dplyr)
library(ggplot2)


## SET PARAMETERS ----

n <- 100 # sample size
y <- rnorm(n) # generate random responses from a normal distribution
groups <- LETTERS[1:2] # number of groups
x <- rep_len(groups, n) # factor variable
df <- data.frame(y, x) # create data frame

group.means <- df %>% group_by(x) %>% summarise(y.mean = mean(y)) # get mean response for each factor level
grand.mean <- mean(y)

df.full <- left_join(df, group.means) %>% 
  mutate(grand.mean = grand.mean, 
         resid.mean = y.mean - grand.mean, 
         squares.mean = resid.mean^2, 
         resid = y - y.mean, 
         squares.resid = resid^2)

ggplot(group.means, aes(x = x, y = y.mean)) + 
  geom_point() + 
  geom_hline(yintercept = grand.mean)

ggplot(df.full, aes(x = x, y = y)) + 
  geom_jitter(width = 0.2) + 
  geom_hline(yintercept = grand.mean) +
  geom_line(aes(x = x, y = resid.mean))

## ANOVA with R ----

anova(lm(y~x))


## ANOVA manually ----

group.DF <- length(groups) - 1
resid.DF <- n - group.DF - 1

group.Sum_Squares <- sum(df.full$squares.mean)
resid.Sum_Squares <- sum(df.full$squares.resid)

group.Mean_Squares <- group.Sum_Squares/group.DF
resid.Mean_Squares <- resid.Sum_Squares/resid.DF

F_ratio <- group.Mean_Squares/resid.Mean_Squares
P_value <- 1 - pf(q = F_ratio, df1 = group.DF, df2 = resid.DF)
