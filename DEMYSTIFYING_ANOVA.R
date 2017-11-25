
## LOAD REQUIRED LIBRARIES ----

library(dplyr)
library(ggplot2)
library('cowplot')


## SET PARAMETERS ----

n <- 100 # sample size
y <- rnorm(n) # generate random responses from a normal distribution
groups <- LETTERS[1:2] # number of groups
x <- as.factor(rep_len(groups, n)) # factor variable

stdev <- 0.05
x.jitter <- as.numeric(x) + rnorm(n, sd = stdev)
df <- data.frame(y, x, x.jitter) # create data frame


group.means <- df %>% group_by(x) %>% summarise(y.mean = mean(y)) # get mean response for each factor level
grand.mean <- mean(y)

df.full <- left_join(df, group.means) %>% 
  mutate(grand.mean = grand.mean, 
         resid.mean = y.mean - grand.mean, 
         squares.mean = resid.mean^2, 
         resid = y - y.mean, 
         squares.resid = resid^2)

variance_betweengroups <- ggplot(group.means, aes(x = x, y = y.mean)) + 
  geom_point(color = "red") + 
  geom_hline(yintercept = grand.mean, linetype = "dotted") +
  geom_segment(aes(yend = y.mean, y = grand.mean, xend = x, x = x), color = "red") +
  scale_y_continuous(limits = range(y)) + 
  ylab("y")

variance_withingroups <- ggplot(df.full, aes(x = x.jitter, y = y)) + 
  geom_point() + 
  geom_linerange(aes(ymin = y.mean, ymax = y)) +
  geom_errorbarh(aes(xmin = as.numeric(x) - stdev*3, 
                     xmax = as.numeric(x) + stdev*3, 
                     y = y.mean),
                 height = 0,
                 color = "red",
                 linetype = "solid") +
  scale_x_continuous(name = "x", breaks = c(1,2), labels = c("A","B"), limits = c(0.5,2.5)) 

plot_grid(variance_betweengroups, variance_withingroups)
  

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
