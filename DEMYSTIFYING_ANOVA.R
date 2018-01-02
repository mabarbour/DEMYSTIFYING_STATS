
## GOALS ----

# WHY WOULD YOU USE THIS?
# We want to test whether groups vary in their response.

# ANOVA will work well if the mean is a good description of the center of each group's response. 
# ANOVA calculates the mean for each group
# 1. Calculate the mean of all the responses. Let's call this the "grand mean"
# 2. Calculate the mean response of each group. Let's call these the "group means"
# 3. Calculate the difference between each group mean and the grand mean. Square these differences and add them all up. We'll call these the "Group Sum of Squares". Note that this is the numerator of the "Group Variance".
# 4. Calculate the difference between each response and its corresponding group mean. Square these differences and add them all up. We'll call these the "Residual Sum of Squares". Note that this is the numerator of the "Residual Variance". 
# 5. Calculate the "Mean Group Sum of Squares". This is actually the same as the "Between Group Variance". Divide the "Group Sum of Squares" by the "Group DF".
# 6. Calculate the "Mean Residual Sum of Squares". This is actually the same as the "Within Group Variance". Divide the "Residual Sum of Squares" by the "Residual DF".
# 
# ANOVA does this by calculating the

# In this example, groups  
# Possible Analyses: Analysis of Variance (ANOVA), Two-Sample t-test

# WHY IS IT CALLED ANOVA?


## LOAD REQUIRED LIBRARIES ----

library(dplyr)
library(ggplot2)
library(cowplot)

## LOAD DATA
data(iris)
grand.mean_Petal.Length <- mean(iris$Petal.Length)

species.means <- iris %>% group_by(Species) %>% summarise(mean_Petal.Length = mean(Petal.Length)) # get mean response for each factor level

setosa.mean_Petal.Length <- mean(filter(iris, Species == "setosa")$Petal.Length)
versicolor.mean_Petal.Length <- mean(filter(iris, Species == "versicolor")$Petal.Length)
virginica.mean_Petal.Length <- mean(filter(iris, Species == "virginica")$Petal.Length)

iris.new <- left_join(iris, species.means) %>% 
  mutate(grand.mean_Petal.Length = grand.mean_Petal.Length,
         grand.diff_Petal.Length = Petal.Length - grand.mean_Petal.Length,
         mean.diff_Petal.Length = Petal.Length - mean_Petal.Length,
         jitter_x.axis = runif(dim(iris)[1]))

Max_Sum.of.Squares <- sum(iris.new$grand.diff_Petal.Length^2)
Species_Sum.of.Squares <- sum()

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(iris.new, aes(x = jitter_x.axis, y = Petal.Length)) +
  geom_segment(aes(yend = grand.mean_Petal.Length, xend = jitter_x.axis), linetype = "dotted") +
  geom_jitter(size = 2) + 
  geom_hline(yintercept = grand.mean_Petal.Length) +
  xlab("") + scale_x_continuous(labels = NULL)

ggplot(iris, aes(y = Petal.Length, x = jitter_x.axis, group = Species, color = Species)) + 
  geom_jitter(size = 2) +
  geom_hline(yintercept = setosa.mean_Petal.Length, color = "red") +
  geom_hline(yintercept = versicolor.mean_Petal.Length, color = "green") +
  geom_hline(yintercept = virginica.mean_Petal.Length, color = "blue") +
  xlab("") + scale_x_continuous(labels = NULL)


anova(lm(Petal.Length ~ Species, iris))

summary(lm(Petal.Length ~ Species - 1, iris))
sd(coef(lm(Petal.Length ~ Species - 1, iris)))
# interesting that this doesn't correspond, even though this is how fixed-effects variance is calculated by Schielzeth and Nakagawa.
sd(as.vector(coef(lm(Petal.Length ~ Species, iris, contrasts = list(Species="contr.sum"))) %*% t(model.matrix(lm(Petal.Length ~ Species, iris, contrasts = list(Species="contr.sum"))))))

#var(as.vector(fixef(mF) %*% t(mF@X)))

library(lme4)
summary(lmer(Petal.Length ~ (1|Species), iris))
sd(ranef(lmer(Petal.Length ~ (1|Species), iris))$Species[ ,"(Intercept)"]) # interesting...the SD(conditional modes) corresponds to the SD of the random effects, which is what I expected.
table(iris$Species) # note equal sample sizes, which may make the SD of conditional modes correspond more closely to random effect SD.


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

## come up with a visualization where the colors designate groups all within the same set of points. See if that is helpful.

variance_betweengroups <- ggplot(group.means, aes(x = x, y = y.mean)) + 
  geom_point(color = "red") + 
  geom_hline(yintercept = grand.mean, linetype = "dotted") +
  geom_segment(aes(yend = y.mean, y = grand.mean, xend = x, x = x), color = "red") +
  scale_y_continuous(limits = range(y)) + 
  ylab("y")

variance_withingroups <- ggplot(df, aes(x = x.jitter, y = y)) + 
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
  
df.full <- left_join(df, group.means) %>% 
  mutate(grand.mean = grand.mean, 
         resid.mean = y.mean - grand.mean, 
         squares.mean = resid.mean^2, 
         resid = y - y.mean, 
         squares.resid = resid^2)

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
