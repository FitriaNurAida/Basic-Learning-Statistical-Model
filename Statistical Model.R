d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

### calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

### generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

### Calculating the spread of combined polls
library(dplyr)
d_hat <- polls %>% summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>% .$avg
d_hat
p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size)); moe   
round(d_hat*100,1)
round(moe*100, 1)

### Generating simulated poll data
library(dslabs)
data("polls_us_election_2016")
names(polls_us_election_2016)
#p is the proportion voting for Clinton
#(1-p) is the proportion voting for Trump
#d is the difference of the proportion voting, named spread.
# keep only national polls from week before election with a grade considered reliable :
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))
head(polls)
# add spread estimate :
polls <- polls %>% mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
head(polls)
# compute estimated spread for combined polls :
d_hat <- polls %>% summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>% .$d_hat
d_hat
# compute margin of error
p_hat <- (d_hat+1)/2 ; p_hat
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize)) ; moe #moe = Margin of Error
# histogram of the spread
library(ggplot2)
polls %>% ggplot(aes(spread)) + geom_histogram(color="black", binwidth = .01)

### Investigating poll data and pollster bias
# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())
# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

### Data Driven Model
# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()
# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)
# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)


### Exercise 1 Heights Revisited
library(dslabs)
data(heights)
head(heights)
x <- heights %>% 
  filter(sex == "Male") %>%
  .$height
head(x)
avg <- mean(x, na.rm=TRUE)
avg
stdev <- sd(x, na.rm = TRUE)
stdev

### Exercise 2 Sample the population of heights
head(x)
set.seed(1)
N <- 50
X <- sample(x, size=N, replace=TRUE)
avg_s <- mean(X, na.rm = TRUE)
avg_s
stdev_s <- sd(X, na.rm = TRUE)
stdev_s

### Exercise 3 Confidence Interval Calculation
head(x)
set.seed(1)
N <- 50
X <- sample(x, size=N, replace=TRUE)
se <- sd(X, na.rm=TRUE)/sqrt(N)
se
z <- qnorm(0.975)
lower <- mean(X, na.rm=TRUE)-(z*se)
ci <- c(lower,mean(X, na.rm=TRUE)+(z*se))
ci

### Exercise 4 Monte Carlo Simulation for Heights
mu <- mean(x)
set.seed(1)
N <- 50
B <- 10000
res <- replicate(B,{
  X <- sample(x, size=N, replace=TRUE)
  z <- qnorm(0.975)
  se <- sd(X, na.rm=TRUE)/sqrt(N)
  interval <- c(mean(X, na.rm=TRUE)-(z*se),mean(X)+(z*se))
  between(mu,interval[1], interval[2])
})
mean(res)

### Exercise 5 Visualizing Polling Bias
data("polls_us_election_2016")

polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

polls %>% group_by(pollster) %>%
  ggplot(aes(x=pollster, y=spread)) + geom_boxplot() + geom_point()

### Exercise 6 Compute the Estimates
head(polls)
sigma<-polls %>% group_by(pollster)%>% summarize(s=sd(spread))
sigma

### Exercise 7 Calculate the 95% Confidence Interval of the Spreads
head(polls)
res <- polls %>% group_by(pollster) %>% 
  summarize(avg=mean(spread), stdev=sd(spread), n=n())
res
estimate <- res$avg[2]-res$avg[1]
estimate
se_hat <- sqrt(res$stdev[1]^2/res$n[1] + res$stdev[2]^2/res$n[2])
se_hat
z <- qnorm(0.975)
lower <- estimate-z*se_hat
ci <- c(lower, estimate+z*se_hat)
ci

### Exercise 8 Calculate p-value
head(polls)
res <- polls %>% group_by(pollster) %>% 
  summarize(avg=mean(spread), stdev=sd(spread), n=n())
res
estimate <- res$avg[2]-res$avg[1]
estimate
se_hat <- sqrt(res$stdev[1]^2/res$n[1] + res$stdev[2]^2/res$n[2])
se_hat
z <- estimate / se_hat
pval <- 2*(1-pnorm(z))
pval

### Exercise 9 Within-Poll and Between-Poll Variability
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

var <- polls %>% group_by(pollster) %>% summarize(avg=mean(spread), s=sd(spread))
var