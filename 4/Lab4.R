library(gtools)

#---------------------------------------------------------------------
# Year      |Democrat Vote % | Republican Vote % | Difference in Vote
#---------------------------------------------------------------------
# 1992      | 37.14          | 40.66             | 3.52
# 1996      | 43.03          | 46.49             | 3.46
# 2000      | 37.56          | 60.30             | 22.74
# 2004      | 38.44          | 59.91             | 21.47
# 2008      | 44.75          | 53.16             | 8.41
# 2012      | 39.87          | 61.53             | 18.02

#Average
dem_vote_percent = 30
rep_vote_percent = 65.56
size = 546

## Function to simulate each state outcome
p.win = function(){
  #Dirichlet distribution because there can be multiple candidates
  p=rdirichlet(1000000,
               size*c(rep_vote_percent,dem_vote_percent,(100-rep_vote_percent-dem_vote_percent))/100+1)
  mean(p[,2]>p[,1])
}

size = 1000


p.state=rdirichlet(100000,
                   size*
                     c(rep_vote_percent,dem_vote_percent,100-rep_vote_percent-dem_vote_percent)/100+1)


hist(p.state[,1], nclass=1000, main=paste('Simulation for South Dakota\nPresidential Elections 2016'), 
     col = 'red', xlab='Simulated Proportion of Vote for Republicans')
abline(v=median(p.state[,1]), lwd=3, col=3)
abline(v=quantile(p.state[,1], prob=c(.025,.975)), col=2)

median(p.state[,1])

summary(p.state[,1])
margin_of_votes <- c(3.94, 4.34, 26.07, 22.26, 11.57, 21.30)
year <- c(1992, 1996, 2000, 2004, 2008, 2012)

margin_df <- data.frame(margin_of_votes, year)

plot(margin_df$year, margin_df$margin_of_votes, 
     xlab="Election Year",
     ylab="Republican Victory Margin %", 
     main="Victory Margin Trend 1992-2012", pch=16,
     xlim = c(1988, 2016),
     col = "firebrick3")

#By visual inspection of the plot, we could see that if more people turn out to vote, democrats 
#receive less votes
rep_model <- lm(margin_df$margin_of_votes ~ margin_df$year)
abline(rep_model, col = "firebrick3", lwd = 2)

#Vote share of republicans in % from 1992-2012
rep_percent <- c(40.9, 46.5, 61.79, 60.25, 54.62, 59.58)
#Election years
year <- c(1992, 1996, 2000, 2004, 2008, 2012)

rep_df <- data.frame(rep_percent, year)

plot(rep_df$year, rep_df$rep_percent, 
     xlab="Election Year",
     ylab="% of Votes", 
     main="Trend for Republican Voteshare\nSouth Dakota", pch=16,
     xlim = c(1988, 2016),
     ylim = c(35, 70),
     col = "firebrick3")

#By visual inspection of the plot, we could see that if more people turn out to vote, democrats 
#receive less votes
rep_model <- lm(rep_df$rep_percent ~ rep_df$year)
abline(rep_model, col = "firebrick3", lwd = 2)

predict(rep_model, data.frame(rep_df$year == c(2016)))

#Using linear regression to fit the model
rep_model <- lm(rep_percent ~ year)
#Predict for Year 2016
y = predict(lm(rep_percent ~ year), data.frame(year = c(2016)))

print (paste('Predicted Vote share for republicans in 2016 Elections: ', predict(lm(rep_percent ~ year), data.frame(year = c(2016)))))

dem_votes <- c(37.14, 43.03, 37.56, 38.44, 44.75, 39.87)
predict(lm(dem_votes ~ year), data.frame(year = c(2016)))
