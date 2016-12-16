######################################################################################################
#                                   Dataset                                                          #
######################################################################################################

SD_and_RI_data <- read.csv("https://raw.githubusercontent.com/satkuma/CSCI-84/master/project/SD%20and%20RI%20Data.csv",
                           stringsAsFactors = FALSE)

correlation_df <- read.csv("https://raw.githubusercontent.com/satkuma/CSCI-84/master/project/correlation.csv",
                           stringsAsFactors = FALSE)

#South Dakota GDP Data
SD_GDP <- read.csv("https://raw.githubusercontent.com/satkuma/CSCI-84/master/project/South%20Dakota%20GDP2.csv")
library(plotly)

str(SD_and_RI_data)
SD_and_RI_data <- SD_and_RI_data[order(SD_and_RI_data$Year), ]
sd_voter_turnout <- sd_df[sd_df$Subject %in% c('Voter Turnout percent'),]
#Take only South Dakota Dataset
sd_df <- SD_and_RI_data[(SD_and_RI_data$State == 'South Dakota'),]
sd_df <- sd_df[with(sd_df, order(Year)),]

sd_democrat_votes <- sd_df[sd_df$Subject %in% c('Democrat vote percent'),]
sd_republican_votes <- sd_df[sd_df$Subject %in% c('Republican vote percent'),]

xax = list(title = "Year", tick0 = 2000, dtick = 4, ticklen = 5)

plot_ly(sd_democrat_votes, x = sd_democrat_votes$Year, y = sd_democrat_votes$percentage, 
        type = 'bar', name = 'Democracts', marker = list(color='skyblue')) %>%
  add_trace(y = sd_republican_votes$percentage, name = 'Republicans', marker = list(color = 'red')) %>% 
  add_trace(x = sd_voter_turnout$Year, y = sd_voter_turnout$percentage, type = 'scatter', 
            mode = 'lines', name = 'Voter Turn out', line = list(color = 'Green', width = 4)) %>% 
  layout(xaxis = xax, yaxis = list(title = "% of Votes"), 
         barmode = 'group', title = 'South Dakota Voting Pattern')

################################## South Dakota GDP Graph ##########################################################
plot_ly(SD_GDP, x = SD_GDP$DATE, y = SD_GDP$SDNGSP, type = 'scatter', mode = 'lines',
        line = list(color = 'red', width = 4)) %>%
  layout(xaxis = list(title = "Year"), yaxis = list(title = "GDP - Millions of Dollars"), title = "South Dakota GDP")


################################# South Dakota Voter Turnout by Age ################################################
voter_turnout <- sd_df[(sd_df$Subject %in% c('Voter Turnout percent') & sd_df$Year %in% c(2004,2008,2012)),]

voter_turnout18_29 <- sd_df[sd_df$Subject %in% c('Voter turnout percent-18_29'),]
voter_turnout30_39 <- sd_df[sd_df$Subject %in% c('Voter turnout percent-30_39'),]
voter_turnout40_49 <- sd_df[sd_df$Subject %in% c('Voter turnout percent-40_49'),]
voter_turnout50_59 <- sd_df[sd_df$Subject %in% c('Voter turnout percent-50_59'),]
voter_turnout60_69 <- sd_df[sd_df$Subject %in% c('Voter turnout percent-60_69'),]
voter_turnout70_plus <- sd_df[sd_df$Subject %in% c('Voter turnout percent-70_plus'),]

plot_ly(voter_turnout18_29, x = voter_turnout18_29$Year, y = voter_turnout18_29$percentage, 
        type = 'scatter', mode = 'lines+markers', name = 'Ages 18-29', marker = list(color='skyblue')) %>%
  add_trace(y = voter_turnout30_39$percentage, name = 'Ages 30-39', marker = list(color = 'blue')) %>% 
  add_trace(x = voter_turnout40_49$Year, y = voter_turnout40_49$percentage, type = 'scatter', 
            mode = 'lines+markers', name = 'Ages 40-49', line = list(color = 'Green', width = 4)) %>% 
  add_trace(y = voter_turnout50_59$percentage, name = 'Ages 50-59', marker = list(color = 'orange')) %>% 
  
  add_trace(x = voter_turnout60_69$Year, y = voter_turnout60_69$percentage, type = 'scatter', 
            mode = 'lines+markers', name = 'Ages 60-69', line = list(color = 'darkblue', width = 4)) %>% 
  add_trace(y = voter_turnout70_plus$percentage, name = 'Ages 70+', marker = list(color = 'yellow')) %>% 
  add_trace(y = voter_turnout$percentage, name = 'All Voters', line = list(color = 'blue', width = 4, dash = 'dash')) %>%
  layout(title = 'South Dakota Voter Turnout by Age group',
         xaxis = xax,
         yaxis = list(title = 'Voter turnout percent'))

################################# South Dakota Voter Turnout by Gender ################################################
voter_turnout_men <- sd_df[sd_df$Subject %in% c('Voter turnout percent-men'),]
voter_turnout_women <- sd_df[sd_df$Subject %in% c('Voter turnout percent-women'),]

voter_turnout_gender <- sd_df[(sd_df$Subject %in% c('Voter Turnout percent') & sd_df$Year %in% c(2000,2004,2008,2012)),]

plot_ly(voter_turnout18_29, x = voter_turnout_men$Year, y = voter_turnout_men$percentage, 
        type = 'scatter', mode = 'lines+markers', name = 'Men', marker = list(color='skyblue')) %>%
  add_trace(y = voter_turnout_women$percentage, name = 'Women', marker = list(color = 'pink')) %>% 
  add_trace(y = voter_turnout_gender$percentage, name = 'All Voters', line = list(color = 'blue', width = 4, dash = 'dash')) %>%
  layout(xaxis = xax, 
         yaxis = list(title = 'Voter turnout percent'), title = 'South Dakota Voter Turnout by Gender')

women_rep <- c(60, 47, 44,41)
women_dem <- c(39,47,55,41)
women_others <- c(1,2,1,5)

men_rep <- c(60, 56, 52,48)
men_dem <- c(38, 40, 45, 33)
men_others <- c(2, 4, 3, 9)
years <- c(2004, 2008, 2012, 2016)

women_df <- data.frame(women_rep, women_dem, women_others, years)
plot_ly(women_df, x = women_df$years, y = women_df$women_rep, 
        type = 'bar', name = 'Republican', marker = list(color='red')) %>%
  add_trace(y = women_df$women_dem, name = 'Democrats', marker = list(color = 'skyblue')) %>% 
  add_trace(x = women_df$years, y = women_df$women_others, name = 'Others', marker = list(color = 'Green')) %>% 
  layout(xaxis = xax, 
         yaxis = list(title = 'Percentage of Votes'), barmode = 'group', title = 'Voting Pattern for Women')

men_df <- data.frame(men_rep, men_dem, men_others, years)

plot_ly(men_df, x = men_df$years, y = men_df$men_rep, 
        type = 'bar', name = 'Republican', marker = list(color='red')) %>%
  add_trace(y = men_df$men_dem, name = 'Democrats', marker = list(color = 'skyblue')) %>% 
  add_trace(x = men_df$years, y = men_df$men_others, name = 'Others', marker = list(color = 'Green')) %>% 
  layout(xaxis = xax, 
         yaxis = list(title = 'Percentage of Votes'), barmode = 'group', title = 'Voting Pattern for Men')

################################# South Dakota Voter Turnout by Gender ################################################

voter_turnout_hispanic  <- sd_df[sd_df$Subject %in% c('Voter turnout percent-hispanic'),]
voter_turnout_white     <- sd_df[sd_df$Subject %in% c('Voter turnout percent-white'),]
voter_turnout_black         <- sd_df[sd_df$Subject %in% c('Voter turnout percent-black'),]

plot_ly(voter_turnout_hispanic, x = voter_turnout_hispanic$Year, y = voter_turnout_hispanic$percentage, 
        type = 'scatter', mode = 'lines+markers', name = 'Hispanic', marker = list(color='skyblue')) %>%
  add_trace(y = voter_turnout_white$percentage, name = 'White', marker = list(color = 'pink')) %>% 
  add_trace(y = voter_turnout_black$percentage, name = 'Blacks', marker = list(color = 'Black')) %>% 
  add_trace(y = voter_turnout$percentage, name = 'All Voters', line = list(color = 'blue', width = 4, dash = 'dash')) %>%
  layout(xaxis = xax, yaxis = list(title = 'Voter turnout percent'), title = 'South Dakota Voter Turnout by Race')

################################# South Dakota Voter Turnout by Education level ################################################

voter_turnout_no_high_school  <- sd_df[sd_df$Subject %in% c('Voter turnout percent-no_high_school'),]
voter_turnout_high_school     <- sd_df[sd_df$Subject %in% c('Voter turnout percent-high_school'),]
voter_turnout_college         <- sd_df[sd_df$Subject %in% c('Voter turnout percent-college'),]
voter_turnout_college_deg_ass <- sd_df[sd_df$Subject %in% c('Voter turnout percent-college_degree_associate'),]
voter_turnout_college_deg_bach <- sd_df[sd_df$Subject %in% c('Voter turnout percent-college_degree_bachelor'),]
voter_turnout_advanced        <- sd_df[sd_df$Subject %in% c('Voter turnout percent-advanced'),]

plot_ly(voter_turnout_no_high_school, x = voter_turnout_no_high_school$Year, y = voter_turnout_no_high_school$percentage, 
        type = 'bar', name = 'No High School', marker = list(color='red')) %>%
  add_trace(y = voter_turnout_high_school$percentage, name = 'High School', marker = list(color = 'skyblue')) %>% 
  add_trace(x = voter_turnout_college$Year, y = voter_turnout_college$percentage, name = 'College', marker = list(color = 'Green')) %>%
  add_trace(x = voter_turnout_college_deg_ass$Year, y = voter_turnout_college_deg_ass$percentage, name = 'College-Associate', marker = list(color = 'blue')) %>% 
  add_trace(x = voter_turnout_college_deg_bach$Year, y = voter_turnout_college_deg_bach$percentage, name = 'College-Bachelors', marker = list(color = 'orange')) %>%   
  add_trace(x = voter_turnout_advanced$Year, y = voter_turnout_advanced$percentage, name = 'College-Advanced', marker = list(color = 'grey')) %>% 
  layout(xaxis = xax, 
         yaxis = list(title = 'Percentage of Votes'), barmode = 'group', title = 'Voting Pattern by Education level')

#Data based on consolidated data
no_high_school_rep <- c(57,0)
high_school_rep <- c(60,59)
college_rep <- c(64,53)
college_bach_rep <- c(65,56)
advanced_rep <- c(45,46)

no_high_school_dem <- c(41,0)
high_school_dem <- c(38,39)
college_dem <- c(34,45)
college_bach_dem <- c(34,40)
advanced_dem <- c(54,52)

edu_years <- c(2004,2008)

edu_df <- data.frame(no_high_school_rep, high_school_rep, edu_years)
plot_ly(edu_df, x = edu_years, y = no_high_school_rep, 
        type = 'bar', name = 'No High School', marker = list(color='red')) %>%
  add_trace(y = high_school_rep, name = 'High School', marker = list(color = 'skyblue')) %>% 
  add_trace(y = college_rep, name = 'College', marker = list(color = 'Green')) %>%
  add_trace(y = college_bach_rep, name = 'College-Bachelors', marker = list(color = 'orange')) %>%   
  add_trace(y = advanced_rep, name = 'College-Advanced', marker = list(color = 'grey')) %>% 
  layout(xaxis = xax, yaxis = list(title = 'Percentage of Votes'), barmode = 'group', title = 'Voting Pattern by Education-Republicans')

plot_ly(edu_df, x = edu_years, y = no_high_school_dem, 
        type = 'bar', name = 'No High School', marker = list(color='red')) %>%
  add_trace(y = high_school_dem, name = 'High School', marker = list(color = 'skyblue')) %>% 
  add_trace(y = college_dem, name = 'College', marker = list(color = 'Green')) %>%
  add_trace(y = college_bach_dem, name = 'College-Bachelors', marker = list(color = 'orange')) %>%   
  add_trace(y = advanced_dem, name = 'College-Advanced', marker = list(color = 'grey')) %>% 
  layout(xaxis = xax, yaxis = list(title = 'Percentage of Votes'), barmode = 'group', title = 'Voting Pattern by Education-Democrats')

################################# South Dakota Voter Turnout (General)################################################

plot_ly(sd_voter_turnout, x = sd_voter_turnout$Year, y = sd_voter_turnout$percentage, 
        type = 'bar', name = 'Republicans', marker = list(color='red')) %>%
  layout(xaxis = xax, yaxis = list(title = 'Voter turnout percent'), barmode = 'group', title = 'Voting turnout over the years')

################################# South Dakota Third Party Candidate Vote Share################################################


third_party_votes <- c(22.13, 11.33, 2.49, 1.71, 0, 2.14, 5.72)
third_party_years <- c(1992, 1996,2000,2004, 2008,2012, 2016)

thirdparty_df <- data.frame(third_party_votes, third_party_years)

plot_ly(thirdparty_df, x = third_party_years, y = third_party_votes, 
        type = 'bar', name = 'Third Party Votes', marker = list(color='green')) %>%
  layout(xaxis = list(title = "Year", tick0 = 1992, dtick = 4, ticklen = 7), 
         yaxis = list(title = 'Percentage of Votes'), barmode = 'group', 
         title = 'Vote Share for 3rd Party Candidates')


################################# South Dakota Correlation between Voter turnout and Margin################################################

plot(correlation_df$Voter_turnout_percent, correlation_df$dem_percent, 
     xlab="Voter Turn Out %",
     ylab="Democrat Vote Share %", 
     main="Voting Pattern by County 1992-2012\nDemocrats - South Dakota", pch=16, 
     col = "skyblue",
     frame.plot=FALSE)

#By visual inspection of the plot, we could see that if more people turn out to vote, democrats 
#receive less votes
dem_model <- lm(correlation_df$dem_percent ~ correlation_df$Voter_turnout_percent)
abline(dem_model, col = "skyblue3", lwd = 2)

plot(correlation_df$Voter_turnout_percent, correlation_df$rep_percent, 
     xlab="Voter Turn Out %",
     ylab="Republican Vote Share %", 
     main="Voting Pattern by County 1992-2012\nRepublicans - South Dakota", pch=16, 
     col = "firebrick3",
     frame.plot=FALSE)

#By visual inspection - More voters - more votes for republicans
rep_model <- lm(correlation_df$rep_percent ~ correlation_df$Voter_turnout_percent)
abline(rep_model, col = "firebrick3", lwd = 2)


######################################################################################################
#                                   2020 Election Predictions                                        #
#                                   Method: Monte Carlo Simulations                                  #
#                                   100,000 Simulations                                              #
######################################################################################################

sd_rep_avg <- sd_df[grep("^Avg Rep Vote Share", sd_df$Subject),]
sd_dem_avg <- sd_df[grep("^Avg Dem Vote Share", sd_df$Subject),]
sd_counties <- sd_df[with(sd_df, sd_df$Year == 0),1] 

median = {}
low = {}
high = {}

for (i in 1:length(sd_dem_avg$percentage)){
  p.state=rdirichlet(100000,
                     500*
                       c(sd_rep_avg$percentage[i],sd_dem_avg$percentage[i],
                         100-sd_rep_avg$percentage[i] - sd_dem_avg$percentage[i])/100+1)
  median[i] = median(p.state[,1])
  quantile = unname(quantile(p.state[,1], prob=c(.025,.975)))
  low[i] = quantile[1]
  high[i] = quantile[2]
}

results = data.frame("Simulation_Median" = median * 100.0, "Confidence_Interval_Low" = low * 100, 
                     "Confidence_Interval_High" = high*100, 
                     row.names = sd_counties)
View(results)

#####State Averages for elections 2000-2016#####
rep_vote_percent = 60.72
dem_vote_percent = 36.86

p.state=rdirichlet(100000,
                   500*
                     c(rep_vote_percent,dem_vote_percent,100-rep_vote_percent-dem_vote_percent)/100+1)

hist(p.state[,1], nclass=1000, main=paste('Simulation for South Dakota\nPresidential Elections 2020'), 
     col = 'red', xlab='Simulated Proportion of Vote for Republicans')
abline(v=median(p.state[,1]), lwd=3, col=3)
abline(v=quantile(p.state[,1], prob=c(.025,.975)), col=2)