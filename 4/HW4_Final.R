#South Dakota historical data by Age, Gender, Race and voter turnout
SD_and_RI_data <- read.csv("https://raw.githubusercontent.com/satkuma/CSCI-84/master/SD%20and%20RI%20Data.csv")

#South Dakota GDP Data
SD_GDP <- read.csv("https://raw.githubusercontent.com/satkuma/CSCI-84/master/South%20Dakota%20GDP2.csv")
library(plotly)

str(SD_and_RI_data)
SD_and_RI_data <- SD_and_RI_data[order(SD_and_RI_data$Year), ]

sd_df <- SD_and_RI_data[(SD_and_RI_data$State == 'South Dakota'),]
ri_df <- SD_and_RI_data[(SD_and_RI_data$State == 'Rhode Island'),]

ri_turnout <- ri_df$population_percent[ri_df$Subject == 'Voter Turnout percent']
sd_turnout <- sd_df$population_percent[sd_df$Subject == 'Voter Turnout percent']

levels(sd_df$Subject)

###############South Dakota Data############################################

#Young people - 15-34 years
sd_young <- sd_df[sd_df$Subject %in% c('15 to 19 years','20 to 24 years','25 to 34 years'),]

#Middle aged - 35-44 years
sd_middle <- sd_df[sd_df$Subject %in% c('35 to 44 years'),]

#Old age - Above 45 Years
sd_old <- sd_df[sd_df$Subject %in% c('45 to 54 years','55 to 59 years', '60 to 64 years', '65 to 74 years', '75 to 84 years', '85 years and over'),]

sd_voter_turnout <- sd_df[sd_df$Subject %in% c('Voter Turnout percent'),]
sd_total_pop <- sd_df[ri_df$Subject %in% c('Total population'),]

sd_young_pop <- aggregate(sd_young$population_percent, by=list(Year=sd_young$Year), FUN=sum)
sd_middle_pop <- aggregate(sd_middle$population_percent, by=list(Year=sd_middle$Year), FUN=sum)
sd_old_pop <- aggregate(sd_old$population_percent, by=list(Year=sd_old$Year), FUN=sum)

xax = list(tick0 = 2000, dtick = 4, ticklen = 5)
yax = list(title = 'Percentage of Votes')

plot_ly(sd_young_pop, x = sd_young_pop$Year, y = sd_young_pop$x, name = 'Age group 15-34', type = 'scatter', mode = 'lines',
        line = list(color = 'red', width = 4)) %>%
  add_trace(y = sd_middle_pop$x, name = 'Age group 35-44', line = list(color = 'blue', width = 4, dash = 'dash')) %>%
  add_trace(y = sd_old_pop$x, name = 'Age group 45 and above', line = list(color = 'orange', width = 4, dash = 'dot')) %>%
  add_trace(y = sd_voter_turnout$population_percent, name = 'Voter Turn out', line = list(color = 'Green', width = 4, dash = 'dash')) %>%
  layout(xaxis = xax, title = 'Voter Characteristics\nSouth Dakota 2000-2016', 
         xaxis = list(title = "Year"),
         yaxis = list(title = "% of Population"))

sd_democrat_votes <- sd_df[sd_df$Subject %in% c('Democrat vote percent'),]
sd_republican_votes <- sd_df[sd_df$Subject %in% c('Republican vote percent'),]

plot_ly(sd_democrat_votes, x = sd_democrat_votes$Year, y = sd_democrat_votes$population_percent, 
        type = 'bar', name = 'Democracts', marker = list(color='skyblue')) %>%
  add_trace(y = sd_republican_votes$population_percent, name = 'Republicans', marker = list(color = 'red')) %>% 
  add_trace(x = sd_voter_turnout$Year, y = sd_voter_turnout$population_percent, type = 'scatter', 
            mode = 'lines', name = 'Voter Turn out', line = list(color = 'Green', width = 4)) %>% 
  layout(xaxis = xax, yaxis = yax, barmode = 'group', title = 'South Dakota Voting Pattern')

str(SD_GDP)

plot_ly(SD_GDP, x = SD_GDP$DATE, y = SD_GDP$SDNGSP, type = 'scatter', mode = 'lines',
        line = list(color = 'red', width = 4)) %>%
  layout(xaxis = list(title = "Year"), yaxis = list(title = "GDP - Millions of Dollars"), title = "South Dakota GDP")