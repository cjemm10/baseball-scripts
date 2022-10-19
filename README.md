This is a repository where I keep all of my baseball scripts that I have been working on as personal projects.

**Fangraphs Scraper** - Script that stores batter and pitcher data from 2010 -present. Every time I run it, I scrape the current seasons data and append it to a dataframe holding all years prior. This helps reduce run time so that I am only retrieving dynamic data, as opposed to static data from past years.

**MLB API** - Script that scrapes pitch-by-pitch game data and sends dataframe to a SQL database table hosted by Google Cloud. Entire process is completely automated. Script can also update dataframes with any missing data, so could be run every day, if user wanted to have an up-to-date database.

**H.E.R & Q.A.B Report** - Hitter Efficiency Rating and Quality At Bat Report Generator. Given data, provides a report for each hitter and details how well their at bats went. This is done through the use of the R Shiny package. Details include calculations as well as strikezone plots of each pitch based on pitch type (fastball, offspeed, breaking) and result (out, rbi, runner moved, single, double, etc...). 
      See example usage here: https://cjemm10.shinyapps.io/HER-QAB/
