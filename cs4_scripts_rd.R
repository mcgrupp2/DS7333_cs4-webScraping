library(tidyverse)
library(gganimate)
library(kableExtra)
library(tswge)
library(e1071)
library(RColorBrewer)
library(changepoint)

return_dataTable <- function(url)
{
  # Fetch the page passed as url, expects cherry blossom database site
  # example: http://www.cballtimeresults.org/performances?division=Overall+Women&page=1&section=10M&sex=W&utf8=%E2%9C%93&year=1999
  # Page 1 of Womens 10 Mile from 1999
  page <- xml2::read_html(url)
  
  # create dataframe with results 
  # data needed is in only <table></table> on page, makes it easy
  dataTable <- page %>% # take page
    rvest::html_node("table") %>% # find table
    rvest::html_table() # convert table to dataframe
  
  return(dataTable)
}

collect_data <- function(year, gender)
{
  # Fetch data for first page of desired gender/year combo, needed to create dataframe to add subsequent pages
  # Column header names created issues with rbind(), got colnames from the site on first page as seed for loop
  # Additionally, this will find out how many pages of results to fetch from the "PiS/TiS" column returned
  url_first <- paste("http://www.cballtimeresults.org/performances?division=Overall+",
                     gender,
                     "&page=",
                     1,
                     "&section=10M&sex=",
                     substring(gender,1,1),
                     "&utf8=%E2%9C%93&year=",
                     year,
                     sep = "")  
  
  # Prints the url of first page to show progress
  print(url_first) 
  
  # Seed dataframe for upcoming loop
  data1 <- return_dataTable(url_first) 
  
  # This column shows Place in Sex/Total in Sex
  n = data1$`PiS/TiS`[1] 
  
  # Split 1st value on / select second element, total runners/gender
  n = as.numeric(strsplit(n,"/")[[1]][2]) 
  
  # Pages display 20 results so this finds the n for pages
  n = trunc(n/20) + 1 
  
  # iterate through pages for the year, adding to seed dataframe per page
  i = 2
  
  for (i in 2:n) {
    url_loop <- paste("http://www.cballtimeresults.org/performances?division=Overall+",
                      gender,
                      "&page=",
                      i,
                      "&section=10M&sex=",
                      substring(gender,1,1),
                      "&utf8=%E2%9C%93&year=",
                      year,
                      sep = "")  
    data2 <- return_dataTable(url_loop)
    print(paste(gender, " - ", i, " - ", year))
    data1 <- bind_rows(mutate_all(data1, as.character), mutate_all(data2, as.character))
  }
  
  # return entire year df
  return(data1)
}



get_all_data = function(years, gender)
{
  # seed dataframe for loop due to column name issue
  # fetches first year data
  df_all <- collect_data(years[1], gender)
  
  # iterate through the years of single gender passed
  for(i in seq(2, length(years)))
    {
    # add to year of data to total dataframe
    df_all <- bind_rows(
      mutate_all(df_all, as.character), 
      mutate_all(collect_data(years[i],gender), as.character)
      )
  }
  
  # write results to csv with file name "allData[gender].csv" for later analysis
  write_csv(df_all, paste("caseStudy04_Scraping/data/allData", gender,".csv",sep = ""))
  return(df_all)
}

scrape_data = function()
  {
  # desired years
  years <- seq(1999,2012,1) 
  # Both genders, need to be in this format will be part of url
  # genders <- c("Men", "Women")
  genders <- "Women"
  for (i in 1:length(genders)) {
    # update print
    print(paste(genders[i], " - Starting"))
    
    # collect all data for both genders
    get_all_data(years, genders[i])
    
    #update print
    print(paste(genders[i], " - COMPLETE!!!"))
  }
  #update print
  print("DATA SCRAPE COMPLETE!!!")
}

# run script
# scrape_data()


# We have seen that the 1999 runners were typically older than the 2012 runners.  
# Compare the age distribution of the runners across all 14 years of the races. 
# Use quantile-quantile plots, boxplots, and density curves to make your comparisons. 
# How do the distributions change over the years? 
# Was it a gradual change? 


#~~~~~~~~~ Data Loading and Cleaing

# column names for data from site
col_names <- c("race", 
               "name", 
               "age", 
               "time", 
               "pace", 
               "placeInSex", 
               "division", 
               "placeInDivision", 
               "hometown")

# read csv of compiled Mens data
df_all_mens <- read_csv("caseStudy04_Scraping/data/allDataMen.csv", 
                        col_types = cols(Age = col_character()))

# set the names for the df
names(df_all_mens) <- col_names

# show how many rows in total dataset
#dim(df_all_mens)

# eliminate rows with NR for age
df_trim <- df_all_mens[which(df_all_mens$age != "NR"),]

# show how many eliminated from data
#dim(df_trim)

# switch the age column from character to integer
df_trim$age <- as.integer(df_trim$age)

# this will take the race column split on whitespace, and index year data
minus <- which(unlist(strsplit(df_trim$race, "\\s")) != "10M")

# creates year column 
df_trim$year <- as.integer(unlist(strsplit(df_trim$race, "\\s"))[minus])

# turns division column into factor
df_trim$division <- as.factor(df_trim$division)

# Trying to find state level data

extract_state = function(string)
  {
  tmp <- string
  if (grepl(",", string) == "TRUE"){
    tmp <- str_trim(strsplit(string, ",")[[1]][2])
  }
  return(tmp)
}

extract_state(df_trim$hometown[900])

df_trim$state2 <- unlist(lapply(df_trim$hometown, extract_state))



df_trim$state <- ifelse((grepl(",", df_trim$hometown) == "TRUE"),substr(df_trim$hometown, nchar(df_trim$hometown) - 2, nchar(df_trim$hometown)), "NaS")


df_trim[which(str_locate(df_trim$hometown, ",") - nchar(df_trim$hometown) != -3),]
df_trim[-which(grepl(",", df_trim$hometown) == "TRUE" | nchar(df_trim$hometown) <= 3),]

unlist(unique(df_trim[-which(grepl(",", df_trim$hometown) == "TRUE" | nchar(df_trim$hometown) <= 3),"hometown"]))
#~~~~~~~ EDA

# create summary dataframe by year
df_age_stats <- df_trim %>%
                  group_by(year) %>%
                  summarize(
                    mean = mean(age), 
                    sd = sd(age),
                    oldest = max(age),
                    yngest = min(age),
                    n=n(),
                    skew = skewness(age)
                    )



# create dataframe of number of racers per division, per year
df_div_stats <- df_trim %>%
                  group_by(division, year) %>%
                  summarize(
                    n=n()
                    )

# names for above dataframe, need to add data
div_stat_names <- c("division", "year", "n")

# adding missing data
df_row_to_add <- data.frame("M8099", 2002, 0)
df_row_to_add2 <- data.frame("M8099", 2000, 0)
names(df_row_to_add)<-div_stat_names
names(df_row_to_add2)<-div_stat_names

df_div_stats <- rbind(df_div_stats, df_row_to_add)
df_div_stats <- rbind(df_div_stats, df_row_to_add2)

# turn divisions into factors
df_div_stats$division <- as.factor(df_div_stats$division)

# order the data correctly chronologically
df_div_stats <- df_div_stats[order(df_div_stats$division, df_div_stats$year),]

# loop to create individual dataframes of data for each division by year for differencing
for (i in 1:length(sort(unique(df_div_stats$division))))
{
  div_to_filter <- sort(unique(df_div_stats$division))[i]
  assign(paste("ts_", div_to_filter, sep = ""), 
         df_div_stats[which(df_div_stats$division == div_to_filter),]
         )
}



# plotts.wge(ts_M0119$n)


# create dataframe of 1 difference for the divisions, with the year
# used to see yearly increase to divisions and overall
yearDifferenceByDiv <- data.frame(cbind(ts_M0119$year[2:14], 
                              artrans.wge(ts_M0119$n, phi.tr = 1),
                              artrans.wge(ts_M2024$n, phi.tr = 1),
                              artrans.wge(ts_M2529$n, phi.tr = 1),
                              artrans.wge(ts_M3034$n, phi.tr = 1),
                              artrans.wge(ts_M3539$n, phi.tr = 1),
                              artrans.wge(ts_M4044$n, phi.tr = 1),
                              artrans.wge(ts_M4549$n, phi.tr = 1),
                              artrans.wge(ts_M5054$n, phi.tr = 1),
                              artrans.wge(ts_M5559$n, phi.tr = 1),
                              artrans.wge(ts_M6064$n, phi.tr = 1),
                              artrans.wge(ts_M6569$n, phi.tr = 1),
                              artrans.wge(ts_M7074$n, phi.tr = 1),
                              artrans.wge(ts_M7579$n, phi.tr = 1),
                              artrans.wge(ts_M8099$n, phi.tr = 1),
                              artrans.wge(df_age_stats$n, phi.tr = 1))) 

# give names to above df
names(yearDifferenceByDiv) <- c("year", unlist(as.character(sort(unique(df_div_stats$division)))), "overall")

yearDifferenceByDiv <- yearDifferenceByDiv %>% gather(division, yearly_change, M0119:overall)



table(age = df_trim$age, year = df_trim$year)



#~~~~~~~ Plots

# Animated Density plot year by year

aniYearDens <- df_trim %>%
                ggplot() +
                geom_density(
                  aes(x = age, fill = as.factor(year)), 
                  alpha = 0.4) + 
                labs(title = 'Year: {frame_time}', fill = "Year") +
                transition_time(year) 

animate(aniYearDens, fps=15)


# Shows the number of entrants per year

df_age_stats %>%
  ggplot() +
  geom_bar(aes(x = as.factor(year),
               y = n,
               fill = as.factor(year)),
           stat = "identity") +
  theme(
    axis.text.x = element_text(angle = 30)
  )


# Violin plot of age distribution

df_trim %>%
  ggplot() +
  geom_violin(
    aes(x=as.factor(year), 
        y = age,
        fill = as.factor(year))
    )

# Facet wrapped histgram count for age, years sep

df_trim %>%
  ggplot() +
  geom_histogram(
    aes(x = age,
        y = ..count..,
        fill = as.factor(year)), 
    alpha = 0.4) +
  facet_wrap(~year)

# Animate version of above

aniYearHistCount <- df_trim %>%
                      ggplot() +
                      geom_histogram(
                        aes(x = age,
                            y = ..count..,
                            fill = as.factor(year)), 
                        alpha = 0.4,
                        bins = 30) + 
                      labs(title = 'Year: {frame_time}', fill = "Year") +
                      transition_time(year) 

animate(aniYearHistCount, fps=15)


df_trim %>%
  ggplot() +
  geom_boxplot(
    aes(x = as.factor(year),
        y = age,
        fill = as.factor(year)), 
    alpha = 0.4)+
  theme(
    axis.text.x = element_text(angle = 30),
    legend.position = 'none'
  )

aniYearBox <- df_trim %>%
                ggplot() +
                geom_boxplot(
                  aes(y = age,
                      fill = as.factor(year)), 
                  alpha = 0.4)+
                theme(
                  axis.text.x = element_text(angle = 30)
                )+ 
                labs(title = 'Year: {frame_time}', fill = "Year") +
                transition_time(year) 

animate(aniYearBox, fps=20)


#df_trim %>%
#  #filter(year == 1999) %>%
#  ggplot() +
#  geom_qq(aes(sample = age, color = as.factor(year))) +
#  facet_wrap(~year)

ggplot(data = df_trim, aes(sample = age, color = as.factor(year))) +
  geom_qq(alpha = 0.5) +
  stat_qq_line(alpha = 0.7, color = "red", linetype = "dashed") +
  facet_wrap(~year)


aniYearQQ <- ggplot(data = df_trim, aes(sample = age, color = as.factor(year))) +
              geom_qq(alpha = 0.5) +
              stat_qq_line(alpha = 0.7, color = "red", linetype = "dashed") + 
              labs(title = 'Year: {frame_time}', fill = "Year") +
              transition_time(year) 

animate(aniYearQQ, fps=10)


# Facet wrapped plot shows all year differences

yearDifferenceByDiv %>%
  filter(division != "overall") %>%
  ggplot()+
  geom_bar(aes(x = as.factor(year), y = yearly_change, fill = yearly_change), stat='identity') +
  scale_fill_gradient2(low = "red", mid = "gold", high = "green", midpoint = 100) +
  facet_wrap(~division, nrow = 7)




#df_trim %>%
#  ggplot() +
#  geom_jitter(aes(x = year,
#                  y = age))
#
## table(age = df_trim$age, year = df_trim$year) %>%
##   kbl() %>%
##   kable_paper(full_width = F)  
#
#
#df_ageYear <- data_frame(age = df_trim$age, year = df_trim$year) 
#
## df_ageYear %>%
##   kbl() %>%
##   kable_paper(full_width = F) %>%
##   column_spec(c(1), color = spec_color(df_ageYear$age))
#
#
## df_trim[which(df_trim$age == min(df_trim$age)),]

# # Example of a change in mean at 100 in simulated normal data
# set.seed(1)
# x=c(rnorm(100,0,1),rnorm(100,10,1))
# cpt.mean(x,penalty="SIC",method="AMOC",class=FALSE) # returns 100 to show that the null hypothesis
# #was rejected and the change in mean is at 100
# ans=cpt.mean(x,penalty="Asymptotic",pen.value=0.01,method="AMOC")
# cpts(ans)# returns 100 to show that the null hypothesis was rejected, the change in mean is at 100
# #and we are 99% confident of this result
# cpt.mean(x,penalty="Manual",pen.value=0.8,method="AMOC",test.stat="CUSUM")
# # returns 101 as the changepoint location
