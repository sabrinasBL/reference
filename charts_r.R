# What Is This?
  # Sample Code to create basic charts using R
  # Heavily commented to explain each step
  # Eventually this will become intuitive, and your code will have fewer comments
# Who Should Use This?
  # People unfamiliar with data visualization in R
  # Reople who have created base tables already and ONLY need R for data visualization
# What Is Needed to Run This Code?
  # RStudio or another program
  # Basic libraries installed
# For a more comprehensive introduction to R: http://www.cookbook-r.com/

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr) #for %% ?

######################################
####      Reference
######################################

# keyboard shortcuts: https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts
# cmd + shift + c >> comment line
# cmd + enter >> run selection
# types of line charts: 'blank', 'solid', 'dashed', 'dotted', “dotdash”, “longdash”, “twodash”
# create a color palette: https://mycolor.space

######################################
#### Set Custom Style ####
######################################

# set personal style templates
emerald_gradient <- c('#14A989', '#00968C', '#008288', '#076E7D', '#255B6D', '#2F4858')
# if you reference a color theme in later parts of the code and want to test out multiple
  # color schemes without rewriting the code, you can set a color scheme to another existing one
# to make your code universal, use a constant 'my_colors' or similar generic name 
  # and define that before each chart
ss_colors <- emerald_gradient
ss_colors <- c(
              #med-emerald,  
              '#14A989', '#00968C', '#008288', '#076E7D', '#255B6D', '#2F4858',
               #small switch palette      
               '#DDECE0','#558764',
               #natural palette https://mycolor.space/?hex=%2314A989&sub=1
               '#5E9C89','#ECFDF7','#F3EADA',
               #dotting palette
               '#98B0A8', '#339DE3','#9FADBD',
               #black
               '#000000')
theme_ss <- theme_minimal()+
  # theme(text = element_text(family = 'mono', size = 10, color=ss_colors[5]),
  theme(text = element_text(family = 'Helvetica', size = 10, color=ss_colors[5]),
        axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        # axis.ticks.length = unit(0, "lines"),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5, face=c('bold')),
        plot.margin = unit(rep(.5,4),"cm"))

######################################
#### Create Data Frame ####
######################################

dat <- data.frame(
  time = factor(c("First Breakfast","Second Breakfast","Elevenses","Luncheon","Afternoon Tea","Dinner","Supper"), levels=c("First Breakfast","Second Breakfast","Elevenses","Luncheon","Afternoon Tea","Dinner","Supper")),
  total_bill = c(3,18.4,14.89, 17.23,5.02,9,8)
)

######################################
##### Explore Data #####
######################################

# view dataframe contents
dat
# view first 4 rows in dataframe
head(dat,4)
# view last 11 rows in dataframe
# if you choose a # higher than row count, all rows will be displayed
tail(dat,11)

######################################
##### Create Bar Chart #####
######################################

# reference: http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
# fill = total_bill scales the fill based on total_bill values, but does not customize the color or fill direction (ex: lower #'s are darker)
bchart <- ggplot(data=dat, aes(x=time, y=total_bill, fill=total_bill)) + theme_ss+
  geom_bar(stat="identity", width=0.5) +
  # manually set color parameters by setting high and low color values
  # If you want to control the middle value, use scale_fill_gradient2() instead
  scale_fill_continuous(low = '#DDECE0', high = '#14A989')+
  # set fill color parameters using bluelabs colors
  # scale_fill_continuous(low = ss_colors[2], high = ss_colors[5])+
  # set the line at a variable depending on content of data (this points to the 1st row, 2nd column)
  geom_hline(yintercept=dat[1,2], color = ss_colors[1], lwd=.1, linetype = 'dashed')+
  # scale_fill_continuous(low = bluelabs_colors[2], high = bluelabs_colors[5])+
  # place the line at the total_bill value of "Supper" regardless of whether Supper is the 7th item or not
  geom_hline(yintercept=dat$total_bill[dat$time == 'Supper'], color = ss_colors[7], lwd=.9) +
  ############################
  # Can also do this
  # geom_hline(yintercept=dat%>%filter(time == 'Supper')%>%.$total_bill, color = bluelabs_colors[7], lwd=.9) +
  ############################
  # manually set the line x-axis value
  geom_vline(xintercept = 3.5, color = ss_colors[4] , lwd=.5, linetype = 'twodash') +
  ############################
  # Since layers are added on top of each other
  # the order of the lines in your code determine
  # which are viewable when they intersect
  # try moving the lines around to see the difference
  ############################
  labs(title = 'Hobbity Meals',
       y = 'Gold Spent',
       x = element_blank()) +
  #eliminate legend
  guides(fill=FALSE)+
  # annotate adds 1 text element to the chart, placed at x/y coordinates
  # '\n\ creates a line break
  annotate("text", x=dat[7,1], y=(dat[7,2] + 2), label='Supper\nBaseline', size = 4, fontface = .7,
           color=ss_colors[1])+
  annotate("text", x=4, y=(dat[4,2]+1), label='Noon', size = 4, fontface=.7, color=ss_colors[4])

# view the chart within rstudio
bchart
# save the chart to a png
ggsave('Hobbity_Meals.png')

######################################
##### Create Line Graph #####
######################################

library(magrittr)
library(tidyverse)

# import csv and manipulate data - for line graph
# note: need source csv
fte_polls <- read_csv('fivethirtyeight_polls.csv')%>%
  #restrict to polls w/ sample size > 200 in the desired states
  filter(sample_size > 200, state_code %in% c('AZ','FL','IN','MO','NV','OH','TN','WI','WV'))%>%
  #add calculated fields
  mutate(dem_margin = cand_a_pct - cand_b_pct,
         dem_twoway = cand_a_pct/(cand_a_pct + cand_b_pct))%>%
  
head(fte_polls)
  
  # create the chart using bluelabs style
  # ggplot(fte_polls%>%filter(date > '2018-09-01'),
ggplot(fte_polls, aes(x = date, y = dem_margin*100, color = dem_margin*100))+
  geom_point(data = fte_polls%>%filter(date > '2018-09-01'),
             inherit.aes = FALSE, aes(x = date, y = dem_margin*100), size = 1.5)+
  geom_point(size = 0.9)+
  geom_hline(yintercept = 0, size = .25)+
  #temp comment out since don't have access to bl_polls source data
  # geom_hline(aes(yintercept = (twoway*100)-50),
  #            linetype = 'dashed',
  #            color = bluelabs_colors[5])+
  # scale_color_gradient2(low = bluelabs_colors[7], high = bluelabs_colors[5], 
                        # mid = 'white', midpoint = 0, name = 'Democratic\nMargin')+
  # creates individual charts for each value in state_code
  facet_wrap(~state_code)+
  theme_bluelabs+
  stat_smooth(se = FALSE, linetype = 'dashed', color = bluelabs_colors[6])+
  labs(title = 'Public Polling After September',
       y = 'Democratic Margin',
       x = element_blank())
# saves to a png file on your computer
ggsave('Public Polling After September.png', height = 4, width = 9)

############################################################################
#### Create charts for multiple states at once
############################################################################

# loop through states and create individual charts for each, then save to separate png files
# source data not embedded - use this as a template
# source code: https://github.com/bluelabsio/DSCC_2018/blob/master/Post%20Mortem/Analysis%20Code.Rmd#L1542-L1562
for(i in unique(pvi_by_state_ras$state_code)){
  ggplot(pvi_by_state_ras%>%filter(state_code == i, year %in% seq(1980,2020,4)),
         aes(x = year, y = pvi, color = pvi))+
    geom_hline(yintercept = 0, linetype = 'dashed')+
    geom_line(alpha = .7, show.legend = FALSE)+
    geom_point(data = pvi_by_state_ras%>%filter(state_code == i,
                                                year %in% seq(1980,2020,4)), 
               inherit.aes = FALSE, aes(x = year, y = pvi), size = .6)+
    geom_point(show.legend = FALSE, size = 0.4)+
    scale_x_continuous(breaks = seq(1980,2016,10))+
    scale_color_gradient2(low = bluelabs_colors[7], high = bluelabs_colors[5], midpoint = 0, mid = 'grey')+
    theme_bluelabs+
    theme(panel.grid.major = element_line(colour = "grey92", size = 0.5))+
    # dynamic naming for each state
    labs(title = paste0('Relative Partisanship - ', i),
         x = element_blank(),
         y = 'Partisan Voting Index\n(50% National Baseline Two-Way)')+
    scale_y_continuous(labels = percent, limits = c(-.2,.1))
  ggsave(paste0('Baseline Partisanship - ', i,'.png'), height = 2.5, width = 3)
}
