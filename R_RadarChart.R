install.packages("gganimate")
library(fmsb)

#Reference
# https://www.r-graph-gallery.com/142-basic-radar-chart/
# https://www.rdocumentation.org/packages/fmsb/versions/0.6.3/topics/radarchart

data <- data.frame(
  Social = c(0),
  # Family = c(2),
  Volunteering = c(1),
  Money = c(9),
  Work = c(3),
  Exercise = c(8),
  Food = c(2),
  Luurv = c(0),
  # Creativity = c(2),
  Learning = c(9)
  # Partner = c(1)
  )

#view data
head(data)

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(10,5) , rep(0,5) , data)

# The default radar chart proposed by the library:
radarchart(data)

#set font style (can't add theme)
op <- par(family = "Caviar Dreams")
# , color = '#3B4856') 

# Customize the radarChart !
radarchart( data , axistype=0, 
            #custom polygon
            pcol=colors_emerald_contrast[1] ,
            pty = 6,
            #color of fill
            # pfcol=colors_ss[14] ,
            # pdensity = .5,
            # pangle = 90,
            # plty = 1,
            plwd=1.5,
            # vlabels=c("Social\nLife", "Volunteering/\nCommunity",
                      # "Money", "Work", "Health","Luurv","Learning")
            title = "Life Chart",
            #color of lines
            cglcol = colors_ss[14],
            #custom the grid
            # cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,5), cglwd=0.8,
            #custom labels
            # vlabels=c("Social\nLife", "Volunteering/\nCommunity",
                      # "Money", "Work", "Health","Luurv","Learning"
            vlcex=1.5,
            cex.main=1.5
            # axislabcol = '#3B4856'
            # cglty = 7
)
par(op)


# Data must be given as the data frame, where the first cases show maximum.
maxmin <- data.frame(
  total=c(5, 1),
  phys=c(15, 3),
  psycho=c(3, 0),
  social=c(5, 1),
  env=c(5, 1))
# data for radarchart function version 1 series, minimum value must be omitted from above.
RNGkind("Mersenne-Twister")
set.seed(123)
dat <- data.frame(
  total=runif(3, 1, 5),
  phys=rnorm(3, 10, 2),
  psycho=c(0.5, NA, 3),
  social=runif(3, 1, 5),
  env=c(5, 2.5, 4))
dat <- rbind(maxmin,dat)
op <- par(mar=c(1, 2, 2, 1),mfrow=c(2, 2))
radarchart(dat, axistype=1, seg=5, plty=1, vlabels=c("Total\nQOL", "Physical\naspects", 
                                                     "Phychological\naspects", "Social\naspects", "Environmental\naspects"), 
           title="(axis=1, 5 segments, with specified vlabels)", vlcex=0.5)
radarchart(dat, axistype=2, pcol=topo.colors(3), plty=1, pdensity=c(5, 10, 30), 
           pangle=c(10, 45, 120), pfcol=topo.colors(3), 
           title="(topo.colors, fill, axis=2)")
radarchart(dat, axistype=3, pty=32, plty=1, axislabcol="grey", na.itp=FALSE,
           title="(no points, axis=3, na.itp=FALSE)")
radarchart(dat, axistype=1, plwd=1:5, pcol=1, centerzero=TRUE, 
           seg=4, caxislabels=c("worst", "", "", "", "best"),
           title="(use lty and lwd but b/w, axis=1,\n centerzero=TRUE, with centerlabels)")
par(op)
# }