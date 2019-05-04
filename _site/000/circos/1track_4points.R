# step5_circos-plots 
# step1 - 

#####
# packages
install.packages("circlize")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ComplexHeatmap")

library(circlize)
library(tidyr)
library(dplyr)
library(ComplexHeatmap)

##### Decide what you want your legend to look like - this will be plotted in the centre of the circle
# lgd_1 <- Legend(at = c("Mothers FOM1", "Mothers FOM2"), # breaks, can be wither numeric or character
#                 labels_gp = gpar(fontsize = 30),
#                 ncol	= TRUE, # if there are too many legends, they can be positioned in an array, this controls number of columns. At a same time only one of nrow and ncol can be specified.
#                 by_row = TRUE, # when there are multiple columns for legends, whether to arrange them by rows.
#                 border = NA, # color of legend borders, also for the ticks in the continuous legend
#                 background = NA, # background colors
#                 legend_gp = gpar(col = c("black", "purple")), # graphic parameters for the legend
#                 type = "points", # type of legends, can be grid, points and lines
#                 pch = 1, # type of points
#                 size = unit(30, "mm"), # size of points
#                 grid_height	= unit(60, "mm"),
#                 grid_width = unit(60, "mm"))
# 
# lgd_2 <- Legend(at = c("P < 0.05/158",  "P < 0.05/158"), # breaks, can be wither numeric or character
#                 labels_gp = gpar(fontsize = 30),
#                 ncol	= TRUE, # if there are too many legends, they can be positioned in an array, this controls number of columns. At a same time only one of nrow and ncol can be specified.
#                 by_row = TRUE, # when there are multiple columns for legends, whether to arrange them by rows.
#                 border = NA, # color of legend borders, also for the ticks in the continuous legend
#                 background = NA, # background colors
#                 legend_gp = gpar(col = c("black", "purple")), # graphic parameters for the legend
#                 type = "points", # type of legends, can be grid, points and lines
#                 pch = 19, # type of points
#                 size = unit(30, "mm"), # size of points
#                 grid_height	= unit(60, "mm"),
#                 grid_width = unit(60, "mm"))
# 
# lgd <- packLegend(lgd_1, lgd_2)

##### output plot - saves best as a PDF
# PDF
# pdf("NAME_OF_PLOT.pdf",
#     width = 35, height = 35, pointsize = 30)

##### load data you want to plot
track2_data1 <- read.table("/Users/ml16847/OneDrive - University of Bristol/001_projects/004_GRS_BMI_FA/analysis/step4_observational-analysis/final_mums_FOM1_BMI_observational.txt", header = T, sep = "\t")
track2_data2 <- read.table("", header = T, sep = "\t")
track2_data3 <- read.table("", header = T, sep = "\t")
track2_data4 <- read.table("", header = T, sep = "\t")
data <- track2_data1 # set this to begin with so we can set the variables below like where to source labels

##### choose all of the variables for automating the plot

##### Variables you must change!
# data specifics
section_column <- 9 # what column (number format) is your data for creating sections - these are the groups you split your data up into
pval_adj <- 0.05/158 # what do you want your adjusted p value threshold to be
labels <- "raw.label" # what is the name of the column in your data that has your labels
join <- "metabolite" # what is the name of the column you will use to join your data frame and the ordering data frame you make
p <- "pval" # what is the name of your pvalue column
beta <- "b"
# plotting specifics (only go up to as many tracks as you will plot)
track1 <- 1 # track 1 is your section header, you start plotting on track 2
track2 <- 2
x_axis_index <- 1 # what section do you want to plot your x axis on


#####


##### Variables you can change if you want
# section header specifics
section_fill_colour <- "snow2" # colour of section header
section_text_colour <- "black" # colour of text in section header
section_line_colour <- "grey" # colour of Y axis lines 
section_line_thickness <- 1.5
section_line_type <- 1

# reference lines that go around the tracks
reference_line_colour <- "deeppink"
reference_line_thickness <- 1.5
reference_line_type <- 1

x_axis_top_line_colour <- "grey"
x_axis_top_line_thickness <- 1.5
x_axis_top_line_type <- 1

x_axis_bottom_line_colour <- "grey"
x_axis_botoom_line_thickness <- 1.5
x_axis_bottom_line_type <- 1

x_axis_tophalf_line_colour <- "deeppink"
x_axis_tophalf_line_thickness <- 1.5
x_axis_tophalf_line_type <- 1

x_axis_bottomhalf_line_colour <- "deeppink"
x_axis_bottomhalf_line_thickness <- 1.5
x_axis_bottomhalf_line_type <- 1

# point specifics
point_type <- 1
point_pval_type <- 19
point_cex <- 1.5
point_col1 <- "black"
point_col2 <- "purple"
point_col3 <- "green"
point_col4 <- "blue"

# y axis specifics
y_axis_location <- "left"
y_axis_tick <- FALSE
y_axis_tick_length <- 0
y_axis_label_cex <- 0.75

# label specifics
label_distance <- 1.5 # distance from track 0 to plot labels
label_col <- "black"
label_cex <- 0.6

# Identify all of the axis you want to use for the plot and store as variables that will plot automatically for you
a <- min(track2_data1[[beta]])
b <- min(track2_data2[[beta]])
c <- min(track2_data3[[beta]])
d <- min(track2_data4[[beta]])
track2_axis_min <- round(min(a,b,c,d), 3)

a <- max(track2_data1[[beta]])
b <- max(track2_data2[[beta]])
c <- max(track2_data3[[beta]])
d <- max(track2_data4[[beta]])
track2_axis_max <- round(max(a,b,c,d), 3)

track_axis_reference <- 0
track2_axis_min_half <- track2_axis_min/2
track2_axis_max_half <- track2_axis_max/2

#####


##### Variables you shouldn't change really
# circle specifics
margins <- c(0.5, 0.5, 0.5, 0.5) * 25 # A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot
start_gap <- 17 # indicates gap at start for Y axis scale, this is a percentage so the larger the number the larger the empty gap
start_degree <- 90 # starting point of the cirlce in degrees (90 is top)
section_track_height <- 0.10 # size of secton header track as percent of whole circle
track_height <- 0.20 # size of track as percentage of whole circle


##### track 1 and 2
# use the track1_data1 to plot the initial circle and plotting area
data <- track2_data1

##### data prep for plotting
# change the name of the column you want to create sections on to 'sections'
colnames(data)[section_column] <- "sections"

# create structured order system for plotting
data <- data[order(data$sections, data[[beta]]),]
data$order <- 1:nrow(data)
order <- data[,c(1,13)]
data <- data[,c(1:12)]

# order based on metabolite category (section) and alphabetically within that based on name of the metabolite
data <-  left_join(data, order, by = join)
data <- data[order(data$order),]

# add the X column - this is position of values within the track and should be 1:n depedning on number of variables in each section and is based also on individual sections. this will give a number 1-n for each individual metabolite in each section as a position within the section for plotting values.
data$x <- with(data, ave(seq_along(sections), sections, FUN=seq_along))

# Set parameter - this sets the paramater of the sections of the circos plot so that you can plot within individual sections
npercat <- as.vector(table(data$sections))

# Standardise x axis to 0-1 - each sections X axis runs 0-1 so this has to be done and provides for each section an individual x axis which we use for plotting individual values within sections as opposed to the plot as a whole
getaxis <- function(data) {
  
  for (i in 1:nrow(data)) {
    
    data$n[i]<-as.numeric(nrow(subset(data, sections == data$sections[i])))
    data$ncat[i]<- data$x[i]/data$n[i]
  }
  return(data)
}

data <- getaxis(data)

# add column that codes sections as numbers so that we can add this into the plot (the whole metabolite category name doesnt fit) - these numbers can then be translated in the legend or figure title
data$section_numbers = factor(data$sections,
                              labels = 1:nlevels(data$sections))

# Set gap for axis
gap = c(rep(1, nlevels(data$sections)-1), start_gap) # creates vector gap which dictates the spacing between sections, and it is 1-n where n is 1 minus the total number of sections you want as dictated by your categories - last number indicates gap at start for Y axis scale, this is a percentage so the larger the number the larger the empty gap

##### Create the plotting area
circos.clear() #start by clearing the plot space ready for a new plot

#outer border, zoom
par(mar = margins, 
    cex = 0.8, #magnification of plotting symbols relative to the default
    xpd = NA) #A logical value or NA. If FALSE, all plotting is clipped to the plot region, if TRUE, all plotting is clipped to the figure region, and if NA, all plotting is clipped to the device region. See also clip.

circos.par(cell.padding = c(0, 0.5, 0, 0.5), 
           start.degree = start_degree, # starting point of the circle
           gap.degree = gap, # gap between two neighbour sections
           track.margin = c(0.012, 0.012), #blank area outside of the plotting area
           points.overflow.warning = FALSE, #this dictates whether warnings will pop up if plots are plotted outside of the plotting region (this is to do with teh plotting region not being circular and instead being rectangular) - keep this as FALSE
           track.height = section_track_height, #height of the circle tracks
           clock.wise = TRUE) #direction to add sections

circos.initialize(factors = data$section_numbers, #circos.initialize startes the plot based on the factors we want plotted, so the factors are the sections of the plot
                  #x = data$ncat, #ncat here acts as xlim=c(0, 1) essentially 
                  xlim = c(0,1),
                  sector.width = npercat) #we set the width of each section based on the number of variables within each section as dictated by nperat which we define earlier

##### Track 1 - the outer track
#in this instance we are setting track 1 as a reference track with information about the entire plot on - we will plot the names of each variable and the sections on this track
circos.trackPlotRegion(factors = data$section_numbers, #we plot the first region based on the column in our data set which we creat the sections from
                       track.index = track1, #the track you are plotting
                       x = data$ncat, #the x axis is dictated by the number of values in our data set
                       ylim = c(0, 1), #the y axis is set based on the track width you want as it will impact the size of text you can have in this track
                       track.height = 0.05, #size of track as % of whole circle
                       panel.fun = function(x, y) { 
                         chr = get.cell.meta.data("sector.index") #dont change as this gathers all of the info you need automatically
                         xlim = get.cell.meta.data("xlim") #dont change as this gathers all of the info you need automatically
                         ylim = get.cell.meta.data("ylim") #dont change as this gathers all of the info you need automatically
                         circos.rect(xlim[1], 0, xlim[2], 1, # n (+ and -) length of track away from centre (low number means smaller) - want it large enough to encompass text
                                     border = NA, #give the track a border
                                     col = section_fill_colour) #colour of track
                         circos.text(mean(xlim), #text location on x axis
                                     mean(ylim), #text location on y axis
                                     chr, #colour of text, default blue
                                     cex = 1, #text size
                                     facing = "outside", #diretion of text
                                     niceFacing = TRUE, #flip so text is readable 
                                     col = section_text_colour)
                       },
                       bg.border = NA) #background border colour

##### Track 2 - the first plotting track
circos.trackPlotRegion(factors = data$section_numbers, #we plot the first region based on the column in our data set which we creat the sections from
                       track.index = track2, #the track you are plotting
                       x = data$ncat, #set this as ncat as ncat dictates the location of the variable you want to plot within each section and within the circle as a whole
                       y = data$b, #variable you want to plot
                       ylim = c(track2_axis_min, track2_axis_max), #co-ordinates of the Y axis of the track
                       track.height = track_height, #how big is the track as % of circle
                       
                       #Set sector background  
                       bg.border = NA,
                       bg.col = NA,
                       
                       #Map values
                       panel.fun = function(x, y) { #this sets x and y as the above defined variables for the following lines of code
                         
                         ######### lines
                         # plot vertical lines as background, this line will be directly behind your point value and will help readers to follow the point to its variable name - lwd =3 ==> top level
                         circos.segments(x0 = x, #x is defined earlier and is ncat
                                         y0 = y * 0 - -(track2_axis_min) , #set the y axis so that it is the same as teh height of your y axis for the track height
                                         x1 = x, #x is defined earlier and is ncat
                                         y1 = y * 0 + track2_axis_max, #set the y axis so that it is the same as teh height of your y axis for the track height
                                         col = section_line_colour, #colour of the line
                                         lwd = section_line_thickness, #set the thickness of the line so it's a bit smaller than your point (looks better)
                                         lty = section_line_type) #best to set the line type as a solid line
                         
                         # plot '0' reference line
                         circos.lines(x = x,
                                      y = y * 0 + track_axis_reference,
                                      col = reference_line_colour, #set the 0 line colour to something distinctive
                                      lwd = reference_line_thickness, #set the thickness of the line so it's a bit smaller than your point (looks better)
                                      lty = reference_line_type) 
                         # plot reference line at y=n - best to have this as the minimum of your y axis
                         circos.lines(x = x, 
                                      y = y * 0 - -(track2_axis_min), 
                                      col = x_axis_top_line_colour,
                                      lwd = x_axis_top_line_thickness,
                                      lty = x_axis_top_line_type)
                         # plot reference line at y=n - set halfway between the 0 and your min Y axis or as desigreen
                         circos.lines(x = x, 
                                      y = y * 0 - -(track2_axis_min_half),
                                      col = x_axis_tophalf_line_colour,
                                      lwd = x_axis_tophalf_line_thickness,
                                      lty = x_axis_tophalf_line_type)
                         # plot reference line at y=n set halfway between the 0 and your max Y axis or as desigreen
                         circos.lines(x = x,
                                      y = y * 0 + track2_axis_max_half,
                                      col = x_axis_bottomhalf_line_colour,
                                      lwd = x_axis_bottomhalf_line_thickness,
                                      lty = x_axis_bottomhalf_line_type)
                         # plot reference line at y=n - best to have this as the maximum of your y axis
                         circos.lines(x = x,
                                      y = y * 0 + track2_axis_max,
                                      col = x_axis_bottom_line_colour,
                                      lwd = x_axis_botoom_line_thickness,
                                      lty = x_axis_bottom_line_type)
                         
                         #points - plot the points 
                         circos.points(x,
                                       y,
                                       pch = point_type,
                                       cex = point_cex,
                                       col = point_col1) 
                       })


#significance - identify significant points within the track by changing the point colour
circos.trackPoints(factors = subset(data, p < pval_adj)$section_numbers, 
                   track.index = track2,
                   x = subset(data, p < pval_adj)$ncat,
                   y = subset(data, p < pval_adj)[[beta]],
                   cex = point_cex, 
                   pch = point_pval_type,
                   col = point_col1)

##### track specifics
# axis values - plot your x axis labels, this is plotted in the space created in the set up of the plot earlier in the script
circos.yaxis(side = y_axis_location,
             sector.index = x_axis_index, #the sector this is plotted in 
             track.index = track2,
             at = c(track2_axis_min, track2_axis_min_half, track_axis_reference, track2_axis_max_half, track2_axis_max), #location on the y axis as well as the name of the label
             tick = y_axis_tick, tick.length = y_axis_tick_length,
             labels.cex = y_axis_label_cex) #size of text


##### add label of each metabolite!
circos.trackText(factors = data$section_numbers,
                 track.index = track1, #choose labels based on the track we have just made as you can only plot text once a track has been created
                 x = data$ncat, #location on the x axis where we will plot the lables
                 y = data$b * 0 + label_distance, #dictates where the labels are plotted - * 0 to give 0 and then choose how far away from the 0 we want to plot the text (this will be trial and error before you get what works best for your data set)
                 labels = labels, #where you are taking the names for the labels from
                 facing = "reverse.clockwise",
                 niceFacing = TRUE, #flip the text so it is readable
                 adj = c(1, 1), 
                 col = label_col,
                 cex = label_cex) #size of the text
#####





##### Track 2 data 2
# use the track1_data1 to plot the initial circle and plotting area
data <- track2_data2

##### data prep for plotting
# change the name of the column you want to create sections on to 'sections'
colnames(data)[section_column] <- "sections"

# order based on metabolite category (section) and alphabetically within that based on name of the metabolite
data <-  left_join(data, order, by = "metabolite")
data <- data[order(data$order),]

# add the X column - this is position of values within the track and should be 1:n depedning on number of variables in each section and is based also on individual sections. this will give a number 1-n for each individual metabolite in each section as a position within the section for plotting values.
data$x <- with(data, ave(seq_along(sections), sections, FUN=seq_along))

# Set parameter - this sets the paramater of the sections of the circos plot so that you can plot within individual sections
npercat <- as.vector(table(data$sections))

# Standardise x axis to 0-1 - each sections X axis runs 0-1 so this has to be done and provides for each section an individual x axis which we use for plotting individual values within sections as opposed to the plot as a whole
getaxis <- function(data) {
  
  for (i in 1:nrow(data)) {
    
    data$n[i]<-as.numeric(nrow(subset(data, sections == data$sections[i])))
    data$ncat[i]<- data$x[i]/data$n[i]
  }
  return(data)
}

data <- getaxis(data)

# add column that codes sections as numbers so that we can add this into the plot (the whole metabolite category name doesnt fit) - these numbers can then be translated in the legend or figure title
data$section_numbers = factor(data$sections,
                              labels = 1:nlevels(data$sections))

# Set gap for axis
gap = c(rep(1, nlevels(data$sections)-1), start_gap) # creates vector gap which dictates the spacing between sections, and it is 1-n where n is 1 minus the total number of sections you want as dictated by your categories - last number indicates gap at start for Y axis scale, this is a percentage so the larger the number the larger the empty gap

##### Track 2 data 2 
circos.trackPlotRegion(factors = data$section_numbers, #we plot the first region based on the column in our data set which we creat the sections from
                       track.index = track2, #the track you are plotting
                       x = data$ncat, #set this as ncat as ncat dictates the location of the variable you want to plot within each section and within the circle as a whole
                       y = data$b, #variable you want to plot
                       ylim = c(track2_axis_min, track2_axis_max), #co-ordinates of the Y axis of the track
                       track.height = track_height, #how big is the track as % of circle
                       
                       #Set sector background  
                       bg.border = NA,
                       bg.col = NA,
                       
                       #Map values
                       panel.fun = function(x, y) { #this sets x and y as the above defined variables for the following lines of code
                         
                         #points - plot the points 
                         circos.points(x,
                                       y,
                                       pch = point_type,
                                       cex = point_cex,
                                       col = point_col2) 
                       })


#significance - identify significant points within the track by changing the point colour
circos.trackPoints(factors = subset(data, p < pval_adj)$section_numbers, 
                   track.index = track2,
                   x = subset(data, p < pval_adj)$ncat,
                   y = subset(data, p < pval_adj)[[beta]],
                   cex = point_cex, 
                   pch = point_pval_type,
                   col = point_col2)
#####


##### Track 2 data 3
# use the track1_data1 to plot the initial circle and plotting area
data <- track2_data3

##### data prep for plotting
# change the name of the column you want to create sections on to 'sections'
colnames(data)[section_column] <- "sections"

# order based on metabolite category (section) and alphabetically within that based on name of the metabolite
data <-  left_join(data, order, by = "metabolite")
data <- data[order(data$order),]

# add the X column - this is position of values within the track and should be 1:n depedning on number of variables in each section and is based also on individual sections. this will give a number 1-n for each individual metabolite in each section as a position within the section for plotting values.
data$x <- with(data, ave(seq_along(sections), sections, FUN=seq_along))

# Set parameter - this sets the paramater of the sections of the circos plot so that you can plot within individual sections
npercat <- as.vector(table(data$sections))

# Standardise x axis to 0-1 - each sections X axis runs 0-1 so this has to be done and provides for each section an individual x axis which we use for plotting individual values within sections as opposed to the plot as a whole
getaxis <- function(data) {
  
  for (i in 1:nrow(data)) {
    
    data$n[i]<-as.numeric(nrow(subset(data, sections == data$sections[i])))
    data$ncat[i]<- data$x[i]/data$n[i]
  }
  return(data)
}

data <- getaxis(data)

# add column that codes sections as numbers so that we can add this into the plot (the whole metabolite category name doesnt fit) - these numbers can then be translated in the legend or figure title
data$section_numbers = factor(data$sections,
                              labels = 1:nlevels(data$sections))

# Set gap for axis
gap = c(rep(1, nlevels(data$sections)-1), start_gap) # creates vector gap which dictates the spacing between sections, and it is 1-n where n is 1 minus the total number of sections you want as dictated by your categories - last number indicates gap at start for Y axis scale, this is a percentage so the larger the number the larger the empty gap

##### Track 2 data 3 
circos.trackPlotRegion(factors = data$section_numbers, #we plot the first region based on the column in our data set which we creat the sections from
                       track.index = track2, #the track you are plotting
                       x = data$ncat, #set this as ncat as ncat dictates the location of the variable you want to plot within each section and within the circle as a whole
                       y = data$b, #variable you want to plot
                       ylim = c(track2_axis_min, track2_axis_max), #co-ordinates of the Y axis of the track
                       track.height = track_height, #how big is the track as % of circle
                       
                       #Set sector background  
                       bg.border = NA,
                       bg.col = NA,
                       
                       #Map values
                       panel.fun = function(x, y) { #this sets x and y as the above defined variables for the following lines of code
                         
                         #points - plot the points 
                         circos.points(x,
                                       y,
                                       pch = point_type,
                                       cex = point_cex,
                                       col = point_col3) 
                       })


#significance - identify significant points within the track by changing the point colour
circos.trackPoints(factors = subset(data, p < pval_adj)$section_numbers, 
                   track.index = track2,
                   x = subset(data, p < pval_adj)$ncat,
                   y = subset(data, p < pval_adj)[[beta]],
                   cex = point_cex, 
                   pch = point_pval_type,
                   col = point_col3)
#####


##### Track 2 data 4
# use the track1_data1 to plot the initial circle and plotting area
data <- track2_data4

##### data prep for plotting
# change the name of the column you want to create sections on to 'sections'
colnames(data)[section_column] <- "sections"

# order based on metabolite category (section) and alphabetically within that based on name of the metabolite
data <-  left_join(data, order, by = "metabolite")
data <- data[order(data$order),]

# add the X column - this is position of values within the track and should be 1:n depedning on number of variables in each section and is based also on individual sections. this will give a number 1-n for each individual metabolite in each section as a position within the section for plotting values.
data$x <- with(data, ave(seq_along(sections), sections, FUN=seq_along))

# Set parameter - this sets the paramater of the sections of the circos plot so that you can plot within individual sections
npercat <- as.vector(table(data$sections))

# Standardise x axis to 0-1 - each sections X axis runs 0-1 so this has to be done and provides for each section an individual x axis which we use for plotting individual values within sections as opposed to the plot as a whole
getaxis <- function(data) {
  
  for (i in 1:nrow(data)) {
    
    data$n[i]<-as.numeric(nrow(subset(data, sections == data$sections[i])))
    data$ncat[i]<- data$x[i]/data$n[i]
  }
  return(data)
}

data <- getaxis(data)

# add column that codes sections as numbers so that we can add this into the plot (the whole metabolite category name doesnt fit) - these numbers can then be translated in the legend or figure title
data$section_numbers = factor(data$sections,
                              labels = 1:nlevels(data$sections))

# Set gap for axis
gap = c(rep(1, nlevels(data$sections)-1), start_gap) # creates vector gap which dictates the spacing between sections, and it is 1-n where n is 1 minus the total number of sections you want as dictated by your categories - last number indicates gap at start for Y axis scale, this is a percentage so the larger the number the larger the empty gap

##### Track 2 data 4 
circos.trackPlotRegion(factors = data$section_numbers, #we plot the first region based on the column in our data set which we creat the sections from
                       track.index = track2, #the track you are plotting
                       x = data$ncat, #set this as ncat as ncat dictates the location of the variable you want to plot within each section and within the circle as a whole
                       y = data$b, #variable you want to plot
                       ylim = c(track2_axis_min, track2_axis_max), #co-ordinates of the Y axis of the track
                       track.height = track_height, #how big is the track as % of circle
                       
                       #Set sector background  
                       bg.border = NA,
                       bg.col = NA,
                       
                       #Map values
                       panel.fun = function(x, y) { #this sets x and y as the above defined variables for the following lines of code
                         
                         #points - plot the points 
                         circos.points(x,
                                       y,
                                       pch = point_type,
                                       cex = point_cex,
                                       col = point_col4) 
                       })


#significance - identify significant points within the track by changing the point colour
circos.trackPoints(factors = subset(data, p < pval_adj)$section_numbers, 
                   track.index = track2,
                   x = subset(data, p < pval_adj)$ncat,
                   y = subset(data, p < pval_adj)[[beta]],
                   cex = point_cex, 
                   pch = point_pval_type,
                   col = point_col4)
#####





##### end circos plotting
circos.clear()

##### draw on the legend
grid.draw(lgd)

##### clear/save out
dev.off()




