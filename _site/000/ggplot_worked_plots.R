# plot script

####################################### this must be run first
library(ggplot2)
library(ggplot2movies)
data(movies, package="ggplot2movies")
str(movies)
dim(movies)
names(movies)
head(movies)
?movies

# TIP: You need first to create a genre variable:
genre <- rep(0,nrow(movies))
for(i in 18:24)
{
  genre[movies[,i]==1] <- names(movies)[i]
}; genre[genre==0] <- "Unknown"
movies$Genre <- genre
#######################################



####################################### plots


######## Line plots
# line
ggplot(data = movies, aes(x = votes, y = rating)) +
  geom_line(aes(x = votes, y = rating))

## line - with groups defined by colour and size
ggplot(data=movies, aes(x = votes, y = rating)) +
  geom_line(aes(colour = Genre, size = Genre))

## line - with groups defined by colour and rating by size
ggplot(data=movies, aes(x = votes, y = rating)) +
  geom_line(aes(colour = Genre, size = rating))
######################## 


######## Scatter plots
# scatter
ggplot(data=movies, aes(x = votes, y = rating)) +
  geom_point(aes(x = votes, y = rating))
  
## scatter - with multiple groups
ggplot(data=movies, aes(x = votes, y = rating)) +
  geom_point(aes(colour = Genre)) 

## scatter - with groups defined by size
ggplot(data=movies, aes(x = votes, y = rating)) +
  geom_point(aes(size=Genre))

# scatter - with stat_smooth function with lines of fit and CI
ggplot(movies, aes(x = votes, y = rating)) +
  geom_point(aes(colour = Genre)) +
  stat_smooth(aes(colour = Genre))

# scatter - individual plots for each group horizontal
ggplot(movies, aes(x = votes, y = rating)) +
  geom_point(aes(colour = Genre)) +
  facet_grid(Genre ~ .) # facet_grid makes horizontal grids

# scatter - individual plots for each group in defined number of columns
ggplot(movies, aes(x = votes, y = rating)) +
  geom_point(aes(colour = Genre)) +
  facet_wrap(~Genre, ncol=4) + # facet_wrap makes columns of n length
  coord_flip() # flips the x and y axis
######################## 


######## Histograms 
# histogram 
ggplot(movies, aes(year)) + 
  geom_histogram(binwidth = 1, alpha = 1) + 
  xlab("Year") + ylab("Number of movies produced") +
  theme(
    panel.background = element_blank()
  )

# histogram - with groups defined by colours
ggplot(movies, aes(year, fill = Genre)) + 
    geom_histogram(binwidth = 1, alpha = 1) + 
xlab("Year") + ylab("Number of movies produced") +
  theme(
    panel.background = element_blank()
  )
######################## 


######## Density plots
# density plot
ggplot(movies, aes(x=rating)) + 
  geom_density(fill="blue", alpha=0.6) + 
  ylab("Density of Movie Rating") + 
  xlab("Score (out of 10)")

# density plot - with multiple plots for each group
ggplot(movies, aes(x = length)) +
  geom_density(fill="blue", alpha=0.6) +
  xlim(0,300) + 
  facet_wrap(~ Genre, ncol = 4) +
  ylab("Density of length of film)") +
  xlab("Length of film (minutes)") 
######################## 


######## Violin plots
# violin
ggplot(movies, aes(x=factor(Genre), y=rating)) + 
  xlab("") + 
  ylab("Rating (out of 10") +
  geom_violin(fill="blue", alpha=0.6) +
  stat_summary(fun.y = median, geom='point') +
  theme_bw() +
  theme(axis.text=element_text(face='bold', size = 12, angle = 90, hjust = 1))
######################## 


######## Heatmap plots

# scater heatmap
library(hexbin)
ggplot(movies, aes(x=votes, y=rating)) + xlab("Votes") + ylab("Rating") +
  stat_binhex() +
  scale_fill_gradient(low="lightblue", high="red", breaks=c(0, 1500, 3000, 5000),
                      limits=c(0, 5000))

######## Forest plot
#add error bars to rating so you can make a forest plot
movies$upper <- movies$rating + 1
movies$lower <- movies$rating - 1
movies1 <- subset(movies, year >= 2000)

# forest plot
ggplot(data = movies1, aes(x = year, y = rating, ymin = lower, ymax = upper)) +
  geom_pointrange() + 
  geom_hline(yintercept=5, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Genre") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background

# forest plot - with multiple groups
ggplot(data = movies1, aes(x = year, y = rating, ymin = lower, ymax = upper)) +
  geom_pointrange() + 
  geom_hline(yintercept=5, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Genre") + ylab("Mean (95% CI)") +
  facet_wrap(~Genre, ncol=4) + # facet_wrap makes columns of n length
  theme_bw()  # use a white background











