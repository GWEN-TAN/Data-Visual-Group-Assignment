library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(scales)
library(gganimate)
library(gapminder)
library(gifski)

# Load the data sets
beauty <- read.csv('Beauty.csv')
nrow(beauty)

food <- read.csv('Food  Drink.csv')
nrow(food)

fitness <- read.csv('Health  Fitness.csv')
nrow(fitness)

lifestyle <- read.csv('Lifestyle.csv')
nrow(lifestyle)

shopping <- read.csv('Shopping.csv')
nrow(shopping)

home <- read.csv('House  Home.csv')
nrow(home)


# combine all in one group
l_style <- bind_rows(beauty, food, fitness, lifestyle, shopping, home)
l_style <- l_style[, -1]

# to save the combined data as csv file
#write.csv(l_style, 'all_lifestyle.csv', row.names = FALSE)

# turn off scientific notation for all variables
options(scipen=0)

######################################################## Data pre-processing

str(l_style)
summary(l_style)
# 2982 observations
# there are 106 missing values in Star.Rating
# remove NA from data
l_style_2 <- l_style[!is.na(l_style$Star.Rating),]
summary(l_style_2)
# 2876 observations

# check for duplicates
#which(duplicated(l_style_2$Name))
l_style_2[which(duplicated(l_style_2$Name)),]

l_style_2[l_style_2$Name == 'Makeup Photo Editor',]
l_style_2[l_style_2$Name == 'Beauty Parlour Course',]
l_style_2[l_style_2$Name == 'BMI Calculator',]
l_style_2[l_style_2$Name == 'Mirror',]
# same name but different apps

######################################################## l_style_2$Size

sum(str_count(l_style_2$Size,"kB"))
sum(str_count(l_style_2$Size,"MB"))

# 1kB = 0.001MB
# remove "kB" and change the numbers to "MB", then remove "MB"
# to check if kB is in MB unit
#a <- rownames(l_style_2[grepl("kB", l_style_2$Size), ])

# Extract numeric values from the Size column
numeric_values <- as.numeric(gsub("[[:alpha:]]", "", l_style_2$Size))

# Identify rows with "kB" and convert them to numeric
l_style_2$Size[grepl("kB", l_style_2$Size)] <- numeric_values[grepl("kB", l_style_2$Size)] * 0.001

# remove "MB"
l_style_2$Size <- gsub(" MB", "", l_style_2$Size)
l_style_2$Size <- as.numeric(l_style_2$Size)

######################################################## l_style_2$Reviews

# Extract alphabets from the Reviews column
unique(grep("[[:alpha:]]", l_style_2$Reviews, value = TRUE))
# 1K (1 thousand)
l_style_2$Reviews <- gsub("K", "e+03", l_style_2$Reviews)
# 1L (1 Lakh) = 100T (100 Thousand)
l_style_2$Reviews <- gsub("L", "e+05", l_style_2$Reviews)
# 1M (1 Million)
l_style_2$Reviews <- gsub("M", "e+06", l_style_2$Reviews)
# 1Cr( 1 Crore) = 10M (10 Million)
l_style_2$Reviews <- gsub("Cr", "e+07", l_style_2$Reviews)
# 1T (1 Trillion)
l_style_2$Reviews <- gsub("T", "e+12", l_style_2$Reviews)

# change chr to num
l_style_2$Reviews <- as.numeric(l_style_2$Reviews)

######################################################## l_style_2$Downloads

# Extract alphabets from the Downloads column
unique(grep("[[:alpha:]]", l_style_2$Downloads, value = TRUE))
# remove the + sign
l_style_2$Downloads <- gsub("\\+", "", l_style_2$Downloads)
# 1L (1 Lakh) = 100T (100 Thousand)
l_style_2$Downloads <- gsub("L", "e+05", l_style_2$Downloads)
# 1M (1 Million)
l_style_2$Downloads <- gsub("M", "e+06", l_style_2$Downloads)
# 1Cr( 1 Crore) = 10M (10 Million)
l_style_2$Downloads <- gsub("Cr", "e+07", l_style_2$Downloads)
# 1T (1 Trillion)
l_style_2$Downloads <- gsub("T", "e+12", l_style_2$Downloads)

# change chr to num
l_style_2$Downloads <- as.numeric(l_style_2$Downloads)

######################################################## l_style_2$Rated.for

# Convert the character into ordered factor
l_style_2$Rated.for <- factor(l_style_2$Rated.for,
                              levels = c("3+", "7+", "12+", "16+", "18+"))

str(l_style_2)
summary(l_style_2)
# there are 3 missing values in Downloads
# remove NA from data
l_style_3 <- l_style_2[!is.na(l_style_2$Downloads),]

summary(l_style_3)
# 2873 observations
str(l_style_3)
table(l_style_3$Category)

######################################################## Static Plot

# data for plot 1
d_plot1 <- as.data.frame(l_style_3 %>%
                           group_by(Category) %>%
                           summarise(Total = sum(Downloads)))
ggplot(data = d_plot1,
       aes(x = as.factor(Category), y = Total,
           fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = brewer.pal(7,"Greens")) +
  guides(fill = "none") +
  labs(x = "Category", y = "Count of Downloads",
       title = "Number of Downloads for Each Category") +
  geom_text(aes(label = format(Total, scientific = TRUE)), 
            vjust = -0.5, colour = "springgreen4",
            fontface = "bold", size = 5) +
  theme(plot.title = element_text(face = "bold", colour = "darkgreen", size = 18),
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", colour = "seagreen", size = 15),
        panel.grid.major = element_line(colour = "palegreen"),
        panel.grid.minor = element_line(colour = "palegreen"),
        panel.background = element_rect(fill = "white"))

######################################################## Interactive Plot

# data for plot 2
d_plot2 <- l_style_3[c("Name", "Category", "Star.Rating", "Reviews")]

# axes
f1 <- list(family = "Overpass", size = 18, color = "navy")
x <- list(title = "Average Star Rating", titlefont = f1, tickfont = f1)
y <- list(title = "Reviews Count", titlefont = f1, tickfont = f1)

# title
f2 <- list(family = "Overpass", size = 20, color = "navy")
t <- list(text = "<b>Average Star Rating and Number of Reviews for Apps in Each Category<b>",
          font = f2)

# legends
l <- list(font = list(family = "Overpass", size = 15), orientation = "h",
          title = list(text='<b> Category of Apps: </b>'))

# hover
h <- list(bgcolor = "lightcyan", bordercolor = "skyblue",
          font = list(family="Overpass",color="black"))

mycol <- c("maroon", "orange", "gold", 
           "green", "dodgerblue", "darkorchid")
plot2 <- 
  plot_ly(data = d_plot2,x = ~Star.Rating, y = ~Reviews,
          text = ~paste("Name:", Name,
                        "<br>Category:", Category,
                        "<br>Number of Reviews:", Reviews,
                        "<br>Average Star Rating:", Star.Rating),
          mode = "markers", hoverinfo = "text") %>%
  add_markers(color = ~Category, alpha = 0.4,
              colors = mycol) %>%
  layout(title = t,
         xaxis = x, yaxis = y,
         legend = l, hoverlabel = h)
plot2

# save plot as html file
#htmlwidgets::saveWidget(plot2, "lifestyle_plot2.html")

######################################################## gif Animation

# data for plot 3
d_plot3 <- l_style_3[c("Category", "Rated.for")]
d_plot3 <- d_plot3 %>%
  group_by(Category, Rated.for) %>%
  summarise(Total = n()) %>%
  complete(Category, Rated.for, fill = list(Total = 0))
d_plot3 <- as.data.frame(d_plot3)

plot3 <-
  ggplot(data = d_plot3,aes(x = Category, y = Total, fill = Rated.for)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = brewer.pal(10,"PiYG")) +
  geom_text(aes(label = Total), position = position_dodge(width = 1), 
            vjust = -0.5, colour = "palevioletred1",
            fontface = "bold", size = 5) +
  transition_states(Category) +
  labs(title = "Category: {closest_state}
       \nNumber of Apps by Minimum Age to Use",
       fill = "Minimum \nAge Limit",
       y = "Number of Apps") +
  theme(plot.title = element_text(face = "bold", colour = "maroon", size = 18),
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", colour = "hotpink", size = 15),
        legend.title = element_text(face = "bold", colour = "hotpink", size = 12),
        legend.text = element_text(size = 12),
        panel.grid.major = element_line(colour = "mistyrose"),
        panel.grid.minor = element_line(colour = "mistyrose"),
        panel.background = element_rect(fill = "white"))

# set the size of gif
plot3 <- animate(plot3, width = 1000, height = 800)
plot3

# save the animation as gif
#anim_save("lifestyle_plot3.gif", plot3)
