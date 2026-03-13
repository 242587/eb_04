################################################################################
################ Bi1121c: Visualization in R ###################################
################################################################################

# Install the libraries we will need today:
install.packages('ggplot2')
install.packages('plotly') 
install.packages('palmerpenguins')

# Import the libraries
library(ggplot2)
library(palmerpenguins)
library(plotly)
library(dplyr)
library(tidyr)

# Read the palmerpenguins data into 'data' variable
data <- penguins; data


################################################################################
###################### Exploring the dataset ###################################
################################################################################

# Get the structure of data, number of rows and number of columns
summarise(data)
summary(data)
# How many NA values are in total in the dataset?
sum(is.na(data))

# Remove rows containing NA values. How many rows are left now?
data<-data %>% tidyr::drop_na()
nrow(data)


# What islands do these penguins live on?
table(data$island)

# What is the mean body mass of male penguins?
data %>% filter(sex=="male") %>% summarise(mean_mass=mean(body_mass_g))
mean(data$body_mass_g[data$sex=="male"])

# Compute maximum flipper length for each of the penguin species
max(data$flipper_length_mm[data$species=="Adelie"])
max(data$flipper_length_mm[data$species==" "])
max(data$flipper_length_mm[data$species])

data %>% 
  group_by(species) %>% 
  summarise(flipper=max(flipper_length_mm))


# Compute how many penguins lived on each of the islands in each year

data %>% 
  group_by(year) %>% 
  summarise(penguins= n())

# (HW) Is the minimal bill length of Adelie species higher than Gentoo?
data %>% 
  group_by(species) %>% 
  summarise(bill=min(bill_length_mm))

# (HW) On which island do more female than male penguins live?


################################################################################
############################## Visualization ###################################
################################################################################

################################ Base R ########################################

# Adjust the following code, so that it has proper labels of x-axis, y-axis and title
boxplot(data$body_mass_g ~ data$species, xlab="Species", ylab="Body mass g", main="Species mass")

# Adjust the following code, so that:
# Color of the points is based on species, and add the color legend
# Size of the points is 1
# Shape of the points is based on sex
plot(x =  data$bill_depth_mm, 
     y =  data$bill_length_mm,
     col=as.integer(as.factor(data$species)),
     pch=as.integer(as.factor(data$sex)),
     cex=1
)

# Adjust the following code, so that the barplot is horizontal (years will be on y axis)
data.barplot <- data %>%
  group_by(year) %>%
  summarise(n = n()); data.barplot 

barplot(data.barplot$n,
        ylab = 'Year',
        xlab = 'Number of penguins',
        names.arg= data.barplot$year,
        horiz=T)


# Create a barplot, where you plot the number of male and female penguins in the study
barplot_sex<-data %>%
  group_by(sex) %>% 
  summarise(n=n())

barplot(barplot_sex$n, names.arg=barplot_sex$sex)


# Plot the histogram of flipper length of penguins living on Biscoe island in year 2008.
hist(data$flipper_length_mm[data$year=="2008"])

# Create a box plot of bill length [y] based on sex [x], and reorder the x-axis labels in a way male is first
boxplot(data$bill_length_mm~data$sexf,
        xlab="sex",)
data$sexf<-factor(data$sex, levels=c("male", "female"))
levels(data$sexf)
# Create a scatterplot of flipper length based on body mass
plot(data$flipper_length_mm~data$body_mass_g,
     col=as.integer(as.factor(data$island)),
     pch=as.integer(as.factor(data$sex)),
     ylab="Flipper length", xlab="mass"
)
# color the points based on the island
# set the shape of points based on the species
# label the axes appropriately

################################# ggplot2 ######################################

# Adjust the following code, so that the bar plot is horizontal
data %>%
  group_by(island) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = island, y = n))+
  geom_bar(stat = "identity")+
  coord_flip()

# Adjust the previous code, so that the bar plots are arranged in an descending way
data %>%
  group_by(island) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(island, n), y = n))+
  geom_bar(stat = "identity")+
  coord_flip()
data$island<-as.factor(data$island)
# Adjust the following code, so that the boxplot fill is based on species
data %>%
  ggplot(aes(x = species, y = bill_length_mm))+
  geom_boxplot()

# Adjust the previous code, so that faceting on island will be on x-axis
data %>%
  ggplot(aes(x = sex, y = bill_length_mm, fill = species))+
  geom_boxplot() 


data %>%
  ggplot(aes(x = island, y = bill_length_mm))+
  geom_boxplot()

# Adjust the following code: add individual points using the geom_jitter() on the plot
# Color the individual points based on species
data %>%
  ggplot(aes(x = sex, y = bill_length_mm))+
  geom_boxplot()+
  geom_jitter(col=as.integer(as.factor(data$species)))

# Plot the histogram of body mass of penguins living on Biscoe island in 2008

data %>%
  filter(island=="Biscoe", year=="2008") %>% 
  ggplot(aes(x=body_mass_g))+
  geom_histogram()


# Create a scatterplot, where you will plot the dependency of bill length on bill depth and:
#   - color the points by species,
#   - set the shape based on island,
#   - create facets based on sex,
#   - set the theme to theme_minimal()
#   - label the axes appropriately

data %>%
  ggplot(aes(x = bill_length_mm,
             y = bill_depth_mm,
             color = species,
             shape = island,
             theme=theme_minimal)) +
  geom_point()


# Create a stacke"knitr"# Create a stacked barplot, where:
#   - on the x-axis will be species
#   - on y axis counts for each of the respective categories (number of penguins),
#   - fill will be based on island,
#   - set theme to theme_classic()
data%>% 
  ggplot(aes(x=species,
             fill=island))+
  geom_bar()+
  theme_classic()


# Plot a violin plot of Adelie species body mass based on island and overlay it with a boxplot.
#   - Change the color based on island. 
#   - How does parameter trim = FALSE in the geom_violin() function change the output? 

# Create a scatterplot, where you will display the flipper length and body mass of Adelie species which have bill length >= 36,
#   - color the points based on the island, and use a dark palette from RColorBrewer package. 
#   - Use theme_minimal()
#   - change the points size to 2
#   - change opacity to 0.5 

# Adjust the previous graph as if you were to publish it in a scientific journal :-)

################################################################################

# Read into 'df' variable the 'stats-results.csv' file

# plot a volcano plot for the Ubi6-Ctrl contrast
# add a variable 'significant' specifying if the protein is upregulated, downregulated or not-changed
## upregulated: logFC > 1 & p-value < 0.05
## downregulated: logFC < -1 & p-value < 0.05
## not-changed: all other cases

# color the points based on the 'significant' variable
# let's suppose we are specifically interested in following proteins: ARF4, BRAP.1, PSMD12, USP5, TNIP1, VKORC1
## label these proteins in the volcano plot

# create the volcano plot in interactive version using plotly or ggplotly
# use the 'df'; color the points based on 'significant' variable
# display gene names on hover 
# save the interactive volcano plot as an .html file