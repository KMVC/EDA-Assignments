#Using data from the 2009 CHIS adult-response dataset, I want to examine which groups are under/over represented 
#within the data set, by creating mosaic plots

# Load all packages
library(tidyverse)
library(reshape2)
library(ggthemes)

#adult dataset was loaded into the Datacamp workspace

# Explore the dataset using the functions summary and str
str(adult)
summary(adult)

# To continue to explore the data, will create 2 histograms, one for age and the second by BMI
ggplot(adult, aes(x = SRAGE_P))+ geom_histogram()

# BMI value histogram
ggplot(adult, aes(x = BMI_P))+ geom_histogram()

#Bring these two variables together (using colour (the fill function)), and specify the binwidth of 1 (bins 
#represent a range of values/buckets)
# Age colored by RBMI, binwidth = 1
ggplot(adult, aes(x = SRAGE_P, fill = factor(RBMI)))+ geom_histogram(binwidth = 1)

#Data cleaning

# Keep only adults younger than or equal to 84 (at 85, there is an odd spike that suggests some irregularity in 
#data collection)
adult <- adult[adult$SRAGE_P <= 84, ] 

# Want to get rid of long positive tale on BMI, so only Keep adults with BMI at least 16 and less than 52
adult <- adult[adult$BMI_P >= 16 & adult$BMI_P < 52, ]

# I want to focus on the relationship between age, BMI, and race, so I need to relabel the race variable
#to make plotting easier later
adult$RACEHPR2 <- factor(adult$RACEHPR2, labels = c("Latino", "Asian", "African American", "White"))

# Also need to relabel the BMI categories variable for easier plotting later
adult$RBMI <- factor(adult$RBMI, labels = c("Under-weight", "Normal-weight", "Over-weight", "Obese"))

# The color scale to be used in the plot
BMI_fill <- scale_fill_brewer("BMI Category", palette = "Reds")

# Use the function Theme to fix category display in faceted plot
fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                    strip.background = element_blank(),
                    legend.position = "none")

# Making a Histogram, I add BMI_fill and use facet_grid to facet rows according to RBMI
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1) +
  fix_strips + BMI_fill + facet_grid(RBMI~.)+ theme_classic()

#Want to make a number of histograms, to explore the different kinds of histograms that can be created and 
#to see their utility

# The first plot is a Count histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1) +
  BMI_fill

# Plot 2 - A Density histogram -  get the density of each BMI category
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  BMI_fill

# Plot 3 - A Faceted count histogram - created by employing facet_grid
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1) +
  BMI_fill + facet_grid(RBMI~.)


# Plot 4 - Faceted density histogram - creating a faceted histogram that shows density
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  BMI_fill + facet_grid(RBMI~.)



# Plot 5 - Density histogram, making position = "fill" (this does not create an accurate representation however)
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, position = "fill") +
  BMI_fill


# Plot 6 - The accurate histogram by using count../sum(..count..) instead of density
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, position = "fill") +
  BMI_fill

#Because the calculations occur on the fly inside ggplot2, a frequency histogram can't be facetted.
#So going to calculate proportions outside ggplot2.

# A failed attempt to facet the accurate frequency histogram from before 
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, position = "fill") +
  BMI_fill +
  facet_grid(RBMI ~ .)

# Create a DF with the table() function
DF <- table(adult$RBMI, adult$SRAGE_P)

# Use apply on DF created above to get frequency of each group
DF_freq <- apply(DF, 2, function(x) x/sum(x))

# Load reshape2 and use melt on DF to create DF_melted (reshape allows me to work direclty on a table)
library(reshape2)
DF_melted <- melt(DF_freq)

# Change names of DF_melted
names(DF_melted) <- c("FILL", "X", "value")

# Add code to make this a faceted plot, using the formula FILL ~ .
ggplot(DF_melted, aes(x = X, y = value, fill = FILL)) +
  geom_col(position = "stack") +
  BMI_fill + 
  facet_grid(FILL ~ .) 

#So far, I've looked at different ways of showing the frequency distribution within each BMI category.
# Now, I want to create a rectangle plot.

# The contingency table is transformed into a data frame
DF <- as.data.frame.matrix(table(adult$SRAGE_P, adult$RBMI))

# To build the rectangle plot, need to add variables to the data frame, such as groupSum, xmax and xmin columns
DF$groupSum <- rowSums(DF)
DF$xmax <- cumsum(DF$groupSum)
DF$xmin <- DF$xmax-DF$groupSum

# The groupSum column needs to be removed
DF$groupSum <- NULL

# Need to store the row names in a new variable X
DF$X <- rownames(DF)

# Melt the dataset, using the reshape package
library(reshape2)
DF_melted <- melt(DF, id.vars = c("X", "xmin", "xmax"), variable.name = "FILL")

# use dplyr call to calculate ymin and ymax - don't change
library(dplyr)
DF_melted <- DF_melted %>%
  group_by(X) %>%
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))

# Plot the rectangles - don't change
library(ggthemes)
ggplot(DF_melted, aes(ymin = ymin,
                      ymax = ymax,
                      xmin = xmin,
                      xmax = xmax,
                      fill = FILL)) +
  geom_rect(colour = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  BMI_fill +
  theme_tufte()

#Adding statistics

# To get Pearson residuals, perform chi.sq test (RBMI and SRAGE_P)
results <- chisq.test(table( adult$RBMI,adult$SRAGE_P))

# Melt results$residuals and store as resid
resid <- melt(results$residuals)

# Change names of resid
names(resid) <- c("FILL", "X", "residual")

# merge the two datasets:
DF_all <- merge(DF_melted, resid)

#Adding text
#have to add group and x-axis labels manually

#plot so far, saved as object p
p


# Position for labels on y axis 
index <- DF_all$xmax == max(DF_all$xmax)
DF_all$yposn <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2

# Plot 1: add geom_text for BMI (i.e. the fill axis)
p1 <- p %+% DF_all + 
  geom_text(aes(x = max(xmax), 
                y = yposn,
                label = FILL),
            size = 3, hjust = 1,
            show.legend  = FALSE)

p1


# Plot 2: add Position for labels on x axis
DF_all$xposn <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2

# Add geom_text for ages (i.e. the x axis)
p1 %+% DF_all + 
  geom_text(aes(x = xposn, label = X),
            y = 1, angle = 90,
            size = 3, hjust = 1,
            show.legend = FALSE)

#Generalizations
#Now I can wrap all the steps into a single function that I can use to examine any 
#two variables of interest in the data frame 

# Script generalized into a function
mosaicGG

# See mosaic plot of BMI described by age (as previously seen)
mosaicGG(adult, X = "SRAGE_P", FILL = "RBMI")

# see a mosaic plot of Poverty described by age
mosaicGG(adult, X = "SRAGE_P", FILL = "POVLL")
