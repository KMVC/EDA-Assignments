# titanic was avaliable in the Datacamp workspace
#the goal of this exercise was to employ ggplot on the titanic data set to investigate whether I might have 
#survived the crash of the Titanic

# 1 - Check the structure of the titanic data set
str(titanic)

# 2 - Use ggplot() to plot the distribution of sexes within the classes of the ship, creating a dodged bar chart
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")

# 3 - the code above gives insight into sexes within classes, but I need to add data concerning survival
#I do this by adding facet_grid() layer to the plot, and employing the variable "survived" within facet_grid
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")+ facet_grid(.~Survived)

# 4 - Define an object for position jitterdodge, to use in 5th step
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# 5 - Want to add age and also ensure that individual dots do not overlap by using the jitterdodge object from Step 4
# For this step, I use Plot 3, but make y = Age, add the position object from step 4, so that a plot is generated 
#where one can see who survived and didn't by age, sex and class
ggplot(titanic, aes(x = Pclass, y = Age, color = Sex)) +
  geom_point(size = 3, alpha = 0.5, position = posn.jd)+ facet_grid(.~Survived)
