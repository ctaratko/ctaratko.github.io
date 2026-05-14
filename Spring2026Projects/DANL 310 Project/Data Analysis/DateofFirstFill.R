# library
#install.packages(ggplot2)
library(ggplot2)

# create a dataset
  #Big label on the bottom, for each "rep", the first thing before the comma is what to call it (in this case week number but it could be a label too) and then the second is the number of catagories on the side
Week <- c(rep(1 , 5) , rep(2 , 5) , rep(3 , 5) , rep(4 , 5) , rep(5 , 5) , rep(6 , 5) , rep(7 , 5) , rep(8 , 5) , rep(9 , 5) , rep(10 , 5) , rep(11 , 5) , rep(12 , 5) , rep(13 , 5) , rep(14 , 5) , rep(15 , 5) , rep(16 , 5) , rep(17 , 5) , rep(18 , 5) , rep(19 , 5) , rep(20 , 5))
  #The different types of bars (will be on the key on the side), the number at the end is the number of reps above
Fill_Type <- rep(c("White Debris" , "Masticated Leaves" , "Light Mud" , "Leaves" , "Grass") , 20)
  #whatever the x axis is, list the data points in order grouped by that. Within each group, they should be ordered in the order that the fill categories are imputed in. So for example, since weeks are on the bottom, list week ones data point first, in the different types in order above, then do it for week two, and so on.
Number_of_Tubes_First_Filled <- c(0,0,0,0,0,0,0,0,0,0,1,0,4,0,0,0,2,1,0,0,1,2,1,0,0,7,4,5,0,0,2,8,4,0,0,0,1,3,0,3,0,6,5,0,2,0,0,6,0,3,1,0,6,1,2,1,0,1,7,0,0,0,0,14,2,1,0,0,11,7,0,0,0,9,1,0,0,0,8,2,0,0,0,4,4,0,0,0,3,2,0,0,0,0,1,0,0,0,0,0)
data <- data.frame(Week,Fill_Type,Number_of_Tubes_First_Filled)

# Grouped
ggplot(data, aes(fill=Fill_Type, y=Number_of_Tubes_First_Filled, x=Week)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = c("orange","lightgreen","lightyellow","limegreen","grey")) +
  labs(title = "Figure 1: Date of First Fill by Type", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="stack", stat="identity") +
  #For scaling the tickmarks along the axes
  scale_x_continuous(breaks=seq(0,20,by=1)) +
  scale_y_continuous(breaks=seq(0,20,by=1))
