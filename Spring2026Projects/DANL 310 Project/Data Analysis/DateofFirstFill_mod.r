# library
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

######################################################
#Daniel's original code above



##JA altered to work with data file - remove white debris from file
fill_2023=read.csv("2023_bee tube first fill data_no WF.csv")
fill_2023 = na.omit(fill_2023)

#must change how refer to variables  first_fill$Week, etc
firs
# Grouped
#####Code for 2023 FillType No White Debris###############################################

ggplot(fill_2023, aes(fill=Fill_Type, y=Number_of_Tubes_First_Filled, x=Week)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = c("darkgreen","lightgreen","grey","orange")) +
  labs(title = "Figure 1a. Date of First Fill in 2023 by Type", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="stack", stat="identity", col = "black") +
  #For scaling the tickmarks along the axes
  scale_x_continuous(breaks=seq(0,20,by=1)) +
  scale_y_continuous(breaks=seq(0,27,by=1))

#####Sophias Code for 2024 Fill Type##########################################################
library(ggplot2)

fill_2024=read.csv("Week_of_First_FIll_for_CSV - Sheet1.csv")
ggplot(fill_2024, aes(fill=Fill_Type, y=Number_of_Tubes_First.Filled, x=Week)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = c("#FDE725FF","#29AF7FFF","#33638DFF","#481567FF","#DCE319FF")) +
  labs(title = "Figure 1b. Date of First Fill in 2024 by Type", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="stack", stat="identity", col = "black") +
  #For scaling the tickmarks along the axes
  scale_x_continuous(breaks=seq(0,20,by=1)) +
  scale_y_continuous(breaks=seq(0,27,by=1))+
  theme_minimal()
#Viridis_d colors used- colorblind friendly palette
c("#FDE725FF","#29AF7FFF","#95D840FF","#33638DFF","#481567FF", "#DCE319FF")

#####Sophias Code for 2025 Fill Type##########################################################
library(ggplot2)
fill_2025=read.csv("Week_of_First_Fill_2025 - Sheet1.csv")
ggplot(fill_2025, aes(fill=Fill_Type, y=Number_of_Tubes_First.Filled, x=Week)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = c("lightyellow","darkgreen","lightgreen","grey","orange", "yellow")) +
  labs(title = "Figure 1b. Date of First Fill in 2025 by Type", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="stack", stat="identity", col = "black") +
  #For scaling the tickmarks along the axes
  scale_x_continuous(breaks=seq(0,20,by=1)) +
  scale_y_continuous(breaks=seq(0,27,by=1))


######Sophias Code for 2025 Fill Type Clean ########################################################
library(ggplot2)
fill_2025_clean=read.csv("Week_of_First_FIll_2025 - 2026_cleaned.csv")
ggplot(fill_2025_clean, aes(fill=Fill_Type, y=Number_of_Tubes_First.Filled, x=Week)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = c("#FDE725FF","#29AF7FFF","#95D840FF","#33638DFF","#481567FF", "#DCE319FF")) +
  labs(title = "Figure 1c. Date of First Fill in 2025 by Type", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="stack", stat="identity", col = "black") +
  #For scaling the tickmarks along the axes
  scale_x_continuous(breaks=seq(0,20,by=1)) +
  scale_y_continuous(breaks=seq(0,27,by=1))+
  theme_minimal()


#Other color blind friendly palettes that could be useful
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)

#####Sophia's Edit of Daniel's 2023 Code to Match Color Code##################################
library(ggplot2)
Week <- c(rep(1 , 5) , rep(2 , 5) , rep(3 , 5) , rep(4 , 5) , rep(5 , 5) , rep(6 , 5) , rep(7 , 5) , rep(8 , 5) , rep(9 , 5) , rep(10 , 5) , rep(11 , 5) , rep(12 , 5) , rep(13 , 5) , rep(14 , 5) , rep(15 , 5) , rep(16 , 5) , rep(17 , 5) , rep(18 , 5) , rep(19 , 5) , rep(20 , 5))
#The different types of bars (will be on the key on the side), the number at the end is the number of reps above
Fill_Type <- rep(c("White Debris" , "Masticated Leaves" , "Light Mud" , "Leaves" , "Grass") , 20)
#whatever the x axis is, list the data points in order grouped by that. Within each group, they should be ordered in the order that the fill categories are imputed in. So for example, since weeks are on the bottom, list week ones data point first, in the different types in order above, then do it for week two, and so on.
Number_of_Tubes_First_Filled <- c(0,0,0,0,0,0,0,0,0,0,1,0,4,0,0,0,2,1,0,0,1,2,1,0,0,7,4,5,0,0,2,8,4,0,0,0,1,3,0,3,0,6,5,0,2,0,0,6,0,3,1,0,6,1,2,1,0,1,7,0,0,0,0,14,2,1,0,0,11,7,0,0,0,9,1,0,0,0,8,2,0,0,0,4,4,0,0,0,3,2,0,0,0,0,1,0,0,0,0,0)
data <- data.frame(Week,Fill_Type,Number_of_Tubes_First_Filled)

# Grouped
ggplot(data, aes(fill=Fill_Type, y=Number_of_Tubes_First_Filled, x=Week)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = c("#009E73","#95D840FF","#33638DFF","#481567FF","#FDE725FF")) +
  labs(title = "Figure 1a: Date of First Fill in 2023 by Type", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="stack", stat="identity", color = "black") +
  #For scaling the tickmarks along the axes
  scale_x_continuous(breaks=seq(0,20,by=1)) +
  scale_y_continuous(breaks=seq(0,20,by=1))+
  theme_minimal()


c("#FDE725FF","#29AF7FFF","#95D840FF","#33638DFF","#481567FF", "#DCE319FF")


