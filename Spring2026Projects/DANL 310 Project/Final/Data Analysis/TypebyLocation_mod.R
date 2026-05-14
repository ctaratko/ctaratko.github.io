# library
library(ggplot2)
library(hrbrthemes)

# create a dataset
#Big label on the bottom, for each "rep", the first thing before the comma is what to call it (in this case week number but it could be a label too) and then the second is the number of catagories on the side
Location <- c(rep("Arboretum" , 12),rep("Bee Barn" , 12),rep("Meadow" , 12),rep("No Mow Zone" , 12),rep("eGarden" , 12))
#The different types of bars (will be on the key on the side), the number at the end is the number of reps above
Inhabitant <- rep(c("Grass-Carrying Wasp" , "Potter Wasp" , "Peg-Shaped Sphecid" , "Leafcutter 1" , "Leafcutter 3","Ants","Spiders","Leafcutter 2","Other Parasites", "Fly Pupa", "Empty","Other") , 5)
#whatever the x axis is, list the data points in order grouped by that. Within each group, they should be ordered in the order that the fill categories are imputed in. So for example, since weeks are on the bottom, list week ones data point first, in the different types in order above, then do it for week two, and so on.
Proportion_of_Fill <- c(0,0.025,0,0,0,0.075,0.1,0,0,0.075,0.725,0,
                        0.09166667,0.11666667,0.08333333,0.43333333,0,0.025,0.01666667,0,0.01666667,0.00833333,0.20833333,0,
                        0.225,0,0,0.025,0,0.3,0.05,0,0,0.05,0.35,0,
                        0.15,0.025,0,0.2,0.125,0,0.1,0.05,0,0,0.35,0,
                        0,0.1,0,0,0,0.05,0.1,0,0,0,0.55,0.2)
data2 <- data.frame(Location,Inhabitant,Proportion_of_Fill)
#doesnt do anything yet im trying to reorder the key but don't know how yet so for now this works
KeyOrder <- c('Empty', 'Other', 'Ants','Other Parasites','Fly Pupa','Spiders','Grass Wasp','Naked Larva','Peg-Shaped Pupa','Leafcutter','Green-fill Leafcutter','Leaf Tube Pupa')
# Grouped
ggplot(data2, aes(fill=Inhabitant, y=Proportion_of_Fill, x=Location)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = c("grey","white","black","orange","lightgreen","darkgreen","limegreen","lightgrey","darkgrey","yellow","lightyellow","brown")) +
  labs(title = "Figure 2: Proportion of Inhabitant Types by Location", x = "Location", y = "Proportion", fill = "Inhabitant") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="fill", stat="identity") +
  #For scaling the tickmarks along the axes
  scale_y_continuous(breaks=seq(0,1,by=0.1))

####################################################
#Daniel's original code above


## mod to run with data file
ID_freq = read.csv("Actual andPresumed ID.csv")
  
ggplot(ID_freq, aes(fill=Inhabitant, y=Proportion_of_Fill, x=Location)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = c("navy","lightyellow","orange","lightgrey","brown","darkgreen","pink","lightblue","black","lightgreen", "yellow", "red", "purple", "blue", "darkgreen", "limegreen", "darkgrey", "hotpink")) +
  labs(title = "Figure 2: Proportion of Inhabitant Types by Location", x = "Location", y = "Proportion", fill = "Inhabitant") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="fill", stat="identity", col = "black") +
  #For scaling the tickmarks along the axes
  scale_y_continuous(breaks=seq(0,1,by=0.1))

##########################Apple csv##################################################
ID_freq_comp = read.csv("Actual_presumed_complete.csv")
sample_sizes = data.frame(Location = c("Arboretum", "BeeBarn", "EGarden", "Meadow", "NoMowZone"),
                           Sample_Size = c(2, 95, 7, 14, 49))
ID_freq_comp <- merge(ID_freq_comp, sample_sizes, by = "Location", all.x = TRUE)
graph = ggplot(ID_freq_comp, aes(fill=Inhabitant, y=Proportion_of_Fill, x=Location)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = c("navy","lightgreen","darkred","darkgrey","darkgreen","yellow","orange","lightyellow", "lightblue", "lightpink")) +
  labs(title = "Figure 2: Proportion of Inhabitant Types by Location", x = "Location", y = "Proportion", fill = "Inhabitant") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="fill", stat="identity", col = "black") +
  #For scaling the tickmarks along the axes
  scale_y_continuous(breaks=seq(0,1,by=0.1))

for (i in 1:nrow(sample_sizes)) {
  graph <- graph + annotate("text", x = sample_sizes$Location[i], y = -0.04, label = sample_sizes$Sample_Size[i], vjust = 1.5, size = 3.6)
}
print(graph)

##Sophia's mod for fill type#####################################################################################
ID_freq_comp = read.csv("Proportion_of_Fill_By_Location - Sheet1.csv")
sample_sizes = data.frame(Location = c("Arboretum", "ISC/Greenhouse", "No Mow Zone", "EGarden", "Island Preserve"),
                          Sample_Size = c(140, 40, 20, 20, 160))
ID_freq_comp <- merge(ID_freq_comp, sample_sizes, by = "Location", all.x = TRUE)
graph = ggplot(ID_freq_comp, aes(fill=Fill_Type, y=Proportion_of_Fill, x=Location)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = c("lightyellow","black","darkgreen","lightgreen","grey","orange","yellow")) +
  labs(title = "Figure 2: Proportion of Fill Types by Location", x = "Location", y = "Proportion", fill = "Fill_Type") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="fill", stat="identity", col = "black") +
  #For scaling the tickmarks along the axes
  scale_y_continuous(breaks=seq(0,1,by=0.1))

for (i in 1:nrow(sample_sizes)) {
  graph <- graph + annotate("text", x = sample_sizes$Location[i], y = -0.04, label = sample_sizes$Sample_Size[i], vjust = 1.5, size = 3.6)
}
print(graph)

######Sophias Clean 2025 Bees##########################################################################################
library(ggthemes)
ID_freq_comp = read.csv("Proportion_of_Fill_By_Location - 2025-2026.csv")
sample_sizes = data.frame(Location = c("Arboretum", "ISC/Greenhouse", "No Mow Zone", "EGarden", "Island Preserve"),
                          Sample_Size = c(140, 40, 20, 20, 160))
ID_freq_comp <- merge(ID_freq_comp, sample_sizes, by = "Location", all.x = TRUE)
graph = ggplot(ID_freq_comp, aes(fill=Fill_Type, y=Proportion_of_Fill, x=Location)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = c("#F0E442","black","#56B4E9","#009E73","grey","#E69F00","yellow")) +
  labs(title = "Figure 2: Proportion of Fill Types by Location", x = "Location", y = "Proportion", fill = "Fill_Type") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="fill", stat="identity", col = "black") +
  #For scaling the tickmarks along the axes
  scale_y_continuous(breaks=seq(0,1,by=0.1))

for (i in 1:nrow(sample_sizes)) {
  graph <- graph + annotate("text", x = sample_sizes$Location[i], y = -0.04, label = sample_sizes$Sample_Size[i], vjust = 1.5, size = 3.6)
}
print(graph)+
  scale_color_colorblind()


| Hex Code  | Color Blind Pallette Color    |
  | --------- | ----------------------------- |
  | `#000000` | Black                         |
  | `#E69F00` | Orange (golden/orange)        |
  | `#56B4E9` | Sky Blue                      |
  | `#009E73` | Bluish Green (teal-green)     |
  | `#F0E442` | Yellow                        |
  | `#0072B2` | Blue (strong/deep blue)       |
  | `#D55E00` | Vermillion (reddish-orange)   |
  | `#CC79A7` | Reddish Purple (magenta-like) |
  


