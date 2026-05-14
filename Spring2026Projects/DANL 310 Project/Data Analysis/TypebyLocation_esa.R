# library
library(ggplot2)         
library(hrbrthemes)
#install.packages("ggthemes")
library(ggthemes)
library(RColorBrewer)

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
ID_freq_comp$Inhabitant<- factor(ID_freq_comp$Inhabitant,
                                 levels = c("Chrysis cessata", "Coelioxys alternatus", "Isodontia mexicana", "Ancistrocerus", "Euodynerus foraminatus", "Trypoxylon lactitarse",
                                            "Megachile pugnata", "Megachile rotundata", "Megachile relativa", "Osmia caerulescens"))
ID_freq_comp <- merge(ID_freq_comp, sample_sizes, by = "Location", all.x = TRUE)
graph = ggplot(ID_freq_comp, aes(fill=Inhabitant, y=Proportion_of_Fill, x=Location)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  #scale_fill_manual(values = c("navy","lightgreen","darkred","darkgrey","darkgreen","yellow","orange","lightyellow", "lightblue", "lightpink")) +
  scale_fill_manual(
    breaks = c("Chrysis cessata", "Coelioxys alternatus", "Isodontia mexicana", "Ancistrocerus", "Euodynerus foraminatus", "Trypoxylon lactitarse",
               "Megachile pugnata", "Megachile rotundata", "Megachile relativa", "Osmia caerulescens"),
    values = c(
      "Chrysis cessata" = "#480154FF", 
      "Coelioxys alternatus" = "#481567FF", 
      "Isodontia mexicana" = "#1F968BFF", 
      "Ancistrocerus" = "#287D8EFF", 
      "Euodynerus foraminatus" = "#33638DFF", 
      "Trypoxylon lactitarse" = "#404788FF",
      "Megachile pugnata" = "#55C667FF", 
      "Megachile rotundata" = "lightgreen", 
      "Megachile relativa" = "#FDE725FF",
      "Osmia caerulescens" = "#DCE319FF"
    )
  )+
  labs(title = "Proportion of Inhabitant Types by Location", x = "Location", y = "Proportion", fill = "Inhabitant") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="fill", stat="identity", col = "white") +
  #For scaling the tickmarks along the axes
  scale_y_continuous(breaks=seq(0,1,by=0.1))+
  theme_minimal()

for (i in 1:nrow(sample_sizes)) {
  graph <- graph + annotate("text", x = sample_sizes$Location[i], y = -0.04, label = sample_sizes$Sample_Size[i], vjust = 1.5, size = 3.6)
}
print(graph)

##Sophia's mod for  2025 fill type#####################################################################################
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

######Sophias Clean 2025 Fill Proportion##########################################################################################
library(ggthemes)
ID_freq_comp = read.csv("Proportion_of_Fill_By_Location - 2025-2026.csv")
sample_sizes = data.frame(Location = c("Arboretum", "ISC/Greenhouse", "No Mow Zone", "EGarden", "Island Preserve"),
                          Sample_Size = c(140, 40, 20, 20, 160))

ID_freq_comp$Fill_Type <- factor(ID_freq_comp$Fill_Type,
                                 levels = c("Empty", "Debris", "Silk", "Grass", "Light_Mud",
                                            "Intact_Leaves", "Masticated_Leaves"))
#Previous step allows you to pick the order of the variables
ID_freq_comp <- merge(ID_freq_comp, sample_sizes, by = "Location", all.x = TRUE)
graph = ggplot(ID_freq_comp, aes(fill=Fill_Type, y=Proportion_of_Fill, x=Location)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(breaks = c("Empty", "Debris", "Silk", "Grass","Light_Mud",
                               "Intact_Leaves", "Masticated_Leaves"),
    values = c(
    Empty = "black",
    Debris = "grey",
    Silk = "white",
    Grass = "#1F968BFF",
    Light_Mud = "#33638DFF",
    Intact_Leaves = "#95D840FF",
    Masticated_Leaves = "#DCE319FF")) +
  labs(title = "Proportion of Fill Types by Location", x = "Location", y = "Proportion", fill = "Fill_Type") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="fill", stat="identity", col = "black") +
  #For scaling the tickmarks along the axes
  scale_y_continuous(breaks=seq(0,1,by=0.1))+
  theme_minimal()

for (i in 1:nrow(sample_sizes)) {
  graph <- graph + annotate("text", x = sample_sizes$Location[i], y = -0.04, label = sample_sizes$Sample_Size[i], vjust = 1.5, size = 3.6)
}
print(graph)
#Used color blind friendly viridis_d
c("#FDE725FF","#29AF7FFF","#95D840FF","#33638DFF","#481567FF", "#DCE319FF")

####Sophia and Hanna type by Location###################################################
library(ggplot2)
ID_freq_2025_clean = read.csv("Bee_Occupants_for_ESA_poster_final.csv")
sample_sizes = data.frame(Location = c("Arboretum", "ISC/Greenhouse", "No Mow Zone", "EGarden", "Island Preserve"),
                          Sample_Size = c(140, 40, 20, 20, 160))
ID_freq_2025_clean$Inhabitant <- factor(ID_freq_2025_clean$Inhabitant,
                                 levels = c("Parasite", "Grass Wasp", "Potter Wasp", "Square-headed Wasp", "Unknown Wasp",
                                            "Leafcutter- Mp", "Leafcutter- Leaf", "Mason Bee", "Small Cell Bee"))
ggplot(ID_freq_2025_clean, aes(fill=Inhabitant, y=Proportion_of_Fill, x=Location)) +
  scale_fill_manual(breaks = c("Parasite", "Grass Wasp", "Potter Wasp", "Square-headed Wasp", "Unknown Wasp",
                               "Leafcutter- Mp", "Leafcutter- Leaf", "Mason Bee", "Small Cell Bee"), 
    values = c(
    "Parasite" = "#481567FF",
    "Grass Wasp" = "#1F968BFF",
    "Potter Wasp" = "#33638DFF",
    "Square-headed Wasp" = "#404788FF",
    "Unknown Wasp" = "white",
    "Leafcutter- Mp" = "#55C667FF",
    "Leafcutter- Leaf" = "#B8DE29FF",
    "Mason Bee" = "#DCE319FF",
    "Small Cell Bee" = "#FDE725FF"))+
  labs(title = "Proportion of Inhabitant Types by Location", x = "Location", y = "Proportion", fill = "Inhabitant") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="fill", stat="identity", col = "black") +
  #For scaling the tickmarks along the axes
  scale_y_continuous(breaks=seq(0,1,by=0.1))+
  theme_minimal()
  
#Used color blind friendly viridis_d
c("#FDE725FF","#29AF7FFF","#95D840FF","#33638DFF","#481567FF", "#DCE319FF", "#B8DE29FF", "#73D055FF")
#Other color blind friendly palettes that could be useful
#library(RColorBrewer)
#display.brewer.all(colorblindFriendly = TRUE)
#library(ggthemes)
#scale_color_colorblind()