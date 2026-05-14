# Libraries & Packages


library(tidyverse)
library(ggthemes)
library(ggrepel)
library(hrbrthemes)
library(RColorBrewer)
# install.packages("ggtext")
library(ggtext)
# library(annotate)
# library(dplyr)


## Data Collection


files <- list.files(path = "/Users/christophertaratko/ProjectsList/Data Analytics/DANL 310 Project/Final/Data Analysis", pattern = "*.csv", full.names = TRUE)

for (k in files) {
  obj_name <- tools::file_path_sans_ext(basename(k))
  print(obj_name)
  assign(obj_name, read_csv(k, show_col_types = F))
  print(colnames(get(obj_name)))
}
rm(f, files, obj_name)

setwd("/Users/christophertaratko/ProjectsList/Data Analytics/DANL 310 Project/Final/Data Analysis")
## Type By Location

fill_2023 <-  read.csv("2023_bee tube first fill data_no WF.csv")
fill_2023 = na.omit(fill_2023)
fill_2024=read.csv("Week_of_First_FIll_for_CSV - Sheet1.csv")
fill_2025=read.csv("Week_of_First_Fill_2025 - Sheet1.csv")

my_colors <- c(
  "Grass"              = "darkgreen", 
  "Leaves"             = "#95D840FF",
  "Intact_Leaves"      = "#29AF7FFF",
  "Masticated_Leaves"  = "#FDE725FF", 
  "Light_Mud"          = "#33638DFF",
  "Debris"             = "grey",
  "Silk"               = "white"
)

my_labels <- c(
  "Grass" = "Grass",
  "Leaves" = "Leaves",
  "Intact_Leaves" = "Intact Leaves",
  "Masticated_Leaves" = "Masticated Leaves",
  "Light_Mud" = "Light Mud",
  "Debris" = "Debris",
  "Silk" = "Spider Silk"
    )

mytheme <- theme(
  plot.title = element_text(face = "bold", family = "Times New Roman", size = 20),
  axis.title = element_text(face = "italic", family = "Times New Roman"),
  axis.text = element_text(family = "Times New Roman"),
  plot.background = element_rect(fill = "white"),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = NA, color = "black"),
  # axis.line.y = element_line(color = "black", linewidth = 0.5),
  panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5), 
  panel.grid.major.x = element_blank(),
  axis.line.y = element_line(color = "black", linewidth = 0.5),
  legend.title = element_text(family = "Times New Roman", face = "bold"),
  
  # for faceted graphs
  strip.background = element_rect(fill = "grey95", color = "black"),
  strip.text = element_text(color = "black", face = "bold"),
)





Location <- c(rep("Arboretum" , 12),rep("Bee Barn" , 12),rep("Meadow" , 12),rep("No Mow Zone" , 12),rep("eGarden" , 12))

Inhabitant <- rep(c("Grass-Carrying Wasp" , "Potter Wasp" , "Peg-Shaped Sphecid"
                    ,"Leafcutter1" , "Leafcutter 3","Ants","Spiders",
                    "Leafcutter 2","Other Parasites","Fly Pupa", "Empty","Other")
                  , 5)

Proportion_of_Fill <- c(0,0.025,0,0,0,0.075,0.1,0,0,0.075,0.725,0,
                        0.09166667,0.11666667,0.08333333,0.43333333,0,0.025,
                        0.01666667,0,0.01666667,0.00833333,0.20833333,0,
                        0.225,0,0,0.025,0,0.3,0.05,0,0,0.05,0.35,0,
                        0.15,0.025,0,0.2,0.125,0,0.1,0.05,0,0,0.35,0,
                        0,0.1,0,0,0,0.05,0.1,0,0,0,0.55,0.2)
data2 <- data.frame(Location,Inhabitant,Proportion_of_Fill)
KeyOrder <- c('Empty', 'Other', 'Ants','Other Parasites','Fly Pupa','Spiders',
              'Grass Wasp'
              ,'Naked Larva','Peg-Shaped Pupa','Leafcutter',
              'Green-fill Leafcutter','Leaf Tube 
              Pupa')
ggplot(data2, aes(fill=Inhabitant, y=Proportion_of_Fill, x=Location)) +
  scale_fill_manual(values = c("grey","white","black","orange","lightgreen","darkgreen"
                               ,"limegreen","lightgrey","darkgrey","yellow","lightyellow"
                               ,"brown")) +
  labs(title = "Figure 2: Proportion of Inhabitant Types by Location", x = "Location", y = 
         "Proportion", fill = "Inhabitant") +
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(breaks=seq(0,1,by=0.1))


####################################################
#Daniel's original code above


## mod to run with data file
ID_freq = read.csv("Actual andPresumed ID.csv")

ggplot(ID_freq, aes(fill=Inhabitant, y=Proportion_of_Fill, x=Location)) +
  scale_fill_manual(values = c("navy","lightyellow","orange","lightgrey","brown","darkgreen"
                               ,"pink","lightblue","black","lightgreen", "yellow", "red", 
                               "purple", "blue", "darkgreen", "limegreen", "darkgrey", 
                               "hotpink")) +
  labs(title = "Figure 2: Proportion of Inhabitant Types by Location", x = "Location", y = 
         "Proportion", fill = "Inhabitant") +
  geom_bar(position="fill", stat="identity", col = "black") +
  scale_y_continuous(breaks=seq(0,1,by=0.1))

##########################Apple csv##################################################
ID_freq_comp = read.csv("Actual_presumed_complete.csv")
sample_sizes = data.frame(Location = c("Arboretum", "BeeBarn", "EGarden", "Meadow", "NoMowZone"
),
Sample_Size = c(2, 95, 7, 14, 49))
ID_freq_comp$Inhabitant<- factor(ID_freq_comp$Inhabitant,
                                 levels = c("Chrysis cessata", "Coelioxys alternatus", 
                                            "Isodontia mexicana", "Ancistrocerus", "Euodynerus 
                                            foraminatus", "Trypoxylon lactitarse",
                                            "Megachile pugnata", "Megachile rotundata", 
                                            "Megachile relativa", "Osmia caerulescens"))
ID_freq_comp <- merge(ID_freq_comp, sample_sizes, by = "Location", all.x = TRUE)
graph = ggplot(ID_freq_comp, aes(fill=Inhabitant, y=Proportion_of_Fill, x=Location)) +
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
                                        levels = c("Parasite", "Grass Wasp", 
                                                   "Potter Wasp", "Square-headed Wasp", 
                                                   "Unknown Wasp","Leafcutter- Mp", 
                                                   "Leafcutter- Leaf", "Mason Bee", 
                                                   "Small Cell Bee"))
ggplot(ID_freq_2025_clean, aes(fill=Inhabitant, y=Proportion_of_Fill, x=Location)) +
  scale_fill_manual(breaks = c("Parasite", "Grass Wasp", "Potter Wasp", "Square-headed Wasp",
                               "Unknown Wasp",
                               "Leafcutter- Mp", "Leafcutter- Leaf", "Mason Bee", "Small Cell
                               Bee"), 
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
  labs(title = "Proportion of Inhabitant Types by Location", x = "Location", y = "Proportion",
       fill = "Inhabitant") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="fill", stat="identity", col = "black") +
  #For scaling the tickmarks along the axes
  scale_y_continuous(breaks=seq(0,1,by=0.1))+
  theme_minimal()

#Used color blind friendly viridis_d
c("#FDE725FF","#29AF7FFF","#95D840FF","#33638DFF","#481567FF", "#DCE319FF", "#B8DE29FF", "#73D055FF")


## Date of First Fill


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


## Fill Type


library(tidyverse)

fill_inhabitant_labels <- tibble(
  Fill_Type = c(
    "Grass",
    "Grass",
    "Grass",
    "Masticated_Leaves",
    "Masticated_Leaves",
    "Intact_Leaves",
    "Intact_Leaves",
    "Light_Mud",
    "Silk",
    "Debris",
    "Empty"
  ),
  Inhabitant = c(
    "Grass Wasp",
    "Square-headed Wasp",
    "Potter Wasp",
    "Leafcutter- Mp",
    "Leafcutter- Leaf",
    "Leafcutter- Mp",
    "Leafcutter- Leaf",
    "Mason Bee",
    "Mason Bee",
    "Parasite",
    NA_character_
  ),
  label_note = c(
    "Primary grass user",
    "Common in grass-filled nests",
    "Sometimes uses grass",
    "Primary masticated leaf user",
    "Uses leaf cuttings",
    "Also uses intact leaves",
    "Uses intact leaf cuttings",
    "Primary mud user",
    "Uses silk as plug/lining",
    "Associated with debris-filled cells",
    "No known primary inhabitant"
  )
)

# label_positions <- fill_2023 %>%
#   group_by(Week) %>%
#   arrange(Week, Fill_Type) %>%
#   mutate(
#     ymax = cumsum(Number_of_Tubes_First_Filled),
#     ymin = ymax - Number_of_Tubes_First_Filled,
#     ymid = (ymax + ymin) / 2
#   ) %>%
#   ungroup() %>%
#   # Join your inhabitant label lookup
#   left_join(fill_inhabitant_labels, by = "Fill_Type") %>%
#   # Only label segments large enough to be readable (adjust threshold as needed)
#   filter(Number_of_Tubes_First_Filled >= 2, !is.na(Inhabitant))

# Collapse multiple inhabitants per fill type into one label
fill_labels_collapsed <- fill_inhabitant_labels %>%
  filter(!is.na(Inhabitant)) %>%
  group_by(Fill_Type) %>%
  summarise(Inhabitant = paste(Inhabitant, collapse = "\n"), .groups = "drop")

# Now the join is clean — one row per Fill_Type
label_positions <- fill_2023 %>%
  group_by(Week) %>%
  arrange(Week, Fill_Type) %>%
  mutate(
    ymax = cumsum(Number_of_Tubes_First_Filled),
    ymin = ymax - Number_of_Tubes_First_Filled,
    ymid = (ymax + ymin) / 2
  ) %>%
  ungroup() %>%
  left_join(fill_labels_collapsed, by = "Fill_Type") %>%
  filter(Number_of_Tubes_First_Filled >= 2, !is.na(Inhabitant))



ggplot(fill_2023, aes(fill = Fill_Type,
                      y = Number_of_Tubes_First_Filled,
                      x = Week)) +
  scale_fill_manual(values = my_colors, labels = my_labels) +
  labs(
    title = "Figure 1a. Date of First Fill in 2023 by Type",
    x = "Week", y = "Number of Tubes First Filled",
    fill = "Fill Type"
  ) +
  geom_bar(position = "stack", stat = "identity", col = "black") +
  # Dynamic labels from label_positions
  geom_text(
    data = label_positions,
    aes(x = Week, y = ymid, label = Inhabitant),
    inherit.aes = FALSE,
    size = 2.8,
    fontface = "italic",
    color = "black",
    fill = "white",# or "black" depending on fill darkness
    check_overlap = TRUE    # suppresses overlapping labels automatically
  ) +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  scale_y_continuous(breaks = seq(0, 27, by = 1)) +
  mytheme



ggplot(fill_2023, aes(fill = Fill_Type, 
                      y = Number_of_Tubes_First_Filled, 
                      x = Week)) +
  scale_fill_manual(values = my_colors, labels = my_labels) +
  labs(
    title = "Figure 1a. Date of First Fill in 2023 by Type",
    x = "Week", y = "Number of Tubes First Filled",
    fill = "Fill Type"
  ) +
  geom_bar(position = "stack", stat = "identity", col = "black") +
  
  # --- Mason Bee (Light_Mud) --- angled arrow, shifted left
  annotate("segment", x = 4, xend = 5, y = -1.5, yend = -0.2,
           arrow = arrow(length = unit(0.15, "cm")), color = "black") +
  annotate("text", x = 2, y = -2.5, label = "Mason Bee",
           size = 2.8, fontface = "italic", hjust = 0.5, vjust = 1) +
  
  # --- Leafcutter- Mp & Leafcutter- Leaf (Masticated_Leaves) ---
  annotate("segment", x = 7, xend = 7, y = 0, yend = -1.5,
           arrow = arrow(length = unit(0.15, "cm")), color = "black") +
  annotate("text", x = 7, y = -2.5, label = "Leafcutter- Mp\nLeafcutter- Leaf",
           size = 2.8, fontface = "italic", hjust = 0.5, vjust = 1) +
  
  # --- Grass Wasp, Square-headed, Potter (Grass, dark green) ---
  annotate("segment", x = 14, xend = 15, y = 0, yend = -1.5,
           arrow = arrow(length = unit(0.15, "cm")), color = "black") +
  annotate("text", x = 18, y = -1.5,
           label = "Grass Wasp\nSquare-headed Wasp\nPotter Wasp",
           size = 2.8, fontface = "italic", hjust = 0.5, vjust = 1) +
  
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  scale_y_continuous(breaks = seq(0, 27, by = 1)) +
  coord_cartesian(ylim = c(-6, 19), clip = "off") +
  mytheme +
  theme(plot.margin = margin(t = 5, r = 5, b = 60, l = 5, unit = "pt"))


# Element & Theme Corrections



# unique(fill_2025$Fill_Type)
# unique(fill_2024$Fill_Type)
# unique(fill_2023$Fill_Type)
fill_2023 <- fill_2023 %>% 
  mutate(Fill_Type = fct_relevel(Fill_Type, 
                                 "Grass", "Leaves", "Masticated_Leaves", 
                                 "Light_Mud", "Debris", "Silk"))
fill_2024 <- fill_2024 %>% 
  mutate(Fill_Type = fct_relevel(Fill_Type, 
                                 "Grass", "Leaves", "Masticated_Leaves", 
                                 "Light_Mud", "Debris", "Silk"))
fill_2025 <- fill_2025 %>% 
  mutate(Fill_Type = fct_relevel(Fill_Type, 
                                 "Grass", "Leaves", "Masticated_Leaves", 
                                 "Light_Mud", "Debris", "Silk"))
# Colors to use
# Define your color palette
unique(Fill_Type)







# Date Of First Fill Mod.r


##JA altered to work with data file - remove white debris from file

# Grouped
#####Code for 2023 FillType No White Debris###############################################
ggplot(fill_2023, aes(fill=Fill_Type, y=Number_of_Tubes_First_Filled, x=Week)) +
  scale_fill_manual(values = my_colors, labels = my_labels) +
  labs(title = "Figure 1a. Date of First Fill in 2023 by Type", 
       x = "Week", y = "Number of Tubes First Filled", 
       fill = "Fill Type")+
  geom_bar(position="stack", stat="identity", col = "black") +
  scale_x_continuous(breaks=seq(0,20,by=1)) +
  scale_y_continuous(breaks=seq(0,27,by=1)) +
  mytheme



### GGTEXT for Wasps/Bee Type


SpeciesFill <- c(
  "Grass Carrying Wasps" = "Grass",
  "Potter Wasps" = "Light_Mud",
  "Square Headed Wasps" = "Light_Mud"
)


#####Sophias Code for 2024 Fill Type##########################################################



ggplot(fill_2024, aes(fill=Fill_Type, y=Number_of_Tubes_First.Filled, x=Week)) +
  scale_fill_manual(values = my_colors, labels = my_labels) +
  labs(title = "Figure 1b. Date of First Fill in 2024 by Type", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
  geom_bar(position="stack", stat="identity", col = "black") +
  scale_x_continuous(breaks=seq(0,20,by=1)) +
  scale_y_continuous(breaks=seq(0,27,by=1))+
  theme_minimal() +
  mytheme +
  theme(
    plot.background = element_rect(color = "black")
  )
# c("#FDE725FF","#29AF7FFF","#95D840FF","#33638DFF","#481567FF", "#DCE319FF")



unique(Fill_Type)
#####Sophias Code for 2025 Fill Type##########################################################
library(ggplot2)

ggplot(fill_2025, aes(fill= Fill_Type, y=Number_of_Tubes_First.Filled, x=Week)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = my_colors,, labels = my_labels) +
  labs(title = "Figure 1b. Date of First Fill in 2025 by Type", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
  geom_bar(position="stack", stat="identity", col = "black") +
  scale_x_continuous(breaks=seq(0,20,by=1)) +
  scale_y_continuous(breaks=seq(0,27,by=1)) +
  mytheme



######Sophias Code for 2025 Fill Type Clean ########################################################
library(ggplot2)
fill_2025_clean=read.csv("Week_of_First_FIll_2025 - 2026_cleaned.csv")
ggplot(fill_2025_clean, aes(fill=Fill_Type, y=Number_of_Tubes_First.Filled, x=Week)) +
  #Colors below must be listed in alphabetical order of the categories in the key
  scale_fill_manual(values = my_colors,, labels = my_labels) +
  labs(title = "Figure 1c. Date of First Fill in 2025 by Type", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
  #To make it sidebyside, change position to "dodge" and to make it a percentage change position to "fill", stacked is "stack" 
  geom_bar(position="stack", stat="identity", col = "black") +
  #For scaling the tickmarks along the axes
  scale_x_continuous(breaks=seq(0,20,by=1)) +
  scale_y_continuous(breaks=seq(0,27,by=1))+
  theme_minimal() +
  mytheme +
  theme(
    plot.background = element_rect(color = "black")
  )




#Other color blind friendly palettes that could be useful
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)

#####Sophia's Edit of Daniel's 2023 Code to Match Color Code##################################
library(ggplot2)
Week <- c(rep(1 , 5) , rep(2 , 5) , rep(3 , 5) , rep(4 , 5) , rep(5 , 5) , rep(6 , 5) , rep(7 , 5) , rep(8 , 5) , rep(9 , 5) , rep(10 , 5) , rep(11 , 5) , rep(12 , 5) , rep(13 , 5) , rep(14 , 5) , rep(15 , 5) , rep(16 , 5) , rep(17 , 5) , rep(18 , 5) , rep(19 , 5) , rep(20 , 5))
#The different types of bars (will be on the key on the side), the number at the end is the number of reps above
Fill_Type <- rep(c("WhiteDebris" , "Masticated_Leaves" , "Light_Mud" , "Leaves" , "Grass") , 20)
#whatever the x axis is, list the data points in order grouped by that. Within each group, they should be ordered in the order that the fill categories are imputed in. So for example, since weeks are on the bottom, list week ones data point first, in the different types in order above, then do it for week two, and so on.
Number_of_Tubes_First_Filled <- c(0,0,0,0,0,0,0,0,0,0,1,0,4,0,0,0,2,1,0,0,1,2,1,0,0,7,4,5,0,0,2,8,4,0,0,0,1,3,0,3,0,6,5,0,2,0,0,6,0,3,1,0,6,1,2,1,0,1,7,0,0,0,0,14,2,1,0,0,11,7,0,0,0,9,1,0,0,0,8,2,0,0,0,4,4,0,0,0,3,2,0,0,0,0,1,0,0,0,0,0)
data <- data.frame(Week,Fill_Type,Number_of_Tubes_First_Filled)

fill_2023<- data|>
  mutate(
    Fill_Type = ifelse(Fill_Type == "WhiteDebris", "Debris", Fill_Type),
    Fill_Type = ifelse(Fill_Type == "Masticated Leaves", "Masticated_Leaves", Fill_Type),
    Fill_Type = ifelse(Fill_Type == "Light Mud", "Light_Mud", Fill_Type))
fill_2023_cleaned <- fill_2023 |> 
  mutate(Year = 2023,
         Number_of_Tubes_First_Filled = ifelse(is.na(Number_of_Tubes_First_Filled), 0, Number_of_Tubes_First_Filled))

fill_2024=read.csv("Week_of_First_FIll_for_CSV - Sheet1.csv")
fill_2024_cleaned <- fill_2024 |> 
  mutate(Number_of_Tubes_First_Filled = Number_of_Tubes_First.Filled,
         Number_of_Tubes_First_Filled = ifelse(is.na(Number_of_Tubes_First_Filled), 0, Number_of_Tubes_First_Filled),
         Year = 2024)|>
  select(Week, Year, Number_of_Tubes_First_Filled, Fill_Type)

fill_2025_clean=read.csv("Week_of_First_FIll_2025 - 2026_cleaned.csv")
fill_2025_cleaned <- fill_2025_clean|> 
  mutate(Year = 2025,
         Number_of_Tubes_First_Filled = Number_of_Tubes_First.Filled,
         Number_of_Tubes_First_Filled = ifelse(is.na(Number_of_Tubes_First_Filled), 0, Number_of_Tubes_First_Filled))|>
  select(Week, Year, Number_of_Tubes_First_Filled, Fill_Type)

combined_df <- bind_rows(fill_2023_cleaned, fill_2024_cleaned, fill_2025_cleaned)

#view(combined_df)
names(combined_df)
combined_df<- combined_df|>
  mutate(
    Fill_Type = recode(Fill_Type,
                       "Leaves" = "Leaves",
                       "Intact_Leaves" = "Leaves"   # combine here
    ))|>
  filter(
    !is.na(Fill_Type)
  )
combined_df$Fill_Type <- factor(combined_df$Fill_Type,
                                levels = c("Debris", "Silk", "Grass", "Light_Mud", "Leaves",
                                           "Masticated_Leaves"))
combined_df<- combined_df|>
  filter(
    !is.na(Fill_Type)
  )

#Descriptive Stats
## Find total fill regardless of fill type for each week in each year
per_week_by_yr_stats<- combined_df|>
  group_by(Year, Week)|>
  summarise(
    fill_per_week = sum(Number_of_Tubes_First_Filled)
  )

##Find the average fill in those weeks
averages<- per_week_by_yr_stats|>
  group_by(Year)|>
  summarise(
    avg = mean(fill_per_week)
  )
print(averages)
mean(averages$avg)


#Plot

ggplot(combined_df, aes(x=Week, y=Number_of_Tubes_First_Filled,fill=Fill_Type)) +
  facet_wrap(~Year, scales = "free_x")+
  scale_fill_manual(values = my_colors, labels = my_labels) +
  # scale_fill_viridis_d() +
  labs(title = " Timing of First Fill by Year", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
  geom_bar(position="stack", stat="identity", color = "black", width = 1) +
  #For scaling the tickmarks along the axes
  scale_x_continuous(breaks=seq(0,20,by=2)) +
  scale_y_continuous(breaks=seq(0,20,by=2))+
  theme_minimal() +
  mytheme


# c("#FDE725FF","#29AF7FFF","#95D840FF","#33638DFF","#481567FF", "#DCE319FF")





## New Sophia Code



##################Sophias Data Project#####################################
# Faceting the first fill graphs
library(dplyr)
library(ggplot2)
library(tidyverse)

fill_2023$Year <- 2023
fill_2024$Year <- 2024
fill_2025_clean$Year <- 2025

fill_2024<- fill_2024|>
  rename(
    Number_of_Tubes_First_Filled = Number_of_Tubes_First.Filled
  )
fill_2025_clean<- fill_2025_clean|>
  rename(
    Number_of_Tubes_First_Filled = Number_of_Tubes_First.Filled
  )

combined_df <- bind_rows(fill_2023, fill_2024, fill_2025_clean)
# combined_df

ggplot(combined_df, aes(x=Week, y=Number_of_Tubes_First_Filled,fill=Fill_Type)) +
  facet_wrap(~Year, scales = "free_x")+
  scale_fill_manual(values = my_colors, labels = my_labels) +
  # scale_fill_viridis_d() +
  labs(title = " Timing of First Fill by Year", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
  geom_bar(position="stack", stat="identity", color = "black", width = 1) +
  #For scaling the tickmarks along the axes
  scale_x_continuous(breaks=seq(0,20,by=2)) +
  scale_y_continuous(breaks=seq(0,20,by=2))+
  theme_minimal() +
  mytheme





#| echo: false
#| fig-width: 8
#| fig-height: 6
library(tidyverse)
fill_df <- combined_df |>
  filter(
    Fill_Type == "Grass" |
      Fill_Type == "Leaves" |
      Fill_Type == "Light_Mud" |
      Fill_Type == "Masticated_Leaves"
  )
# ggplot(fill_df, aes(x=Week, y=Number_of_Tubes_First_Filled,fill= Fill_Type)) +
#   facet_wrap(~Fill_Type, scales = "free_x")+
#   scale_fill_manual(values = my_colors, labels = my_labels) +
#   # scale_fill_viridis_d() +
#   labs(title = " Timing of First Fill by Week", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
#   geom_bar(position="stack", stat="identity") +
#   #For scaling the tickmarks along the axes
#   scale_x_continuous(breaks=seq(0,20,by=1)) +
#   scale_y_continuous(breaks=seq(0,20,by=1))+
#   theme_minimal() +
#   mytheme +
#   theme(
#     axis.text = element_text(size = 8)
#   )


ggplot(fill_df,
       aes(x = Week,
           y = Number_of_Tubes_First_Filled,
           fill = Fill_Type)) +
  facet_wrap(
    ~Fill_Type,
    scales = "free_x",
    labeller = labeller(Fill_Type = c(
      "Light_Mud" = "Light Mud",
      "Masticated_Leaves" = "Masticated Leaves",
      "Intact_Leaves" = "Intact Leaves"
    ))
  ) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(
    values = my_colors,
    labels = c(
      "Grass" = "Grass",
      "Leaves" = "Leaves",
      "Light_Mud" = "Light Mud",
      "Masticated_Leaves" = "Masticated Leaves"
    )
  ) +
  labs(
    title = "Timing of First Fill by Fill Type: Summary Across Years",
    x = "Week",
    y = "Number of Tubes First Filled",
    fill = "Fill Type"
  ) +
  scale_x_continuous(breaks = seq(0, 20, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  theme_minimal() +
  mytheme +
  theme(
    axis.text = element_text(size = 8)
  )






#Number of Major Fill Types
# fig-height: 7
# fig-hwidth: 6
combined_df_no_na<- combined_df|>
  filter(!is.na(Number_of_Tubes_First_Filled))
combined_df_no_na


total_filled <- sum(combined_df_no_na$Number_of_Tubes_First_Filled)
grass<- sum(combined_df_no_na$Number_of_Tubes_First_Filled[Fill_Type == "Grass"])
leaves<- sum(combined_df_no_na$Number_of_Tubes_First_Filled[Fill_Type == "Leaves"])
# light_mud<- sum(combined_df_no_na$Number_of_Tubes_First_Filled[Fill_Type == "Light_Mud"])
# mast_leaves<- sum(combined_df_no_na$Number_of_Tubes_First_Filled[Fill_Type == "Masticated_Leaves"])


# light_mud
# mast_leaves
combined_df_no_na
unique(combined_df$Fill_Type)
light_mud <- combined_df_no_na %>% 
  filter(Fill_Type == "Light_Mud") %>%
  summarise(
    mySum = sum(Number_of_Tubes_First_Filled)
  ) %>% 
  pull(mySum)

mast_leaves<- combined_df_no_na %>% 
  filter(Fill_Type == "Masticated_Leaves") %>%
  summarise(
    mySum = sum(Number_of_Tubes_First_Filled)
  ) %>% 
  pull(mySum)


combined_df_no_na$Number_of_Tubes_First_Filled[Fill_Type == "Light_Mud"]

stats<- combined_df_no_na |>
  summarise(
    percent_grass = grass / total_filled,
    percent_leaves = leaves/total_filled,
    percent_lm = light_mud/total_filled,
    percent_ml = mast_leaves/total_filled
  )
stats


ggplot(fill_df, aes(x=Week, y=Number_of_Tubes_First_Filled,fill= Fill_Type)) +
  facet_wrap(~Fill_Type, scales = "free_x", labeller = labeller(Fill_Type = c(
    "Light_Mud" = "Light Mud",
    "Masticated_Leaves" = "Masticated Leaves",
    "Intact_Leaves" = "Intact Leaves")))+
  scale_fill_manual(values = my_colors, labels = my_labels) +
  labs(title = " Timing of First Fill by Year", x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
  geom_bar(position="stack", stat="identity") +
  #For scaling the tickmarks along the axes
  scale_x_continuous(breaks=seq(0,20,by=1)) +
  scale_y_continuous(breaks=seq(0,20,by=1))+
  theme_minimal() +
  mytheme +
  theme(
    axis.text = element_text(size = 8)
  )

fill_2023_cum <- fill_2023_cleaned |>
  mutate(cum_sum = cumsum(Number_of_Tubes_First_Filled))
fill_2024_cum <- fill_2024_cleaned |>
  mutate(cum_sum = cumsum(Number_of_Tubes_First_Filled))
fill_2025_clean_cum <- fill_2025_cleaned |>
  mutate(
    cum_sum = cumsum(Number_of_Tubes_First_Filled)
  )



# Line Plot By Year
## Cummulative sums of fill by year
fill_2023_cum_fill <- fill_2023_cleaned |>
  group_by(Fill_Type)|>
  mutate(cum_sum = cumsum(Number_of_Tubes_First_Filled))
fill_2024_cum_fill <- fill_2024_cleaned |>
  group_by(Fill_Type)|>
  mutate(cum_sum = cumsum(Number_of_Tubes_First_Filled))
fill_2025_clean_cum_fill <- fill_2025_cleaned |>
  group_by(Fill_Type)|>
  mutate(
    cum_sum = cumsum(Number_of_Tubes_First_Filled)
  )



#Join Cumulative Dataframes
full_cum <- bind_rows(fill_2023_cum, fill_2024_cum, fill_2025_clean_cum)
full_cum_fill <- bind_rows(fill_2023_cum_fill, fill_2024_cum_fill, fill_2025_clean_cum_fill)

#Make leaves and intact leaves the same thing, remove non-cavity nester fill
full_cum_clean <- full_cum |>
  mutate(
    Fill_Type = recode(Fill_Type,
                       "Leaves" = "Leaves",
                       "Intact_Leaves" = "Leaves"   # combine here
    )
  ) |>
  filter(!Fill_Type %in% c("Silk", "Debris"))

full_cum_clean_fill <- full_cum_fill |>
  mutate(
    Fill_Type = recode(Fill_Type,
                       "Leaves" = "Leaves",
                       "Intact_Leaves" = "Leaves"   # combine here
    )
  ) |>
  filter(!Fill_Type %in% c("Silk", "Debris"))
bee_fill_comp <- c(
  "Grass"             = "Grass\n(Grass Wasp)",
  "Leaves"            = "Leaves\n(Leafcutter Bee)",
  "Light_Mud"         = "Light Mud\n(Square-headed Wasp, Potter Wasp)",
  "Masticated_Leaves" = "Masticated Leaves\n(Leafcutter Bee, Mason Bee)",
  "Empty"             = "Empty"
)

ggplot(full_cum_clean,
       aes(x = Week,
           y = cum_sum,
           color = factor(Year),
           group = Year)) +
  geom_smooth()+
  labs(
    y = "Cumulative Fills",
    title = "Cumulative Sums of Fill Types by Year",
    color = "Year"
  ) +
  scale_color_manual(
    values = c("#FDE725FF", "#29AF7FFF", "#33638DFF"), 
    labels = c("2023", "2024", "2025")) +
  theme_minimal() +
  mytheme +
  theme(
    legend.text = element_text(size = 6)
  )

######Trial
ggplot(full_cum_clean_fill,
       aes(x = Week,
           y = cum_sum,
           color = Fill_Type)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~Year) +
  labs(
    y = "Cumulative Fills",
    title = "Cumulative Sums of Fill Types by Year",
    color = "Fill Type"
  ) +
  scale_color_manual(
    values = my_colors,
    labels = bee_fill_comp
  ) +
  scale_y_continuous(
    breaks = c(10, 20, 30, 40, 50, 60),
    limits = c(0, 60)
  ) +
  theme_minimal() +
  mytheme


# ?geom_area
ggplot(full_cum_clean_fill,
       aes(x = Week,
           y = cum_sum,
           color = Fill_Type)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~Year) +
  labs(
    y = "Cumulative Fills",
    title = "Cumulative Sums of Fill Types by Year",
    color = "Fill Type"
  ) +
  scale_color_manual(
    values = my_colors,
    labels = bee_fill_comp
  ) +
  scale_y_continuous(
    breaks = c(10, 20, 30, 40, 50, 60),
    limits = c(0, 60)
  ) +
  theme_minimal() +
  mytheme


fill_2023_cum %>% 
  ggplot(aes(x = cum_sum, fill = Fill_Type)) +
  geom_density(alpha = 0.5, position = "identity") +
  scale_fill_manual(values = my_colors, labels = my_labels) +
  theme_minimal() +
  mytheme



# install.packages("ggridges")
library(ggridges)

# Build the named labels vector from fill_inhabitant_labels
# bee_labels <- fill_inhabitant_labels %>%
#   filter(!is.na(Inhabitant)) %>%
#   group_by(Fill_Type) %>%
#   summarise(
#     bee_list = paste(Inhabitant, collapse = ", "),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     full_label = paste0(
#       gsub("_", " ", Fill_Type),   # "Light_Mud" -> "Light Mud"
#       "\n(", bee_list, ")"
#     )
#   ) %>%
#   # Named vector: names = Fill_Type levels, values = display labels
#   { setNames(.$full_label, .$Fill_Type) }


fill_2023_cum %>% 
  ggplot(aes(x = cum_sum, y = Fill_Type, fill = Fill_Type)) +
  geom_density_ridges(alpha = 0.6, scale = 1.2, show.legend = T) + 
  labs(
    x = "Cumulative Sum",
    y = "",
    title = "Cumulative Fill Type Comparisons",
    fill = "Bee Type(s) by \nFill Type"
  ) +
  scale_fill_manual(values = my_colors, labels = bee_fill_comp) +
  theme_minimal() +
  mytheme +
  theme(
    legend.text = element_text(size = 8, lineheight = 0.85),
    legend.key.height = unit(1.8, "cm"),   # give each key enough vertical room
    legend.title = element_text(size = 9, face = "bold")
  )

# fill_2023_cum
# fill_inhabitant_labels





`Proportion_of_Fill_By_Location - Sheet1` %>% 
  filter(Fill_Type != "Empty") %>% 
  ggplot(aes(x = Fill_Type, y = Location, fill = Proportion_of_Fill)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "white", 
    high = "darkgreen",
    breaks = c(0,.1,.2,.3)
  ) +
  labs(
    title = "Fill Type Distribution by Location",
    x = "Fill Type",
    y = "Location",
    fill = "Proportion"
  ) +
  theme_minimal() +
  mytheme +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

?scale_fill_gradient


`2023_bee tube first fill data_no WF` %>% 
  filter(`2023_bee tube first fill data_no WF`$Fill_Type != "NA") %>% 
  ggplot(aes(x = Week, y = Number_of_Tubes_First_Filled, color = Fill_Type)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = my_colors, labels = my_labels) +
  labs(
    y = "# of Tubes First Filled",
    title = "Tubes First Filled by Fill Type",
    subtitle = "2023 Data"
  ) +
  theme_minimal() +
  mytheme +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# unique(`2023_bee tube first fill data_no WF`$Fill_Type)



fill_2023 %>% 
  summary()
fill_2024 %>% 
  summary()
fill_2025 %>% 
  summary()



full_cum_clean <- full_cum |>
  mutate(
    Fill_Type = recode(Fill_Type,
                       "Leaves" = "Leaves",
                       "Intact_Leaves" = "Leaves"   # combine here
    )
  ) |>
  filter(!Fill_Type %in% c("Silk", "Debris"))
bee_fill_comp <- c(
  "Grass"             = "Grass\n(Grass Wasp)",
  "Leaves"            = "Leaves\n(Leafcutter Bee)",
  "Light_Mud"         = "Light Mud\n(Square-headed Wasp, Potter Wasp)",
  "Masticated_Leaves" = "Masticated Leaves\n(Leafcutter bee, Mason Bees)",
  "Empty"             = "Empty"
)

ggplot(full_cum_clean,
       aes(x = Week,
           y = cum_sum,
           fill = Fill_Type)) +
  geom_area() +
  facet_wrap(~Year) +
  labs(
    y = "Cumulative Fills",
    title = "Cumulative Sums of Fills by Year",
    fill = "Fill Type"
  ) +
  scale_fill_manual(values = my_colors, labels = bee_fill_comp) +
  theme_minimal() +
  mytheme +
  theme(
    legend.text = element_text(size = 6)
  )
##################################Location Differences#########
ID_freq_comp_24 = read.csv("Actual_presumed_complete.csv")
sample_sizes = data.frame(Location = c("Arboretum", "BeeBarn", "EGarden", "Meadow", "NoMowZone"),
                          Sample_Size = c(2, 95, 7, 14, 49))
ID_freq_comp_24$Inhabitant<- factor(ID_freq_comp_24$Inhabitant,
                                 levels = c("Chrysis cessata", "Coelioxys alternatus", "Isodontia mexicana", "Ancistrocerus", "Euodynerus foraminatus", "Trypoxylon lactitarse",
                                            "Megachile pugnata", "Megachile rotundata", "Megachile relativa", "Osmia caerulescens"))
ID_freq_comp_24 <- merge(ID_freq_comp_24, sample_sizes, by = "Location", all.x = TRUE)
graph = ggplot(ID_freq_comp_24, aes(fill=Inhabitant, y=Proportion_of_Fill, x=Location)) +
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
  labs(title = "Proportion of 2024 Inhabitants by Location", x = "Location", y = "Proportion", fill = "Inhabitant") +
  geom_bar(position="fill", stat="identity", col = "grey") +
  scale_y_continuous(breaks=seq(0,1,by=0.1))+
  theme_minimal()+
  mytheme

for (i in 1:nrow(sample_sizes)) {
  graph <- graph + annotate("text", x = sample_sizes$Location[i], y = -0.04, label = sample_sizes$Sample_Size[i], vjust = 1.5, size = 3.6)
}
print(graph)



ID_freq_2025_clean = read.csv("Bee_Occupants_for_ESA_poster_final.csv")
sample_sizes_2 = data.frame(Location = c("Arboretum", "ISC/Greenhouse", "No Mow Zone", "E-Garden", "Island Preserve"),
                            Sample_Size = c(140, 40, 20, 20, 160))
ID_freq_2025_clean$Inhabitant <- factor(ID_freq_2025_clean$Inhabitant,
                                        levels = c("Parasite", "Grass Wasp", "Potter Wasp", "Square-headed Wasp", "Unknown Wasp",
                                                   "Leafcutter- Mp", "Leafcutter- Leaf", "Mason Bee", "Small Cell Bee"))
graph_2<- ggplot(ID_freq_2025_clean, aes(fill=Inhabitant, y=Proportion_of_Fill, x=Location)) +
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
  labs(title = "Proportion of 2025 Inhabitants by Location", x = "Location", y = "Proportion", fill = "Inhabitant") +
  geom_bar(position="fill", stat="identity", col = "grey") +
  scale_y_continuous(breaks=seq(0,1,by=0.1))+
  theme_minimal()+
  mytheme

for (i in 1:nrow(sample_sizes_2)) {
  graph_2 <- graph_2 + annotate("text", x = sample_sizes_2$Location[i], y = -0.04, label = sample_sizes_2$Sample_Size[i], vjust = 1.5, size = 3.6)
}
print(graph_2)

