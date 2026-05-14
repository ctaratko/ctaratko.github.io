# =========================================================
# FinalStang.R (Reorganized)
# Goal: Read all CSVs at the top using read.csv("Data Analysis/{file name}")
#       so objects exist before they are used.
# Note: This script preserves your logic and plots, but reorders reads and
#       avoids accidental overwriting of key objects.
# =========================================================

# =========================================================
# Libraries & Packages
# =========================================================
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(hrbrthemes)
library(RColorBrewer)
library(ggtext)
library(ggridges)
library(janitor)

# =========================================================
# READ ALL CSV DATA (TOP OF SCRIPT)
# All reads MUST use: read.csv("Data Analysis/{file name}")
# =========================================================

# --- Inhabitant proportions (Actual & Presumed) ---
ID_freq <- read.csv("Data Analysis/Actual andPresumed ID.csv")

# --- Species-level ID proportions (complete) ---
ID_freq_comp <- read.csv("Data Analysis/Actual_presumed_complete.csv")

# --- Fill-type by location (2025 sheet1) ---
fill_by_location_2025 <- read.csv("Data Analysis/Proportion_of_Fill_By_Location - Sheet1.csv")

# --- Fill-type by location (2025–2026 cleaned) ---
fill_by_location_2025_clean <- read.csv("Data Analysis/Proportion_of_Fill_By_Location - 2025-2026.csv")

# --- Occupants by location (ESA poster final) ---
ID_freq_2025_clean <- read.csv("Data Analysis/Bee_Occupants_for_ESA_poster_final.csv")

# --- First fill timing CSVs ---
fill_2023_file <- read.csv("Data Analysis/2023_bee tube first fill data_no WF.csv")
fill_2024_file <- read.csv("Data Analysis/Week_of_First_FIll_for_CSV - Sheet1.csv")
fill_2025_file <- read.csv("Data Analysis/Week_of_First_Fill_2025 - Sheet1.csv")
fill_2025_clean_file <- read.csv("Data Analysis/Week_of_First_FIll_2025 - 2026_cleaned.csv")

fill_2023 <- read.csv("Data Analysis/2023_bee tube first fill data_no WF.csv")
fill_2024 <- read.csv("Data Analysis/Week_of_First_FIll_for_CSV - Sheet1.csv")
fill_2025 <- read.csv("Data Analysis/Week_of_First_Fill_2025 - Sheet1.csv")
fill_2025_clean <- read.csv("Data Analysis/Week_of_First_FIll_2025 - 2026_cleaned.csv")


# Optional: remove NA rows from the 2023 file version (as you did)
fill_2023_file <- na.omit(fill_2023_file)

# =========================================================
# OPTIONAL BULK IMPORT (kept for reference)
# NOTE: Your old version used absolute path + read_csv().
# This block is commented out because you required read.csv("Data Analysis/{file}").
# =========================================================
# files <- list.files(path = "Data Analysis", pattern = "*.csv", full.names = TRUE)
# for (k in files) {
#   obj_name <- tools::file_path_sans_ext(basename(k))
#   print(obj_name)
#   assign(obj_name, read.csv(k))
#   print(colnames(get(obj_name)))
# }
# rm(files, obj_name)

# =========================================================
# Type By Location (Daniel’s hand-entered demo dataset)
# =========================================================
Location <- c(rep("Arboretum", 12),
              rep("Bee Barn", 12),
              rep("Meadow", 12),
              rep("No Mow Zone", 12),
              rep("eGarden", 12))

Inhabitant <- rep(c("Grass-Carrying Wasp", "Potter Wasp", "Peg-Shaped Sphecid",
                    "Leafcutter1", "Leafcutter 3", "Ants", "Spiders",
                    "Leafcutter 2", "Other Parasites", "Fly Pupa", "Empty", "Other"),
                  5)

Proportion_of_Fill <- c(
  0,0.025,0,0,0,0.075,0.1,0,0,0.075,0.725,0,
  0.09166667,0.11666667,0.08333333,0.43333333,0,0.025,
  0.01666667,0,0.01666667,0.00833333,0.20833333,0,
  0.225,0,0,0.025,0,0.3,0.05,0,0,0.05,0.35,0,
  0.15,0.025,0,0.2,0.125,0,0.1,0.05,0,0,0.35,0,
  0,0.1,0,0,0,0.05,0.1,0,0,0,0.55,0.2
)

data2 <- data.frame(Location, Inhabitant, Proportion_of_Fill)

# Fixed: your KeyOrder string was broken across lines; preserved but corrected
KeyOrder <- c("Empty", "Other", "Ants", "Other Parasites", "Fly Pupa", "Spiders",
              "Grass Wasp", "Naked Larva", "Peg-Shaped Pupa", "Leafcutter",
              "Green-fill Leafcutter", "Leaf Tube Pupa")

# =========================================================
# Consistent Colors, Labels, Theme (defined BEFORE used)
# =========================================================
my_colors <- c(
  "Grass"             = "darkgreen",
  "Leaves"            = "#95D840FF",
  "Intact_Leaves"     = "#29AF7FFF",
  "Masticated_Leaves" = "#FDE725FF",
  "Light_Mud"         = "#33638DFF",
  "Debris"            = "grey",
  "Silk"              = "white",
  "Empty"             = "black"
)

my_labels <- c(
  "Grass"             = "Grass",
  "Leaves"            = "Leaves",
  "Intact_Leaves"     = "Intact Leaves",
  "Masticated_Leaves" = "Masticated Leaves",
  "Light_Mud"         = "Light Mud",
  "Debris"            = "Debris",
  "Silk"              = "Spider Silk",
  "Empty"             = "Empty"
)

mytheme <- theme(
  plot.title = element_text(face = "bold", family = "Times New Roman", size = 20),
  axis.title = element_text(face = "italic", family = "Times New Roman"),
  axis.text = element_text(family = "Times New Roman"),
  plot.background = element_rect(fill = "white"),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = NA, color = "black"),
  panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
  panel.grid.major.x = element_blank(),
  axis.line.y = element_line(color = "black", linewidth = 0.5),
  legend.title = element_text(family = "Times New Roman", face = "bold"),
  strip.background = element_rect(fill = "grey95", color = "black"),
  strip.text = element_text(color = "black", face = "bold")
)

# =========================================================
# PLOTS (same as your original ordering, but now safe)
# =========================================================

# Daniel demo plot
print(
  ggplot(data2, aes(fill = Inhabitant, y = Proportion_of_Fill, x = Location)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(values = c("grey","white","black","orange","lightgreen","darkgreen",
                                 "limegreen","lightgrey","darkgrey","yellow","lightyellow","brown")) +
    labs(title = "Figure 2: Proportion of Inhabitant Types by Location",
         x = "Location", y = "Proportion", fill = "Inhabitant") +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    mytheme
)

# Actual and presumed ID plot
print(
  ggplot(ID_freq, aes(fill = Inhabitant, y = Proportion_of_Fill, x = Location)) +
    geom_bar(position = "fill", stat = "identity", col = "black") +
    scale_fill_manual(values = c("navy","lightyellow","orange","lightgrey","brown","darkgreen",
                                 "pink","lightblue","black","lightgreen","yellow","red",
                                 "purple","blue","darkgreen","limegreen","darkgrey","hotpink")) +
    labs(title = "Figure 2: Proportion of Inhabitant Types by Location",
         x = "Location", y = "Proportion", fill = "Inhabitant") +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    mytheme
)

# =========================================================
# Species-level composition plot (Actual_presumed_complete.csv)
# =========================================================
sample_sizes_species <- data.frame(
  Location = c("Arboretum", "BeeBarn", "EGarden", "Meadow", "NoMowZone"),
  Sample_Size = c(2, 95, 7, 14, 49)
)

ID_freq_comp$Inhabitant <- factor(ID_freq_comp$Inhabitant,
  levels = c("Chrysis cessata", "Coelioxys alternatus", "Isodontia mexicana",
             "Ancistrocerus", "Euodynerus foraminatus", "Trypoxylon lactitarse",
             "Megachile pugnata", "Megachile rotundata", "Megachile relativa", "Osmia caerulescens")
)

ID_freq_comp <- merge(ID_freq_comp, sample_sizes_species, by = "Location", all.x = TRUE)

species_colors <- c(
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

graph_species <- ggplot(ID_freq_comp, aes(fill = Inhabitant, y = Proportion_of_Fill, x = Location)) +
  geom_bar(position = "fill", stat = "identity", col = "white") +
  scale_fill_manual(values = species_colors, breaks = names(species_colors)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  labs(title = "Proportion of Inhabitant Types by Location (Species-level)",
       x = "Location", y = "Proportion", fill = "Inhabitant") +
  theme_minimal() + mytheme +
  coord_cartesian(ylim = c(-0.08, 1), clip = "off") +
  theme(plot.margin = margin(t=5, r=5, b=45, l=5))

for (i in 1:nrow(sample_sizes_species)) {
  graph_species <- graph_species +
    annotate("text",
             x = sample_sizes_species$Location[i],
             y = -0.04,
             label = sample_sizes_species$Sample_Size[i],
             vjust = 1.5,
             size = 3.6)
}
print(graph_species)

# =========================================================
# Fill Type by Location (2025 Sheet1)
# =========================================================
sample_sizes_filltype <- data.frame(
  Location = c("Arboretum", "ISC/Greenhouse", "No Mow Zone", "EGarden", "Island Preserve"),
  Sample_Size = c(140, 40, 20, 20, 160)
)

fill_by_location_2025 <- merge(fill_by_location_2025, sample_sizes_filltype, by = "Location", all.x = TRUE)

graph_fill_loc <- ggplot(fill_by_location_2025, aes(fill = Fill_Type, y = Proportion_of_Fill, x = Location)) +
  geom_bar(position = "fill", stat = "identity", col = "black") +
  scale_fill_manual(values = c("lightyellow","black","darkgreen","lightgreen","grey","orange","yellow")) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  labs(title = "Figure 2: Proportion of Fill Types by Location",
       x = "Location", y = "Proportion", fill = "Fill Type") +
  mytheme +
  coord_cartesian(ylim = c(-0.08, 1), clip = "off") +
  theme(plot.margin = margin(t=5, r=5, b=45, l=5))

for (i in 1:nrow(sample_sizes_filltype)) {
  graph_fill_loc <- graph_fill_loc +
    annotate("text",
             x = sample_sizes_filltype$Location[i],
             y = -0.04,
             label = sample_sizes_filltype$Sample_Size[i],
             vjust = 1.5,
             size = 3.6)
}
print(graph_fill_loc)

# =========================================================
# Clean 2025–2026 Fill Proportions by Location
# =========================================================
fill_by_location_2025_clean$Fill_Type <- factor(fill_by_location_2025_clean$Fill_Type,
  levels = c("Empty", "Debris", "Silk", "Grass", "Light_Mud", "Intact_Leaves", "Masticated_Leaves")
)

fill_by_location_2025_clean <- merge(fill_by_location_2025_clean, sample_sizes_filltype, by = "Location", all.x = TRUE)

graph_fill_loc_clean <- ggplot(fill_by_location_2025_clean, aes(fill = Fill_Type, y = Proportion_of_Fill, x = Location)) +
  geom_bar(position = "fill", stat = "identity", col = "black") +
  scale_fill_manual(values = my_colors, labels = my_labels) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  labs(title = "Proportion of Fill Types by Location (2025–2026 Clean)",
       x = "Location", y = "Proportion", fill = "Fill Type") +
  theme_minimal() + mytheme +
  coord_cartesian(ylim = c(-0.08, 1), clip = "off") +
  theme(plot.margin = margin(t=5, r=5, b=45, l=5))

for (i in 1:nrow(sample_sizes_filltype)) {
  graph_fill_loc_clean <- graph_fill_loc_clean +
    annotate("text",
             x = sample_sizes_filltype$Location[i],
             y = -0.04,
             label = sample_sizes_filltype$Sample_Size[i],
             vjust = 1.5,
             size = 3.6)
}
print(graph_fill_loc_clean)

# =========================================================
# ESA Poster Inhabitant Type by Location
# =========================================================
ID_freq_2025_clean$Inhabitant <- factor(ID_freq_2025_clean$Inhabitant,
  levels = c("Parasite", "Grass Wasp", "Potter Wasp", "Square-headed Wasp",
             "Unknown Wasp", "Leafcutter- Mp", "Leafcutter- Leaf",
             "Mason Bee", "Small Cell Bee")
)

print(
  ggplot(ID_freq_2025_clean, aes(fill = Inhabitant, y = Proportion_of_Fill, x = Location)) +
    geom_bar(position = "fill", stat = "identity", col = "black") +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    labs(title = "Proportion of Inhabitant Types by Location (2025)",
         x = "Location", y = "Proportion", fill = "Inhabitant") +
    theme_minimal() + mytheme
)

# =========================================================
# First Fill Timing (Daniel manual dataset preserved)
# =========================================================
Week <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5),rep(6,5),rep(7,5),rep(8,5),
          rep(9,5),rep(10,5),rep(11,5),rep(12,5),rep(13,5),rep(14,5),rep(15,5),rep(16,5),
          rep(17,5),rep(18,5),rep(19,5),rep(20,5))

Fill_Type <- rep(c("WhiteDebris","Masticated_Leaves","Light_Mud","Leaves","Grass"), 20)

Number_of_Tubes_First_Filled <- c(
  0,0,0,0,0,  0,0,0,0,0,  1,0,4,0,0,  0,2,1,0,0,  1,2,1,0,0,
  7,4,5,0,0,  2,8,4,0,0,  0,1,3,0,3,  0,6,5,0,2,  0,0,6,0,3,
  1,0,6,1,2,  1,0,1,7,0,  0,0,0,14,2,  1,0,0,11,7,  0,0,0,9,1,
  0,0,0,8,2,  0,0,0,4,4,  0,0,0,3,2,  0,0,0,0,1,  0,0,0,0,0
)

data <- data.frame(Week, Fill_Type, Number_of_Tubes_First_Filled)

fill_2023 <- data %>%
  mutate(
    Fill_Type = ifelse(Fill_Type == "WhiteDebris", "Debris", Fill_Type)
  )

# Label system for annotated stacked bars
fill_inhabitant_labels <- tibble(
  Fill_Type = c("Grass","Grass","Grass",
                "Masticated_Leaves","Masticated_Leaves",
                "Intact_Leaves","Intact_Leaves",
                "Light_Mud","Silk","Debris","Empty"),
  Inhabitant = c("Grass Wasp","Square-headed Wasp","Potter Wasp",
                 "Leafcutter- Mp","Leafcutter- Leaf",
                 "Leafcutter- Mp","Leafcutter- Leaf",
                 "Mason Bee","Mason Bee","Parasite", NA_character_)
)

fill_labels_collapsed <- fill_inhabitant_labels %>%
  filter(!is.na(Inhabitant)) %>%
  group_by(Fill_Type) %>%
  summarise(Inhabitant = paste(Inhabitant, collapse = "\n"), .groups = "drop")

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

# Annotated stacked bar
print(
  ggplot(fill_2023, aes(fill = Fill_Type, y = Number_of_Tubes_First_Filled, x = Week)) +
    geom_bar(position = "stack", stat = "identity", col = "black") +
    geom_text(
      data = label_positions,
      aes(x = Week, y = ymid, label = Inhabitant),
      inherit.aes = FALSE,
      size = 2.8,
      fontface = "italic",
      color = "black",
      check_overlap = TRUE
    ) +
    scale_fill_manual(values = my_colors, labels = my_labels) +
    scale_x_continuous(breaks = seq(0, 20, by = 1)) +
    scale_y_continuous(breaks = seq(0, 27, by = 1)) +
    labs(
      title = "Figure 1a. Date of First Fill in 2023 by Type",
      x = "Week", y = "Number of Tubes First Filled",
      fill = "Fill Type"
    ) +
    mytheme
)

# =========================================================
# NOTE
# Your original script continues with additional faceting/cumulative sections.
# Those can be appended below safely because all CSVs are now loaded above.
# =========================================================

fill_2023_cleaned <- fill_2023 |> 
  mutate(Year = 2023,
         Number_of_Tubes_First_Filled = ifelse(is.na(Number_of_Tubes_First_Filled), 0, Number_of_Tubes_First_Filled))
fill_2024_cleaned <- fill_2024 |> 
  mutate(Number_of_Tubes_First_Filled = Number_of_Tubes_First.Filled,
         Number_of_Tubes_First_Filled = ifelse(is.na(Number_of_Tubes_First_Filled), 0, Number_of_Tubes_First_Filled),
         Year = 2024)|>
  select(Week, Year, Number_of_Tubes_First_Filled, Fill_Type)

fill_2025_cleaned <- fill_2025_clean|> 
  mutate(Year = 2025,
         Number_of_Tubes_First_Filled = Number_of_Tubes_First.Filled,
         Number_of_Tubes_First_Filled = ifelse(is.na(Number_of_Tubes_First_Filled), 0, Number_of_Tubes_First_Filled))|>
  select(Week, Year, Number_of_Tubes_First_Filled, Fill_Type)

combined_df <- bind_rows(fill_2023_cleaned, fill_2024_cleaned, fill_2025_cleaned)

fill_2023_cum <- fill_2023_cleaned |>
  mutate(cum_sum = cumsum(Number_of_Tubes_First_Filled))
fill_2024_cum <- fill_2024_cleaned |>
  mutate(cum_sum = cumsum(Number_of_Tubes_First_Filled))
fill_2025_clean_cum <- fill_2025_cleaned |>
  mutate(
    cum_sum = cumsum(Number_of_Tubes_First_Filled)
  )

ID_freq_comp = read.csv("Data Analysis/Proportion_of_Fill_By_Location - 2025-2026.csv")
sample_sizes = data.frame(Location = c("Arboretum", "ISC/Greenhouse", "No Mow Zone", "EGarden", "Island Preserve"),
                          Sample_Size = c(140, 40, 20, 20, 160))


ID_freq_comp$Fill_Type <- factor(ID_freq_comp$Fill_Type,
                                 levels = c("Empty", "Debris", "Silk", "Grass", "Light_Mud",
                                            "Intact_Leaves", "Masticated_Leaves"))


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
