#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("shiny")
#install.packages("tidyr")
#install.packages("ggpub")
#install.packages("plyr")
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(tidyr)
library(ggpubr)
library(plyr)





#Read Data
data1 <- read.csv("data/Results_21Mar2022.csv")
data2 <- read.csv("data/Results_21MAR2022_nokcaladjust.csv")
merged_data <- rbind(data1, data2)
write.csv(merged_data, "Result.csv", row.names = FALSE)
result <- read.csv("Result.csv")
#str(result)

################################################################################
age_diet_data = result[, c("mc_run_id", "n_participants", "age_group", "sex","diet_group")]
write.csv(age_diet_data, "Age_Diet_Data.csv", row.names = FALSE)
ageDiet_data <- read.csv("Age_Diet_Data.csv", header = T, check.names = F)
colnames(ageDiet_data) = c("ID","Participants","Age_Group", "Gender", "Diet_Group")
################################################################################

##p1(q1): Relationship between Age, Gender, Participants and Diest
labels = ageDiet_data[order(ageDiet_data$Age_Group), "Participants"]
ageDiet_data$Participants = as.numeric(ageDiet_data$Participants, levels = labels)

p = ggplot(ageDiet_data, aes(Age_Group, Participants)) +
    geom_point(aes(shape=Gender, color=Diet_Group), size=3) 

p1 <- p + 
      scale_shape_manual(values = c(15,16,17,18,19,20)) + 
      scale_colour_manual(values = c("purple", "orange", "yellow", "green", "blue", "red")) + 
      labs(color="Diet Group", shape="Gender", x="Age Group", y="Participants", title = "Relationship between Age, Gender, Participants, and Diet") +
      theme(axis.text.x = element_text(color="black", size=5), axis.text.y = element_text("black", size=5)) +
      scale_y_continuous(breaks = seq(0, max(ageDiet_data$Participants),by=200)) +
      theme_bw()
p1
ggsave("bubble.pdf", width = 10, height = 12)


################################################################################

# Read Data
selected_cols_data = result[, c("mc_run_id", "mean_ghgs", "mean_land", 
                              "mean_eut", "mean_ghgs_ch4", "mean_ghgs_n2o",
                              "mean_bio", "n_participants", "diet_group")]
write.csv(selected_cols_data, "Environment_Data.csv", row.names = FALSE)

env_data <- read.csv("Environment_Data.csv", header = T, check.names = F)
colnames(env_data) = c("id", "mean_ghg", "mean_land", 
                       "mean_eut", "mean_ghg_ch4", "mean_ghg_n2o", 
                       "mean_bio", "n_participants", "diet_group")
################################################################################

# p2(q2): Which diet is most beneficial for the environment?
# Factors: mean_ghgs, mean_bio, mean_land, mean_acid, mean_eut

#str(env_data)


rename_data <- env_data %>%
  mutate(diet_group = case_when(
    diet_group == "meat" ~ "Less Meat",
    diet_group == "meat50" ~ "Medium Meat",
    diet_group == "meat100" ~ "More Meat",
    diet_group == "vegan" ~ "Vegans",
    diet_group == "veggie" ~ "Vegetarians",
    diet_group == "fish" ~ "Fish",
    TRUE ~ diet_group
  ))

#str(rename_data)

# Rename the name of each column
X_axis_names <- c(
  "mean_ghg" = "GHG Emissions",
  "mean_land" = "Land Used",
  "mean_eut" = "Eutrophication",
  "mean_ghg_ch4" = "GHG_CH4 Emissions",
  "mean_ghg_n2o" = "GHG_N2O Emissions",
  "mean_bio" = "Biodiversity"
)


# Store the weighted average of environmental factors for different dietary categories separately
fish_data <- subset(rename_data, diet_group == "Fish")
medium_meat_data <- subset(rename_data, diet_group == "Medium Meat")
more_meat_data <- subset(rename_data, diet_group == "More Meat")
less_meat_data <- subset(rename_data, diet_group == "Less Meat")
vegans_data <- subset(rename_data, diet_group == "Vegans")
veges_data <- subset(rename_data, diet_group == "Vegetarians")


################################################################################
fish_summ_aver <- fish_data %>%
  group_by(diet_group) %>%
    summarise(mean_ghg = weighted.mean(mean_ghg, n_participants),
              mean_land = weighted.mean(mean_land, n_participants),
              mean_eut = weighted.mean(mean_eut, n_participants),
              mean_ghg_ch4 = weighted.mean(mean_ghg_ch4, n_participants),
              mean_ghg_n2o = weighted.mean(mean_ghg_n2o, n_participants),
              mean_bio = weighted.mean(mean_bio, n_participants
    ))


medium_summ_aver <- medium_meat_data %>%
  group_by(diet_group) %>%
  summarise(mean_ghg = weighted.mean(mean_ghg, n_participants),
            mean_land = weighted.mean(mean_land, n_participants),
            mean_eut = weighted.mean(mean_eut, n_participants),
            mean_ghg_ch4 = weighted.mean(mean_ghg_ch4, n_participants),
            mean_ghg_n2o = weighted.mean(mean_ghg_n2o, n_participants),
            mean_bio = weighted.mean(mean_bio, n_participants
            ))

more_summ_aver <- more_meat_data %>%
  group_by(diet_group) %>%
  summarise(mean_ghg = weighted.mean(mean_ghg, n_participants),
            mean_land = weighted.mean(mean_land, n_participants),
            mean_eut = weighted.mean(mean_eut, n_participants),
            mean_ghg_ch4 = weighted.mean(mean_ghg_ch4, n_participants),
            mean_ghg_n2o = weighted.mean(mean_ghg_n2o, n_participants),
            mean_bio = weighted.mean(mean_bio, n_participants
            ))


less_summ_aver <- less_meat_data %>%
  group_by(diet_group) %>%
  summarise(mean_ghg = weighted.mean(mean_ghg, n_participants),
            mean_land = weighted.mean(mean_land, n_participants),
            mean_eut = weighted.mean(mean_eut, n_participants),
            mean_ghg_ch4 = weighted.mean(mean_ghg_ch4, n_participants),
            mean_ghg_n2o = weighted.mean(mean_ghg_n2o, n_participants),
            mean_bio = weighted.mean(mean_bio, n_participants
            ))


vegans_summ_aver <- vegans_data %>%
  group_by(diet_group) %>%
  summarise(mean_ghg = weighted.mean(mean_ghg, n_participants),
            mean_land = weighted.mean(mean_land, n_participants),
            mean_eut = weighted.mean(mean_eut, n_participants),
            mean_ghg_ch4 = weighted.mean(mean_ghg_ch4, n_participants),
            mean_ghg_n2o = weighted.mean(mean_ghg_n2o, n_participants),
            mean_bio = weighted.mean(mean_bio, n_participants
            ))


veges_summ_aver <- veges_data %>%
  group_by(diet_group) %>%
  summarise(mean_ghg = weighted.mean(mean_ghg, n_participants),
            mean_land = weighted.mean(mean_land, n_participants),
            mean_eut = weighted.mean(mean_eut, n_participants),
            mean_ghg_ch4 = weighted.mean(mean_ghg_ch4, n_participants),
            mean_ghg_n2o = weighted.mean(mean_ghg_n2o, n_participants),
            mean_bio = weighted.mean(mean_bio, n_participants
            ))
################################################################################

# Adjust the shape of data
fish_weighted_aver <- tidyr::pivot_longer(
                        fish_summ_aver,
                        cols = c(mean_ghg, mean_land, mean_eut, mean_ghg_ch4, mean_ghg_n2o, mean_bio),
                        names_to = "variable",
                        values_to = "value")

medium_weighted_aver <- tidyr::pivot_longer(
                        medium_summ_aver,
                        cols = c(mean_ghg, mean_land, mean_eut, mean_ghg_ch4, mean_ghg_n2o, mean_bio),
                        names_to = "variable",
                        values_to = "value")

more_weighted_aver <- tidyr::pivot_longer(
                        more_summ_aver,
                        cols = c(mean_ghg, mean_land, mean_eut, mean_ghg_ch4, mean_ghg_n2o, mean_bio),
                        names_to = "variable",
                        values_to = "value")


less_weighted_aver <- tidyr::pivot_longer(
                        less_summ_aver,
                        cols = c(mean_ghg, mean_land, mean_eut, mean_ghg_ch4, mean_ghg_n2o, mean_bio),
                        names_to = "variable",
                        values_to = "value")


vegans_weighted_aver <- tidyr::pivot_longer(
                        vegans_summ_aver,
                        cols = c(mean_ghg, mean_land, mean_eut, mean_ghg_ch4, mean_ghg_n2o, mean_bio),
                        names_to = "variable",
                        values_to = "value")


veges_weighted_aver <- tidyr::pivot_longer(
                        veges_summ_aver,
                        cols = c(mean_ghg, mean_land, mean_eut, mean_ghg_ch4, mean_ghg_n2o, mean_bio),
                        names_to = "variable",
                        values_to = "value")

################################################################################
#fish_weighted_aver$variable <- factor(fish_weighted_aver$variable, levels = names(X_axis_names), labels = X_axis_names)
#medium_weighted_aver$variable <- factor(medium_weighted_aver$variable, levels = names(X_axis_names), labels = X_axis_names)
#more_weighted_aver$variable <- factor(more_weighted_aver$variable, levels = names(X_axis_names), labels = X_axis_names)
#less_weighted_aver$variable <- factor(less_weighted_aver$variable, levels = names(X_axis_names), labels = X_axis_names)
#vegans_weighted_aver$variable <- factor(vegans_weighted_aver$variable, levels = names(X_axis_names), labels = X_axis_names)
#veges_weighted_aver$variable <- factor(veges_weighted_aver$variable, levels = names(X_axis_names), labels = X_axis_names)
################################################################################

#Normalized data scale within the range of 0 to 1
std_fish_weighted_aver <- within(fish_weighted_aver, {value <- scales::rescale(value, to = c(0,1))})
std_fish_weighted_aver$diet_group <- rep("Fish", nrow(std_fish_weighted_aver))

std_medium_weighted_aver <- within(medium_weighted_aver, {value <- scales::rescale(value, to = c(0,1))})
std_medium_weighted_aver$diet_group <- rep("Medium Meat", nrow(std_medium_weighted_aver))

std_more_weighted_aver <- within(more_weighted_aver, {value <- scales::rescale(value, to = c(0,1))})
std_more_weighted_aver$diet_group <- rep("More Meat", nrow(std_more_weighted_aver))

std_less_weighted_aver <- within(less_weighted_aver, {value <- scales::rescale(value, to = c(0,1))})
std_less_weighted_aver$diet_group <- rep("Less Meat", nrow(std_less_weighted_aver))

std_vegans_weighted_aver <- within(vegans_weighted_aver, {value <- scales::rescale(value, to = c(0,1))})
std_vegans_weighted_aver$diet_group <- rep("Vegans", nrow(std_vegans_weighted_aver))

std_veges_weighted_aver <- within(veges_weighted_aver, {value <- scales::rescale(value, to = c(0,1))})
std_veges_weighted_aver$diet_group <- rep("Vegetarians", nrow(std_veges_weighted_aver))
################################################################################

#View(std_fish_weighted_aver)
#View(std_medium_weighted_aver)
#View(std_more_weighted_aver)
#View(std_less_weighted_aver)
#View(std_vegans_weighted_aver)
#View(std_veges_weighted_aver)


################################################################################
df <- data.frame(
  variable = rep(c("GHG Emissions", "Biodiversity",
                   "Land Used", "Eutrophication",
                   "GHG_CH4 Emissions", "GHG_N2O Emissions"),6),
  
  value = c(0.01849011, 1, 0.02501415, 0.08245127, 0.00205788, 0,
            0.019753693, 1, 0.030107666, 0.086730932, 0.002533848, 0,
            0.025016461, 1, 0.055063811, 0.096387200, 0.003797901, 0,
            0.02191640, 1, 0.03930764, 0.09226697, 0.00304929, 0,
            0.0198746487, 1, 0.0348176341, 0.0915670158, 0, 0.0005267028,
            0.016582531, 1, 0.024414207, 0.069416141, 0.001910376, 0),
  
  diet_group = rep(c("Fish", "Medium Meat", "More Meat", 
                     "Less Meat", "Vegans", "Vegetarians"), each = 6)
)
################################################################################

# Compare which type of diet has a greater impact on environmental factors
# Draw the parallel plot
p2 <- ggplot(df, aes(x = variable, y = value, group = diet_group, color = diet_group)) +
  geom_line(size = 0.6) +
  geom_errorbar(aes(ymin = value - 0.05, ymax = value + 0.05), linewidth = 0.1) +
  geom_point(size = 1.5) +
  theme_classic() +
  theme(legend.position = "right", legend.title = element_blank()) +
  labs(title = "Comparison of Environmental Factors by Diet", x = "Environmental Factor", y = "Mean Value") +
  scale_color_manual(values = c("#00FFFF", "#FF00FF", "#4B00B2",
                                "#A0522D", "#FFFF00", "#FE420F")) +
  theme(panel.grid.major = element_line(colour = "grey"))

#p2
################################################################################

#Convert the drawn data graph into an interactive data graph
inter_plot <- ggplotly(p2)
inter_plot

int_plot <- ggplotly(p1)
int_plot

