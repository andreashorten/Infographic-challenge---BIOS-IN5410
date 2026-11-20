# Load the necessary library
library(tidyverse)


# Enabling command-line arguments
arguments <- commandArgs(trailingOnly = TRUE)


# Provides an error message if arguments are not given
if (length(arguments) == 0) {
  stop(
    "Please provide exactly three colors.\n",
    "Example: Rscript InfographicPlot_Penguins.R ",
    "\"steelblue,gold,forestgreen\""
  )
}


# Defines the argument type and input
species_colours <- str_split(arguments[1], ",", simplify = TRUE) |>
  as.character() |>
  str_trim()


# Provides an error message if arguments are not given correctly
if (length(species_colours) != 3) {
  stop(
    "Please provide exactly three comma-separated colours: ",
    "one each for Adelie, Chinstrap, and Gentoo."
  )
}


# Give each color a species name
species_colours <- setNames(species_colours, c("Adelie", "Chinstrap", "Gentoo"))


# Load the "penguins" dataset
penguins_data <- datasets::penguins


# Summarize every species on every island
island_summary <- penguins |>
  drop_na(island, species, body_mass, flipper_len) |>
  group_by(island, species) |>
  summarise(
    average_mass = mean(body_mass),
    average_flipper = mean(flipper_len),
    number_sampled = n(),
    .groups = "drop") |>
  mutate(
    island = factor(
      island,
      levels = c("Biscoe", "Dream", "Torgersen")))


# Create the bubble chart 
penguin_plot <- ggplot(
  island_summary, 
  aes(x = island, y = average_mass, size = average_flipper, colour = species)) + 
  geom_point(alpha = 0.85) +
  scale_y_continuous(limits = c(3600, 5200)) +
  scale_colour_manual(values = species_colours) +
  scale_size(range = c(10, 25)) +
  guides(
    colour = guide_legend(
      title = "Species",
      order = 1,
      position = "top"),
    size = guide_legend(
      title = "Average flipper length (mm)",
      title.hjust = 0.5,
      order = 2,
      position = "bottom",
      override.aes = list(size = c(3, 5, 7)))) +
  labs(
    title = "Meet the Palmer Archipelago Penguins: Where do they live and who is biggest?",
    subtitle = "Each bubble represents a penguin species found on an island.",
    x = "Home island",
    y = "Average body mass (g)",
    colour = "Species",
    caption = "Bubble size represents average flipper length\nSource: penguins dataset in R") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(colour = "grey30"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0.5))
    #legend.position = "top")


# Save the plot as a png-file
ggsave(
  filename = "penguin_bubbles.png",
  plot = penguin_plot,
  width = 10,
  height = 7,
  dpi = 300)

# Print a message when the script is done running
message("Saved chart as: penguin_bubbles.png")
