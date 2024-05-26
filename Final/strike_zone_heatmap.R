library(tree)
library(randomForest)

setwd("~/bigdata/")

source("LASSO/create_matrix.R")

left_plate_edge <- -(17/2)/12
right_plate_edge <- (17/2)/12

heatmap_data <- df_to_matrix %>%
  mutate(center_z = (sz_bot + sz_top) / 2,
         dist_z = plate_z - center_z) %>% 
  mutate(x_bin = cut(plate_x, breaks = seq(-1.5, 1.5, by = 0.1)),
         z_bin = cut(dist_z, breaks = seq(-2, 2, by = 0.1))) %>%
  group_by(x_bin, z_bin) %>%
  summarise(
    strike_rate = mean(is_called_strike),
    observations = n()
  ) %>%
  na.omit() %>% 
  separate(x_bin, c("xmin", "xmax"), sep = ",") %>% 
  separate(z_bin, c("zmin", "zmax"), sep = ",") %>% 
  mutate(xmin = str_remove(xmin, "\\(") %>% as.numeric(),
         zmin = str_remove(zmin, "\\(") %>% as.numeric(),
         xmax = str_remove(xmax, "]") %>% as.numeric(),
         zmax = str_remove(zmax, "]") %>% as.numeric(),
         x = (xmin + xmax)/2,
         z = (zmin + zmax)/2)

# Create the heatmap
ggplot(heatmap_data, aes(x = x, y = z, fill = strike_rate)) +
  geom_tile() +
  scale_fill_gradient(low = "purple", high = "orange", name = "% Called Strike", labels = scales::percent_format(accuracy = 1)) +
  scale_size(range = c(0.5, 5)) +
  labs(
    title = "Heatmap of Pitch Locations & Calls",
    x = "X Coordinate",
    y = "Z Coordinate"
  ) +
  annotate("rect", 
           xmin = left_plate_edge, 
           xmax = right_plate_edge, 
           ymin = -0.89, 
           ymax = 0.89, 
           fill = NA, color = "black") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )


