library(ggplot2)
library(ggridges)
library(dplyr)
library(forcats)

var <- 'safeh20'

d <- raw_analysis_data |> 
  filter(
    variable == var,
    year == 2022,
    !is.na(value),
    !is.na(income_group)
  ) |> 
  rename(
    un_continental_region = `UN Continental Region`
  ) |> 
  mutate(
    value = as.numeric(value),
    un_continental_region = factor(un_continental_region, levels = c("Africa", "Americas", "Asia", "Europe", "Oceania")),
    income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))
  )


d_long <- bind_rows(
  d |> mutate(facet = "All Countries", group = "All Countries"),
  d |> mutate(facet = "By Region", group = un_continental_region),
  d |> mutate(facet = "By Income Group", group = income_group)
)

d_long <- d_long |> 
  mutate(
    group = case_when(
      facet == "By Region" ~ factor(group, levels = c("Africa", "Americas", "Asia", "Europe", "Oceania")),
      facet == "By Income Group" ~ factor(group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income")),
      facet == "All Countries" ~ factor(group, levels = c("All Countries")),
      TRUE ~ factor(group) # Default to factor
    )
  )


ridge_plot <- ggplot(d_long, aes(x = value, y = group, fill = facet)) +
  geom_density_ridges(alpha = 0.7, scale = 1) +
  facet_wrap(~facet, scales = "free_y", ncol = 1) +
  labs(
    x = "Value",
    y = NULL,
    title = "Safe Water % (2022) by Region and Income Group",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10)
  ) +
  theme(legend.position = "none")

ridge_plot

ggsave("output/ridge_plot_illustration.png", ridge_plot, width = 8, height = 6, dpi = 300)