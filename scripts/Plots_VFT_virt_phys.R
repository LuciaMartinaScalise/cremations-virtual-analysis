library(dplyr)

# Creating separate columns for methods percentages 
data <- data %>%
  group_by(burial, element) %>%
  mutate(
    # Calculate percentage for virtual method
    element_virt_perc = if_else(method == "virtual", 
                                (area3d_tot_element / area3d_tot_element[section == "total" & method == "virtual"]) * 100, 
                                NA_real_),
    # Calculate percentage for physical method
    element_phys_perc = if_else(method == "physical", 
                                (area3d_tot_element / area3d_tot_element[section == "total" & method == "physical"]) * 100, 
                                NA_real_)
  ) %>%
  ungroup()

#1. SETTING DATA-----
# Creating a single column for method percentages - what I did
virt_phys <- virt_phys %>%
  group_by(burial, element) %>%
  mutate(
    # Calculate percentage for both methods and store in one column `element_perc`
    element_perc = if_else(
      section != "total",  # Only calculate for non-total sections
      (area3d_tot_element / area3d_tot_element[section == "total" & method == method]) * 100,
      NA_real_
    )
  ) %>%
  ungroup()

#2. VERTICAL DISTRIBUTION-----
# Plotting - warped plots - with patterning
ggplot(
  data = virt_phys %>%
    filter(element == "CB") %>%
    filter(section %in% c("upper", "central", "lower")) %>%
    mutate(method = factor(method, levels = c("virtual", "physical"))), # Reorder factor levels
  aes(
    x = factor(burial),
    y = element_perc,
    fill = section
  )
) +
  geom_col_pattern(
    aes(pattern = method),
    position = "stack",
    color = "black",
    pattern_fill = NA, # Remove dashes in the legend
    pattern_density = 0.1,
    pattern_angle = 45,
    pattern_spacing = 0.05
  ) +
  ggtitle("Cranial bones Vertical Distribution") + #TITLE IS NOT ALWAYS NEEDED
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2), #TITLE IS NOT ALWAYS NEEDED
    legend.position = "right"
  ) +
  labs(x = "Burial nr", y = "%") +
  scale_fill_manual(
    name = NULL, # Removes legend label
    values = cb_palette,
    breaks = c("upper", "central", "lower"),
    labels = c("Upper", "Central", "Lower") # Clean labels
  ) +
  scale_pattern_manual(
    name = NULL, # Removes legend label
    values = c(physical = "none", virtual = "stripe"), # Define patterns
    labels = c("Physical", "Virtual") # Clean labels
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    sec.axis = dup_axis(name = "%") # Add secondary y-axis
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~ method, ncol = 2)  # Separate columns for virtual and physical methods

#Plotting - warped plots - without patterning - VERTICAL
ggplot(
  data = virt_phys %>%
    filter(element == "T") %>%
    filter(section %in% c("upper", "central", "lower")) %>%
    mutate(method = factor(method, levels = c("virtual", "physical"))), # Reorder factor levels
  aes(
    x = factor(burial),
    y = element_perc,
    fill = section
  )
) +
  geom_col(
    position = "stack",
    color = "black"
  ) +
  ggtitle("Trunk Vertical Distribution") + #TITLE IS NOT ALWAYS NEEDED
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2),
    legend.position = "right"
  ) +
  labs(x = "Burial nr", y = "%") +
  scale_fill_manual(
    name = NULL, # Removes legend label
    values = t_palette,
    breaks = c("upper", "central", "lower"),
    labels = c("Upper", "Central", "Lower") # Clean labels
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    sec.axis = dup_axis(name = "%") # Add secondary y-axis
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~ method, ncol = 2)

#3. HORIZONTAL DISTRIBUTION-------
#Plotting - warped plots - without patterning - HORIZONTAL - COMBINED
library(patchwork)

(
  ggplot(
    data = virt_phys %>%
      filter(element == "CB") %>%
      filter(section %in% c("half_A", "half_B")) %>%
      mutate(method = factor(method, levels = c("virtual", "physical"))), # Reorder factor levels
    aes(
      x = factor(burial),
      y = element_perc,
      fill = section
    )
  ) +
    geom_col(
      position = "stack",
      color = "black"
    ) +
    ggtitle("Cranial Bones Horizontal Distribution - Half A/B") + #TITLE IS NOT ALWAYS NEEDED
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 2),
      legend.position = "right"
    ) +
    labs(x = "Burial nr", y = "%") +
    scale_fill_manual(
      name = NULL, # Removes legend label
      values = cb_palette,
      breaks = c("half_A", "half_B"),
      labels = c("Half_A", "Half_B") # Clean labels
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      sec.axis = dup_axis(name = "%") # Add secondary y-axis
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    facet_wrap(~ method, ncol = 2)
) /
  (
    ggplot(
      data = virt_phys %>%
        filter(element == "CB") %>%
        filter(section %in% c("half_C", "half_D")) %>%
        mutate(method = factor(method, levels = c("virtual", "physical"))), # Reorder factor levels
      aes(
        x = factor(burial),
        y = element_perc,
        fill = section
      )
    ) +
      geom_col(
        position = "stack",
        color = "black"
      ) +
      ggtitle("Cranial Bones Horizontal Distribution - Half C/D") + #TITLE IS NOT ALWAYS NEEDED
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "right"
      ) +
      labs(x = "Burial nr", y = "%") +
      scale_fill_manual(
        name = NULL, # Removes legend label
        values = c("half_C" = "navyblue", "half_D" = "#E0E0E0"),
        breaks = c("half_C", "half_D"),
        labels = c("Half_C", "Half_D") # Clean labels
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, by = 10),
        sec.axis = dup_axis(name = "%") # Add secondary y-axis
      ) +
      coord_cartesian(ylim = c(0, 100)) +
      facet_wrap(~ method, ncol = 2)
  )

#plot reference lines to help in the description
(
  ggplot(
    data = virt_phys %>%
      filter(element == "T") %>%
      filter(section %in% c("half_A", "half_B")) %>%
      mutate(method = factor(method, levels = c("virtual", "physical"))), # Reorder factor levels
    aes(
      x = factor(burial),
      y = element_perc,
      fill = section
    )
  ) +
    geom_col(
      position = "stack",
      color = "black"
    ) +
    geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + # Add horizontal lines
    ggtitle("Trunk Horizontal Distribution - Half A/B") + #TITLE IS NOT ALWAYS NEEDED
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 2),
      legend.position = "right"
    ) +
    labs(x = "Burial nr", y = "%") +
    scale_fill_manual(
      name = NULL, # Removes legend label
      values = t_palette,
      breaks = c("half_A", "half_B"),
      labels = c("Half_A", "Half_B") # Clean labels
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      sec.axis = dup_axis(name = "%") # Add secondary y-axis
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    facet_wrap(~ method, ncol = 2)
) /
  (
    ggplot(
      data = virt_phys %>%
        filter(element == "T") %>%
        filter(section %in% c("half_C", "half_D")) %>%
        mutate(method = factor(method, levels = c("virtual", "physical"))), # Reorder factor levels
      aes(
        x = factor(burial),
        y = element_perc,
        fill = section
      )
    ) +
      geom_col(
        position = "stack",
        color = "black"
      ) +
      geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + # Add horizontal lines
      ggtitle("Trunk Horizontal Distribution - Half C/D") + #TITLE IS NOT ALWAYS NEEDED
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "right"
      ) +
      labs(x = "Burial nr", y = "%") +
      scale_fill_manual(
        name = NULL, # Removes legend label
        values = c("half_C" = "navyblue", "half_D" = "#E0E0E0"),
        breaks = c("half_C", "half_D"),
        labels = c("Half_C", "Half_D") # Clean labels
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, by = 10),
        sec.axis = dup_axis(name = "%") # Add secondary y-axis
      ) +
      coord_cartesian(ylim = c(0, 100)) +
      facet_wrap(~ method, ncol = 2)
  )

