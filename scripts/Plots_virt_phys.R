# Load libraries and data ----
library(here)
library(ggplot2)
library(dplyr)
library(patchwork)
virt_phys <- read.csv(here("data","virt_phys.csv"))
virt_phys <- virt_phys %>%
  mutate(section = factor(section, levels = c("upper", "central", "lower")))

# Customised palettes
cb_palette <-c("#e14444","darkgrey","navyblue","#E0E0E0")
lb_palette <-c("#3cf065","darkgrey","navyblue","#E0E0E0")
sb_palette <-c("#5050f3","darkgrey","navyblue","#E0E0E0")
ul_palette <-c("#a0e8b2","darkgrey","navyblue","#E0E0E0")
ll_palette <-c("#3c7f49","darkgrey","navyblue","#E0E0E0")
t_palette <-c("#f0f050","darkgrey","navyblue","#E0E0E0")

# VERTICAL DISTRIBUTION ----

# Fig. 7 - A
ggplot(
  data = virt_phys %>%
    filter(element == "CB") %>%
    filter(section %in% c("upper", "central", "lower")) %>%
    mutate(method = factor(method, levels = c("virtual", "physical"))),
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
  ggtitle("Cranial Bones Vertical Distribution") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2),
    legend.position = "right"
  ) +
  labs(x = "Burial nr", y = "%") +
  scale_fill_manual(
    name = NULL, 
    values = cb_palette,
    breaks = c("upper", "central", "lower"),
    labels = c("Upper", "Central", "Lower") 
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    sec.axis = dup_axis(name = "%") 
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~ method, ncol = 2)
# Save plot
pdf(file= here("figures", "fig.7","Cranial_Bones_Vertical_A.pdf"))

# Fig. 7 - B
ggplot(
  data = virt_phys %>%
    filter(element == "LB") %>%
    filter(section %in% c("upper", "central", "lower")) %>%
    mutate(method = factor(method, levels = c("virtual", "physical"))),
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
  ggtitle("Long Bones Vertical Distribution") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2),
    legend.position = "right"
  ) +
  labs(x = "Burial nr", y = "%") +
  scale_fill_manual(
    name = NULL, 
    values = lb_palette,
    breaks = c("upper", "central", "lower"),
    labels = c("Upper", "Central", "Lower") 
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    sec.axis = dup_axis(name = "%") 
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~ method, ncol = 2)
# Save plot
pdf(file= here("figures", "fig.7","Long_Bones_Vertical_B.pdf"))

# Fig. 7 - C
ggplot(
  data = virt_phys %>%
    filter(element == "SB") %>%
    filter(section %in% c("upper", "central", "lower")) %>%
    mutate(method = factor(method, levels = c("virtual", "physical"))),
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
  ggtitle("Spongy Bone Vertical Distribution") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2),
    legend.position = "right"
  ) +
  labs(x = "Burial nr", y = "%") +
  scale_fill_manual(
    name = NULL, 
    values = sb_palette,
    breaks = c("upper", "central", "lower"),
    labels = c("Upper", "Central", "Lower") 
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    sec.axis = dup_axis(name = "%") 
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~ method, ncol = 2)
# Save plot
pdf(file= here("figures", "fig.7","Spongy_Bone_Vertical_C.pdf"))

# Fig. 8 - A
ggplot(
  data = virt_phys %>%
    filter(element == "UL") %>%
    filter(section %in% c("upper", "central", "lower")) %>%
    mutate(method = factor(method, levels = c("virtual", "physical"))),
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
  ggtitle("Upper Limbs Vertical Distribution") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2),
    legend.position = "right"
  ) +
  labs(x = "Burial nr", y = "%") +
  scale_fill_manual(
    name = NULL, 
    values = ul_palette,
    breaks = c("upper", "central", "lower"),
    labels = c("Upper", "Central", "Lower") 
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    sec.axis = dup_axis(name = "%") 
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~ method, ncol = 2)
# Save plot
pdf(file= here("figures", "fig.8","Upper_Limbs_Vertical_A.pdf"))

# Fig. 8 - B
ggplot(
  data = virt_phys %>%
    filter(element == "LL") %>%
    filter(section %in% c("upper", "central", "lower")) %>%
    mutate(method = factor(method, levels = c("virtual", "physical"))),
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
  ggtitle("Lower Limbs Vertical Distribution") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2),
    legend.position = "right"
  ) +
  labs(x = "Burial nr", y = "%") +
  scale_fill_manual(
    name = NULL, 
    values = ll_palette,
    breaks = c("upper", "central", "lower"),
    labels = c("Upper", "Central", "Lower") 
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    sec.axis = dup_axis(name = "%") 
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~ method, ncol = 2)
# Save plot
pdf(file= here("figures", "fig.8","Lower_Limbs_Vertical_B.pdf"))

# Fig. 8 - C
ggplot(
  data = virt_phys %>%
    filter(element == "T") %>%
    filter(section %in% c("upper", "central", "lower")) %>%
    mutate(method = factor(method, levels = c("virtual", "physical"))),
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
  ggtitle("Trunk Vertical Distribution") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2),
    legend.position = "right"
  ) +
  labs(x = "Burial nr", y = "%") +
  scale_fill_manual(
    name = NULL, 
    values = t_palette,
    breaks = c("upper", "central", "lower"),
    labels = c("Upper", "Central", "Lower") 
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    sec.axis = dup_axis(name = "%") 
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~ method, ncol = 2)
# Save plot
pdf(file= here("figures", "fig.8","Trunk_Vertical_C.pdf"))

# HORIZONTAL DISTRIBUTION ----

# Fig. 9 - A
(
  ggplot(
    data = virt_phys %>%
      filter(element == "CB") %>%
      filter(section %in% c("half_A", "half_B")) %>%
      mutate(method = factor(method, levels = c("virtual", "physical"))), 
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
    geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + 
    ggtitle("Cranial Bones Horizontal Distribution - Half A/B") + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 2),
      legend.position = "right"
    ) +
    labs(x = "Burial nr", y = "%") +
    scale_fill_manual(
      name = NULL, 
      values = cb_palette,
      breaks = c("half_A", "half_B"),
      labels = c("Half_A", "Half_B")
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      sec.axis = dup_axis(name = "%") 
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    facet_wrap(~ method, ncol = 2)
) /
  (
    ggplot(
      data = virt_phys %>%
        filter(element == "CB") %>%
        filter(section %in% c("half_C", "half_D")) %>%
        mutate(method = factor(method, levels = c("virtual", "physical"))), 
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
      geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + 
      ggtitle("Cranial Bones Horizontal Distribution - Half C/D") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "right"
      ) +
      labs(x = "Burial nr", y = "%") +
      scale_fill_manual(
        name = NULL, 
        values = c("half_C" = "navyblue", "half_D" = "#E0E0E0"),
        breaks = c("half_C", "half_D"),
        labels = c("Half_C", "Half_D") 
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, by = 10),
        sec.axis = dup_axis(name = "%") 
      ) +
      coord_cartesian(ylim = c(0, 100)) +
      facet_wrap(~ method, ncol = 2)
  )
# Save plot
pdf(file= here("figures", "fig.9","Cranial_Bones_Horizontal_A.pdf"))

# Fig. 9 - B
(
  ggplot(
    data = virt_phys %>%
      filter(element == "LB") %>%
      filter(section %in% c("half_A", "half_B")) %>%
      mutate(method = factor(method, levels = c("virtual", "physical"))), 
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
    geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + 
    ggtitle("Long Bones Horizontal Distribution - Half A/B") + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 2),
      legend.position = "right"
    ) +
    labs(x = "Burial nr", y = "%") +
    scale_fill_manual(
      name = NULL, 
      values = lb_palette,
      breaks = c("half_A", "half_B"),
      labels = c("Half_A", "Half_B")
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      sec.axis = dup_axis(name = "%") 
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    facet_wrap(~ method, ncol = 2)
) /
  (
    ggplot(
      data = virt_phys %>%
        filter(element == "LB") %>%
        filter(section %in% c("half_C", "half_D")) %>%
        mutate(method = factor(method, levels = c("virtual", "physical"))), 
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
      geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + 
      ggtitle("Long Bones Horizontal Distribution - Half C/D") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "right"
      ) +
      labs(x = "Burial nr", y = "%") +
      scale_fill_manual(
        name = NULL,
        values = c("half_C" = "navyblue", "half_D" = "#E0E0E0"),
        breaks = c("half_C", "half_D"),
        labels = c("Half_C", "Half_D") 
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, by = 10),
        sec.axis = dup_axis(name = "%") 
      ) +
      coord_cartesian(ylim = c(0, 100)) +
      facet_wrap(~ method, ncol = 2)
  )
# Save plot
pdf(file= here("figures", "fig.9","Long_Bones_Horizontal_B.pdf"))

# Fig. 10 - A
(
  ggplot(
    data = virt_phys %>%
      filter(element == "SB") %>%
      filter(section %in% c("half_A", "half_B")) %>%
      mutate(method = factor(method, levels = c("virtual", "physical"))), 
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
    geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + 
    ggtitle("Spongy Bone Horizontal Distribution - Half A/B") + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 2),
      legend.position = "right"
    ) +
    labs(x = "Burial nr", y = "%") +
    scale_fill_manual(
      name = NULL, 
      values = sb_palette,
      breaks = c("half_A", "half_B"),
      labels = c("Half_A", "Half_B")
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      sec.axis = dup_axis(name = "%") 
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    facet_wrap(~ method, ncol = 2)
) /
  (
    ggplot(
      data = virt_phys %>%
        filter(element == "SB") %>%
        filter(section %in% c("half_C", "half_D")) %>%
        mutate(method = factor(method, levels = c("virtual", "physical"))), 
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
      geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + 
      ggtitle("Spongy Bone Horizontal Distribution - Half C/D") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "right"
      ) +
      labs(x = "Burial nr", y = "%") +
      scale_fill_manual(
        name = NULL,
        values = c("half_C" = "navyblue", "half_D" = "#E0E0E0"),
        breaks = c("half_C", "half_D"),
        labels = c("Half_C", "Half_D") 
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, by = 10),
        sec.axis = dup_axis(name = "%") 
      ) +
      coord_cartesian(ylim = c(0, 100)) +
      facet_wrap(~ method, ncol = 2)
  )
# Save plot
pdf(file= here("figures", "fig.10","Spongy_Bone_Horizontal_A.pdf"))

# Fig. 10 - B
(
  ggplot(
    data = virt_phys %>%
      filter(element == "T") %>%
      filter(section %in% c("half_A", "half_B")) %>%
      mutate(method = factor(method, levels = c("virtual", "physical"))), 
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
    geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + 
    ggtitle("Trunk Horizontal Distribution - Half A/B") + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 2),
      legend.position = "right"
    ) +
    labs(x = "Burial nr", y = "%") +
    scale_fill_manual(
      name = NULL, 
      values = t_palette,
      breaks = c("half_A", "half_B"),
      labels = c("Half_A", "Half_B")
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      sec.axis = dup_axis(name = "%") 
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    facet_wrap(~ method, ncol = 2)
) /
  (
    ggplot(
      data = virt_phys %>%
        filter(element == "T") %>%
        filter(section %in% c("half_C", "half_D")) %>%
        mutate(method = factor(method, levels = c("virtual", "physical"))), 
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
      geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + 
      ggtitle("Trunk Horizontal Distribution - Half C/D") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "right"
      ) +
      labs(x = "Burial nr", y = "%") +
      scale_fill_manual(
        name = NULL,
        values = c("half_C" = "navyblue", "half_D" = "#E0E0E0"),
        breaks = c("half_C", "half_D"),
        labels = c("Half_C", "Half_D") 
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, by = 10),
        sec.axis = dup_axis(name = "%") 
      ) +
      coord_cartesian(ylim = c(0, 100)) +
      facet_wrap(~ method, ncol = 2)
  )
# Save plot
pdf(file= here("figures", "fig.10","Trunk_Horizontal_B.pdf"))

# Fig. 12 - A
(
  ggplot(
    data = virt_phys %>%
      filter(element == "UL") %>%
      filter(section %in% c("half_A", "half_B")) %>%
      mutate(method = factor(method, levels = c("virtual", "physical"))), 
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
    geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + 
    ggtitle("Upper Limbs Horizontal Distribution - Half A/B") + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 2),
      legend.position = "right"
    ) +
    labs(x = "Burial nr", y = "%") +
    scale_fill_manual(
      name = NULL, 
      values = ul_palette,
      breaks = c("half_A", "half_B"),
      labels = c("Half_A", "Half_B")
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      sec.axis = dup_axis(name = "%") 
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    facet_wrap(~ method, ncol = 2)
) /
  (
    ggplot(
      data = virt_phys %>%
        filter(element == "UL") %>%
        filter(section %in% c("half_C", "half_D")) %>%
        mutate(method = factor(method, levels = c("virtual", "physical"))), 
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
      geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + 
      ggtitle("Upper Limbs Horizontal Distribution - Half C/D") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "right"
      ) +
      labs(x = "Burial nr", y = "%") +
      scale_fill_manual(
        name = NULL,
        values = c("half_C" = "navyblue", "half_D" = "#E0E0E0"),
        breaks = c("half_C", "half_D"),
        labels = c("Half_C", "Half_D") 
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, by = 10),
        sec.axis = dup_axis(name = "%") 
      ) +
      coord_cartesian(ylim = c(0, 100)) +
      facet_wrap(~ method, ncol = 2)
  )
# Save plot
pdf(file= here("figures", "fig.12","Upper_Limbs_Horizontal_A.pdf"))

# Fig. 12 - B
(
  ggplot(
    data = virt_phys %>%
      filter(element == "LL") %>%
      filter(section %in% c("half_A", "half_B")) %>%
      mutate(method = factor(method, levels = c("virtual", "physical"))), 
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
    geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + 
    ggtitle("Lower Limbs Horizontal Distribution - Half A/B") + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 2),
      legend.position = "right"
    ) +
    labs(x = "Burial nr", y = "%") +
    scale_fill_manual(
      name = NULL, 
      values = ll_palette,
      breaks = c("half_A", "half_B"),
      labels = c("Half_A", "Half_B")
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      sec.axis = dup_axis(name = "%") 
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    facet_wrap(~ method, ncol = 2)
) /
  (
    ggplot(
      data = virt_phys %>%
        filter(element == "LL") %>%
        filter(section %in% c("half_C", "half_D")) %>%
        mutate(method = factor(method, levels = c("virtual", "physical"))), 
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
      geom_hline(yintercept = c(65, 35), color = "black", linetype = "dashed") + 
      ggtitle("Lower Limbs Horizontal Distribution - Half C/D") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "right"
      ) +
      labs(x = "Burial nr", y = "%") +
      scale_fill_manual(
        name = NULL,
        values = c("half_C" = "navyblue", "half_D" = "#E0E0E0"),
        breaks = c("half_C", "half_D"),
        labels = c("Half_C", "Half_D") 
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, by = 10),
        sec.axis = dup_axis(name = "%") 
      ) +
      coord_cartesian(ylim = c(0, 100)) +
      facet_wrap(~ method, ncol = 2)
  )
# Save plot
pdf(file= here("figures", "fig.12","Lower_Limbs_Horizontal_B.pdf"))
