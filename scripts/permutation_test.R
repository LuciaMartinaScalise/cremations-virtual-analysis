# Load libraries and data ----
library(here)
library(reshape2)
library(tidyverse)
virt<-read.csv(here("data","vertical_virtual"))
phys<-read.csv(here("data","vertical_physical"))
virt_HA_HB<- read.csv(here("data","horizontal_HAB_virtual"))
phys_HA_HB<- read.csv(here("data","horizontal_HAB_physical"))
virt_HC_HD<- read.csv(here("data","horizontal_HCD_virtual"))
phys_HC_HD<- read.csv(here("data","horizontal_HCD_physical"))

# VERTICAL DISTRIBUTION ----
# Data preparation 
virt<- (virt[,1:18]/virt$TOT)
phys<- (phys[,1:18]/phys$TOT)

# Initialize the 'obs' object to store observed correlations
obs <- numeric(18) 

# Initialize the 'sim' object to store permuted correlations
sim <- matrix(NA, nrow = 1000, ncol = 18) 

# Calculate observed correlations between 'physical' and 'virtual' for each column
for (j in 1:18) {    
  obs[j] <- cor(phys[, j], virt[, j])
}

# Permutation test loop: perform 1000 permutations
for (i in 1:1000) {     
  virt <- virt[sample(10), ]
  # For each permutation, calculate the correlation for each column
  for (j in 1:18) {
    sim[i, j] <- cor(phys[, j], virt[, j])
  }
}

# Convert the simulation matrix into a long format for easier plotting
sim_long <- as.data.frame(sim)
sim_long <- sim_long %>%
  rename(
    `Cranial Bones Upper` = V1, 
    `Cranial Bones Central` = V2, 
    `Cranial Bones Lower` = V3,
    `Long Bones Upper` = V4, 
    `Long Bones Central` = V5, 
    `Long Bones Lower` = V6,
    `Spongy Bone Upper` = V7, 
    `Spongy Bone Central` = V8, 
    `Spongy Bone Lower` = V9,
    `Upper Limbs Upper` = V10, 
    `Upper Limbs Central` = V11, 
    `Upper Limbs Lower` = V12,
    `Lower Limbs Upper` = V13, 
    `Lower Limbs Central` = V14, 
    `Lower Limbs Lower` = V15,
    `Trunk Upper` = V16, 
    `Trunk Central` = V17, 
    `Trunk Lower` = V18)

# Reshape dataset 
sim_long <- reshape2::melt(sim_long, variable.name = "Variable", value.name = "Correlation")

# Create a dataframe for the observed correlations
obs_df <- data.frame(
  Variable = c("Cranial Bones Upper", 
               "Cranial Bones Central", 
               "Cranial Bones Lower",
               "Long Bones Upper", 
               "Long Bones Central", 
               "Long Bones Lower",
               "Spongy Bone Upper", 
               "Spongy Bone Central", 
               "Spongy Bone Lower",
               "Upper Limbs Upper", 
               "Upper Limbs Central", 
               "Upper Limbs Lower",
               "Lower Limbs Upper", 
               "Lower Limbs Central", 
               "Lower Limbs Lower",
               "Trunk Upper", 
               "Trunk Central", 
               "Trunk Lower"),
  Correlation = obs)

# Export data frame
write.csv(obs_df, here("table_2", "Observed_correlation_vertical.csv"), row.names = FALSE)

# Plot the histogram of simulated correlations, facetted by variable
ggplot(sim_long, aes(x = Correlation)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_vline(data = obs_df, aes(xintercept = Correlation), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Simulated Correlations with Observed Values (Red Dashed Line)",
       x = "Correlation", y = "Frequency") +
  facet_wrap(~ factor(Variable, levels = c("Cranial Bones Upper", 
                                           "Cranial Bones Central", 
                                           "Cranial Bones Lower",
                                           "Long Bones Upper", 
                                           "Long Bones Central", 
                                           "Long Bones Lower",
                                           "Spongy Bone Upper", 
                                           "Spongy Bone Central", 
                                           "Spongy Bone Lower",
                                           "Upper Limbs Upper", 
                                           "Upper Limbs Central", 
                                           "Upper Limbs Lower",
                                           "Lower Limbs Upper", 
                                           "Lower Limbs Central", 
                                           "Lower Limbs Lower",
                                           "Trunk Upper", 
                                           "Trunk Central", 
                                           "Trunk Lower")), 
             scales = "free_x", ncol=3) +  # Facet by variable with free x scales only
  coord_cartesian(xlim = c(-1, 1)) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Save plot
pdf(file= here("figures", "fig.13.pdf"))

# Initialize p-value vector to store p-values for each column
p_values_two_sided <- numeric(18)

# Two-sided p-values 
for (j in 1:18) {
p_values_two_sided[j] <- 2*min((sum(obs[j] > sim[,j]) + 1)/(1000+1),
                                         (sum(obs[j] < sim[,j]) + 1)/(1000+1))
}


# p-values stored in a data frame
p_values_two_sided <- data.frame(p_values_two, row.names = c("Cranial Bones Upper", 
                                                         "Cranial Bones Central", 
                                                         "Cranial Bones Lower",
                                                         "Long Bones Upper", 
                                                         "Long Bones Central", 
                                                         "Long Bones Lower",
                                                         "Spongy Bone Upper", 
                                                         "Spongy Bone Central", 
                                                         "Spongy Bone Lower",
                                                         "Upper Limbs Upper", 
                                                         "Upper Limbs Central", 
                                                         "Upper Limbs Lower",
                                                         "Lower Limbs Upper", 
                                                         "Lower Limbs Central", 
                                                         "Lower Limbs Lower",
                                                         "Trunk Upper", 
                                                         "Trunk Central", 
                                                         "Trunk Lower"))

# Export data frame
write.csv(p_values_two_sided, here("table_2", "Two-sided_p-value_vertical.csv"), row.names = TRUE)

# HORIZONTAL DISTRIBUTION HA/HB----
# Data preparation
virt_HA_HB<- (virt_HA_HB[,1:12]/virt_HA_HB$TOT)
phys_HA_HB<- (phys_HA_HB[,1:12]/phys_HA_HB$TOT)

# Initialize the 'obs' object to store observed correlations
obs_HA_HB <- numeric(12) 

# Initialize the 'sim' object to store permuted correlations
sim_HA_HB <- matrix(NA, nrow = 1000, ncol = 12) 

# Calculate observed correlations between 'physical' and 'virtual' for each column
for (j in 1:12) {
  obs_HA_HB[j] <- cor(phys_HA_HB[, j], virt_HA_HB[, j])
}

# Permutation test loop: perform 1000 permutations
for (i in 1:1000) {
  virt_HA_HB <- virt_HA_HB[sample(10), ]
  for (j in 1:12) {
    sim_HA_HB[i, j] <- cor(phys_HA_HB[, j], virt_HA_HB[, j])}
  }

# Convert the simulation matrix into a long format for easier plotting
sim_long_HA_HB <- as.data.frame(sim_HA_HB)
sim_long_HA_HB <- sim_long_HA_HB %>%
  rename(
    `Cranial Bones Half A` = V1, 
    `Cranial Bones Half B` = V2, 
    `Long Bones Half A` = V3, 
    `Long Bones Half B` = V4, 
    `Spongy Bone Half A` = V5, 
    `Spongy Bone Half B` = V6, 
    `Upper Limbs Half A` = V7, 
    `Upper Limbs Half B` = V8, 
    `Lower Limbs Half A` = V9, 
    `Lower Limbs Half B` = V10, 
    `Trunk Half A` = V11, 
    `Trunk Half B` = V12)

sim_long_HA_HB <- reshape2::melt(sim_long_HA_HB, variable.name = "Variable", value.name = "Correlation")

# Create a dataframe for the observed correlations
obs_df_HA_HB <- data.frame(
  Variable = c("Cranial Bones Half A", 
               "Cranial Bones Half B", 
               "Long Bones Half A", 
               "Long Bones Half B", 
               "Spongy Bone Half A", 
               "Spongy Bone Half B", 
               "Upper Limbs Half A", 
               "Upper Limbs Half B", 
               "Lower Limbs Half A", 
               "Lower Limbs Half B", 
               "Trunk Half A", 
               "Trunk Half B"),
  Correlation = obs_HA_HB)

# Export data frame
write.csv(obs_df_HA_HB, here("table_2", "Observed_correlation_HA_B.csv"), row.names = FALSE)

# Plot the histogram of simulated correlations, facetted by variable
ggplot(sim_long_HA_HB, aes(x = Correlation)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_vline(data = obs_df_HA_HB, aes(xintercept = Correlation), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Simulated Correlations with Observed Values (Red Dashed Line)",
       x = "Correlation", y = "Frequency") +
  facet_wrap(~ factor(Variable, levels = c("Cranial Bones Half A", 
                                           "Cranial Bones Half B", 
                                           "Long Bones Half A", 
                                           "Long Bones Half B", 
                                           "Spongy Bone Half A", 
                                           "Spongy Bone Half B", 
                                           "Upper Limbs Half A", 
                                           "Upper Limbs Half B", 
                                           "Lower Limbs Half A", 
                                           "Lower Limbs Half B", 
                                           "Trunk Half A", 
                                           "Trunk Half B")), 
             scales = "free_x", ncol=2) +  
  coord_cartesian(xlim = c(-1, 1)) +
  theme_minimal()

# Save plot
pdf(file= here("figures", "fig.14.pdf"))

# Initialize p-value vector to store p-values for each column
p_values_perm_HA_HB <- numeric(12)

#p-values two-sided 
for (j in 1:12) {
  p_values_perm_HA_HB[j] <- 2*min((sum(obs_HA_HB[j] > sim_HA_HB[,j]) + 1)/(1000+1),
                                           (sum(obs_HA_HB[j] < sim_HA_HB[,j]) + 1)/(1000+1))
}

#p-values stored in a data frame
p_values_perm_HA_HB <- data.frame(p_values_perm_HA_HB, row.names = c("Cranial Bones Half A", 
                                                                     "Cranial Bones Half B", 
                                                                     "Long Bones Half A", 
                                                                     "Long Bones Half B", 
                                                                     "Spongy Bone Half A", 
                                                                     "Spongy Bone Half B", 
                                                                     "Upper Limbs Half A", 
                                                                     "Upper Limbs Half B", 
                                                                     "Lower Limbs Half A", 
                                                                     "Lower Limbs Half B", 
                                                                     "Trunk Half A", 
                                                                     "Trunk Half B"))

# Export data frame
write.csv(p_values_perm_HA_HB, here("table_2", "Two-sided_p-value_HA_B.csv"), row.names = TRUE)

# HORIZONTAL DISTRIBUTION HC/HD----
# Data preparation
virt_HC_HD<- (virt_HC_HD[,1:12]/virt_HC_HD$TOT)
phys_HC_HD<- (phys_HC_HD[,1:12]/phys_HC_HD$TOT)

# Initialize the 'obs' object to store observed correlations
obs_HC_HD <- numeric(12) 

# Initialize the 'sim' object to store permuted correlations
sim_HC_HD <- matrix(NA, nrow = 1000, ncol = 12) 

# Calculate observed correlations between 'physical' and 'virtual' for each column
for (j in 1:12) {
  obs_HC_HD[j] <- cor(phys_HC_HD[, j], virt_HC_HD[, j])
}

# Permutation test loop: perform 1000 permutations
for (i in 1:1000) {
  virt_HC_HD <- virt_HC_HD[sample(10), ]
  for (j in 1:12) {
    sim_HC_HD[i, j] <- cor(phys_HC_HD[, j], virt_HC_HD[, j])}
}

# Convert the simulation matrix into a long format for easier plotting
sim_long_HC_HD <- as.data.frame(sim_HC_HD)
sim_long_HC_HD <- sim_long_HC_HD %>%
  rename(
    `Cranial Bones Half C` = V1, 
    `Cranial Bones Half D` = V2, 
    `Long Bones Half C` = V3, 
    `Long Bones Half D` = V4, 
    `Spongy Bone Half C` = V5, 
    `Spongy Bone Half D` = V6, 
    `Upper Limbs Half C` = V7, 
    `Upper Limbs Half D` = V8, 
    `Lower Limbs Half C` = V9, 
    `Lower Limbs Half D` = V10, 
    `Trunk Half C` = V11, 
    `Trunk Half D` = V12)

sim_long_HC_HD <- reshape2::melt(sim_long_HC_HD, variable.name = "Variable", value.name = "Correlation")

# Create a dataframe for the observed correlations
obs_df_HC_HD <- data.frame(
  Variable = c("Cranial Bones Half C", 
               "Cranial Bones Half D", 
               "Long Bones Half C", 
               "Long Bones Half D", 
               "Spongy Bone Half C", 
               "Spongy Bone Half D", 
               "Upper Limbs Half C", 
               "Upper Limbs Half D", 
               "Lower Limbs Half C", 
               "Lower Limbs Half D", 
               "Trunk Half C", 
               "Trunk Half D"),
  Correlation = obs_HC_HD)

# Export data frame
write.csv(obs_df_HC_HD, here("table_2", "Observed_correlation_HC_D.csv"), row.names = FALSE)

# Plot the histogram of simulated correlations, facetted by variable
ggplot(sim_long_HC_HD, aes(x = Correlation)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_vline(data = obs_df_HC_HD, aes(xintercept = Correlation), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Simulated Correlations with Observed Values (Red Dashed Line)",
       x = "Correlation", y = "Frequency") +
  facet_wrap(~ factor(Variable, levels = c("Cranial Bones Half C", 
                                           "Cranial Bones Half D", 
                                           "Long Bones Half C", 
                                           "Long Bones Half D", 
                                           "Spongy Bone Half C", 
                                           "Spongy Bone Half D", 
                                           "Upper Limbs Half C", 
                                           "Upper Limbs Half D", 
                                           "Lower Limbs Half C", 
                                           "Lower Limbs Half D", 
                                           "Trunk Half C", 
                                           "Trunk Half D")), 
             scales = "free_x", ncol=2) +  
  scale_x_continuous(limits = c(-1, 1)) + # Strict x-axis limits
  coord_cartesian(ylim = c(0,200))+
  theme_minimal()

# Save plot
pdf(file= here("figures", "fig.15.pdf"))

# Initialize p-value vector to store p-values for each column
p_values_perm_HC_HD <- numeric(12)

#p-values two-sided suggested by Enrico
for (j in 1:12) {
  p_values_perm_HC_HD[j] <- 2*min((sum(obs_HC_HD[j] > sim_HC_HD[,j]) + 1)/(1000+1),
                                  (sum(obs_HC_HD[j] < sim_HC_HD[,j]) + 1)/(1000+1))
}

#p-values stored in a data frame
p_values_perm_HC_HD <- data.frame(p_values_perm_HC_HD, row.names = c("Cranial Bones Half C", 
                                                                     "Cranial Bones Half D", 
                                                                     "Long Bones Half C", 
                                                                     "Long Bones Half D", 
                                                                     "Spongy Bone Half C", 
                                                                     "Spongy Bone Half D", 
                                                                     "Upper Limbs Half C", 
                                                                     "Upper Limbs Half D", 
                                                                     "Lower Limbs Half C", 
                                                                     "Lower Limbs Half D", 
                                                                     "Trunk Half C", 
                                                                     "Trunk Half D"))

# Export data frame
write.csv(p_values_perm_HC_HD, here("table_2", "Two-sided_p-value_HC_D.csv"), row.names = TRUE)
