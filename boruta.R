set.seed(1) 
n_repeticoes <- 30 # Select the number of blocks of iterations in boruta 

resultados_rankings <- list()

for (i in 1:n_repeticoes) {
  # Running the Boruta algorithm
  boruta_result <- Boruta(Classe ~ ., data = insert.your.data doTrace = 1)
  
  # Getting the importance statistics
  boruta_importancia <- attStats(boruta_result)
  
  # Sorting attributes by the average importance value
  boruta_importancia <- boruta_importancia[order(boruta_importancia$meanImp, decreasing = TRUE), ]
  
  # Extracting the attribute ranking
  ranking_atual <- rownames(boruta_importancia)
  
  # Saving the ranking for this iteration
  resultados_rankings[[i]] <- ranking_atual
  
  # Displaying progress
  if (i %% 1 == 0) cat(" Iteration Block", i, "of", n_repeticoes, "\n")
}

# Consolidate the rankings into a matrix for analysis
library(dplyr)
rankings_df <- do.call(rbind, lapply(resultados_rankings, function(x) {
  data.frame(Atributo = x, Posicao = seq_along(x))
}))

# Counting the frequency of each attribute at each position in the ranking
frequencias <- rankings_df %>% 
  group_by(Atributo, Posicao) %>% 
  summarise(Frequencia = n(), .groups = "drop")

# Transforming to a more user-friendly format
frequencias_wide <- frequencias %>% 
  pivot_wider(names_from = Posicao, values_from = Frequencia, values_fill = 0)

# Displaying the frequency table
print(frequencias_wide)


# BOXPLOT

# Adding boxplots for the position of each attribute
frequencias_boxplot <- rankings_df %>%
  group_by(Atributo, Posicao) %>%
  summarise(Frequencia = n(), .groups = "drop")

ggplot(rankings_df, aes(x = reorder(Atributo, Posicao, median), y = Posicao)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = 0.7, fill = "grey80", color = "black") +
  coord_flip() + # Rotates for easier label reading
  labs(
    title = "Frequency of Position by Attribute",
    x = "Attributes",
    y = "Position in Ranking"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "none" # Keeps the legend hidden
  )
