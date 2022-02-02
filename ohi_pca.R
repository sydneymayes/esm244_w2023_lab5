library(tidyverse)
library(ggfortify)

ohi_rgns <- read_csv('data/rgn_names.csv') %>%
  select(rgn_id, georgn) %>%
  distinct()
ohi <- read_csv('data/scores_2021-11-17.csv') %>%
  left_join(ohi_rgns, by = c('region_id' = 'rgn_id')) %>%
  filter(dimension == 'score') %>%
  filter(nchar(goal) == 2) %>%
  filter(year == 2021) %>%
  spread(goal, score) %>%
  drop_na()

ohi_pca <- prcomp(ohi %>% 
                    select(-dimension, -region_id, -year, -georgn),
                  scale = TRUE)

autoplot(ohi_pca,
         data = ohi,
         colour = 'georgn',
         loadings = TRUE,
         loadings.label = TRUE,
         loadings.colour = 'grey50',
         loadings.label.colour = "black",
         loadings.label.vjust = -0.5
) +
  theme_minimal() +
  theme(legend.position = 'none') +
  scale_color_brewer(palette = 'Dark2')

screeplot(ohi_pca, type = 'barplot')

ohi_var_expl <- data.frame(
  v = (ohi_pca$sdev^2) / sum(ohi_pca$sdev^2),
  pc = fct_inorder(colnames(ohi_pca$rotation))
) %>%
  mutate(pct_lbl = paste0(round(v*100, 1), '%'))

ggplot(ohi_var_expl, aes(x = pc, y = v)) +
  geom_col(color = 'darkblue', fill = 'darkcyan', alpha = .5) +
  geom_text(aes(label = pct_lbl), vjust = 0, nudge_y = .002) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = 'Principal component', y = 'Variance explained')

# See the loadings (weighting for each principal component)
ohi_pca$rotation

### totally independent PCA
n_obs <- 100
indep_df <- data.frame(
  x = rnorm(n = n_obs),
  y = rnorm(n = n_obs),
  z = rnorm(n = n_obs),
  w = runif(n = n_obs))

indep_pca <- prcomp(indep_df)
autoplot(indep_pca,
         loadings = TRUE,
         loadings.label = TRUE,
         loadings.colour = 'red',
         loadings.label.colour = "red",
         loadings.label.vjust = -0.5
) +
  theme_minimal()
