# Load required libraries
library(tidyverse)
library(lubridate)
library(scales)
library(treemapify)
library(ggrepel)

# Read data
df <- read_csv("/Users/gfq/Desktop/生信分析/Manuscript data of my written project/GFI Company Database US Website View .csv") 

# Clean and prepare data
clean_df <- df %>%
  mutate(`Year founded` = as.numeric(`Year founded`),
         `Company type` = factor(`Company type`,
                                 levels = c("Specialized (focused on alternative proteins)",
                                               "Diversified")))

# 1. Founding Year Trends (Area Chart)
time_plot <- clean_df %>%
  filter(`Year founded` >= 2000) %>%
  count(`Year founded`) %>%
  ggplot(aes(x = `Year founded`, y = n)) +
  geom_area(fill = "#4d9221", alpha = 0.4) +
  geom_smooth(method = "loess", color = "#276419", se = FALSE) +
  labs(title = "Industry Growth Timeline",
       subtitle = "Emergence of companies by founding year",
       x = "Year", y = "Number of New Companies") +
  scale_x_continuous(breaks = seq(2000, 2025, 5))

ggsave("founding_trend.png", time_plot, width = 10, height = 6)

# Load required libraries
library(tidyverse)
library(tm)
library(wordcloud)
library(viridis)
library(ggrepel)
library(scales)

# Read and preprocess data
companies <- read_csv("/Users/gfq/Desktop/生信分析/Manuscript data of my written project/GFI Company Database US Website View .csv") %>%
  mutate(
    across(c("Technology Focus", "Operating Regions"), 
           ~str_split(., ",\\s*")),
    # Corrected AI_related column
    AI_related = ifelse(
      str_detect(`Technology Focus`, "AI|artificial intelligence|machine learning") |
        str_detect(`Brief Description`, "AI|artificial intelligence|machine learning"),
      "AI", "Non-AI"),
    Scale_challenge = ifelse(
      str_detect(`Technology Focus`, "upscale|scale|bioprocess design"),
      "Scale Tech", "Other")
  ) %>%
  unnest(`Technology Focus`) %>%
  filter(!is.na(`Technology Focus`))

# 2. Regional Innovation Clusters (All Countries)
# Load required libraries
library(sf)
library(viridis)
library(rnaturalearth)
library(dplyr)
library(ggplot2)
library(ggrepel)  # For better label placement

# Summarize the data to find the top 3 technology focuses for each country
top_tech_focus_1 <- companies %>%
  group_by(`HQ Country`, `Technology Focus`) %>%
  count() %>%
  arrange(`HQ Country`, desc(n)) %>%
  group_by(`HQ Country`) %>%
  slice_head(n = 3) %>% 
  ungroup() %>%
  filter(!is.na(`HQ Country`))

# Summarize the data to find the top technology focuses for each country
top_tech_focus <- companies %>%
  group_by(`HQ Country`, `Technology Focus`) %>%
  count() %>%
  arrange(`HQ Country`, desc(n)) %>%
  group_by(`HQ Country`) %>%
  slice_head(n = 1) %>%  # Keep top technology focus (with the biggest n) for each country
  ungroup()

# Standardize country names to match the world map data
top_tech_focus <- top_tech_focus %>%
  mutate(`HQ Country` = case_when(
    `HQ Country` == "United States" ~ "United States of America",
    `HQ Country` == "Mainland China" ~ "China",
    `HQ Country` == "Scotland" ~ "United Kindom",
    `HQ Country` == "Czech Republic" ~ "Czechia",
    `HQ Country` == "Hong Kong SAR" ~ "Hong Kong",
    TRUE ~ `HQ Country`
  )) %>%
  filter(!is.na(`HQ Country`))

# Load world map data using rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join your data with the world map data
map_data <- world %>%
  left_join(top_tech_focus, by = c("name" = "HQ Country"))

# Filter out countries that don't appear in your dataset
map_data <- map_data %>%
  filter(!is.na(`Technology Focus`))  # Only keep rows where Technology Focus is available

# Extract centroids for label positioning
map_data <- map_data %>%
  mutate(
    centroid = st_centroid(geometry),  # Extract centroids
    x = st_coordinates(centroid)[, 1],  # Get x coordinates from centroids
    y = st_coordinates(centroid)[, 2]   # Get y coordinates from centroids
  )

# Define a custom color palette for better visualization
tech_focus_colors <- c(
  "Crop development" = "#1f77b4",  # Blue
  "Feedstocks" = "#ff7f0e",  # Orange
  "Host strain development" = "#8c564b", # Brown
  "Bioprocess design" = "#bcbd22",  # Olive
  "Ingredient optimization" = "#9467bd",  # Purple
  "End product formulation and manufacturing" = "#2ca02c", # Green
  "Cell culture media" = "#e377c2"  # Pink
)

regional_tech_map <- ggplot(data = map_data) +
  # Fill countries based on their Technology Focus
  geom_sf(aes(fill = `Technology Focus`), color = "white", size = 0.1) +  
  scale_fill_manual(values = tech_focus_colors) +  # Use custom color palette
  
  # Add borders for countries that don't appear in your dataset (gray)
  geom_sf(data = world %>% filter(!(name %in% unique(map_data$name))), color = "gray", fill = NA, size = 0.5) +
  
  # Merge Taiwan and Hong Kong with China in the dataset (Color them same as China)
  
  geom_sf(data = map_data %>%
            
            mutate(name = ifelse(name %in% c("Taiwan", "Hong Kong"), "China", name),
                   
                   `Technology Focus` = ifelse(name == "China", "Ingredient optimization", `Technology Focus`)) %>%
            
            filter(!is.na(name)), 
          
          aes(fill = `Technology Focus`), color = "white", size = 0.1) +
  
  # Add country names (only once, for countries in the dataset)
  geom_text_repel(
    data = map_data %>%
      filter(!is.na(`Technology Focus`), !(name %in% c("Taiwan", "Hong Kong"))) %>%
      group_by(name) %>%
      slice_head(n = 1),  # Ensure only one country name per country
    aes(x = x, y = y, label = name),
    size = 3.5,
    color = "black",
    fontface = "bold",
    box.padding = 0.5,  # Adjust spacing
    point.padding = 0.5,  # Adjust spacing
    max.overlaps = Inf  # Allow unlimited overlaps (ggrepel will handle it)
  ) +
  
  labs(
    title = "Global Distribution of Technology Focus on Alternative Proteins",
    subtitle = "Top Technology Focuses per Country",
    fill = "Technology Focus"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid = element_blank(),  # Remove gridlines
    plot.background = element_blank(),  # Clean background
    plot.title = element_text(size = 16, face = "bold"),  # Adjust title size
    plot.subtitle = element_text(size = 12),  # Adjust subtitle size
    plot.margin = margin(10, 10, 10, 10),  # Add margins for better readability in print
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_blank()  # Remove any borders from the map
  ) +
  coord_sf(expand = FALSE) +  # Prevent axes from being shown
  theme_void()  # Remove all axis, ticks, and coordinates

# Display the plot
print(regional_tech_map)

# Save the plot
ggsave("regional_tech_map.png", regional_tech_map, width = 36, height = 20, dpi = 300)

# Create the stacked barplot

stacked_barplot <- ggplot(top_tech_focus_1, aes(x = `HQ Country`, y = n, fill = `Technology Focus`)) +
  
  geom_bar(stat = "identity", position = "stack") +
  
  scale_fill_viridis(discrete = TRUE, name = "Technology Focus") +
  
  labs(
    
    x = "Region",
    
    y = "Number of Companies"
    
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    
    legend.position = "bottom",
    
    plot.title = element_text(size = 14, face = "bold")
    
  ) +
  
  coord_flip()

print(stacked_barplot)

ggsave("stacked_barplot.png", plot = stacked_barplot, width = 15, height = 12, dpi = 300)

# 3-1. Protein Category vs. Product Types
# Read and preprocess data
companies <- read_csv("/Users/gfq/Desktop/生信分析/Manuscript data of my written project/GFI Company Database US Website View .csv") %>%
  mutate(
    across(c("Technology Focus", "Operating Regions"), 
           ~str_split(., ",\\s*")),
    # Corrected AI_related column
    AI_related = ifelse(
      str_detect(`Technology Focus`, "AI|artificial intelligence|machine learning") |
        str_detect(`Brief Description`, "AI|artificial intelligence|machine learning"),
      "AI", "Non-AI"),
    Scale_challenge = ifelse(
      str_detect(`Technology Focus`, "upscale|scale|bioprocess design"),
      "Scale Tech", "Other")
  )

# Stacked Bar Plot: Protein Category vs. Product Types
# Step to filter the top 20 protein categories

top_protein_categories <- companies %>%
  
  filter(!is.na(`Protein category`)) %>%
  
  count(`Protein category`) %>%
  
  arrange(desc(n)) %>%
  
  slice_head(n = 20) %>%
  
  pull(`Protein category`)

# Step to filter the top 50 product types

top_product_types <- companies %>%
  
  filter(!is.na(`Product Type`)) %>%
  
  count(`Product Type`) %>%
  
  arrange(desc(n)) %>%
  
  slice_head(n = 50) %>%
  
  pull(`Product Type`)

# Create the stacked bar plot for top 20 protein categories and top 50 product types

protein_product_stacked_plot <- companies %>%
  
  filter(`Protein category` %in% top_protein_categories & 
           
           `Product Type` %in% top_product_types) %>%
  
  count(`Protein category`, `Product Type`) %>%
  
  group_by(`Protein category`) %>%
  
  mutate(percent = n / sum(n)) %>%
  
  ungroup() %>%
  
  ggplot(aes(x = `Protein category`, y = percent, fill = `Product Type`)) +
  
  geom_col(position = "stack") +
  
  scale_fill_viridis(discrete = TRUE) +
  
  labs(title = "Product Type Distribution by Protein Category",
       
       subtitle = "Proportion of product types within each protein category",
       
       x = "Protein Category",
       
       y = "Proportion",
       
       fill = "Product Type") +
  
  theme_minimal() +
  
  theme(
    
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    
    legend.position = "bottom"
    
  )

# Save as PNG with larger dimensions
ggsave("protein_product_stacked_plot.png", protein_product_stacked_plot, width = 22, height = 12, dpi = 300)


# 3-2. Protein Category vs. Technology Focus
# Read and preprocess data
companies <- read_csv("/Users/gfq/Desktop/生信分析/Manuscript data of my written project/GFI Company Database US Website View .csv") %>%
  mutate(
    across(c("Technology Focus", "Operating Regions"), 
           ~str_split(., ",\\s*")),
    # Corrected AI_related column
    AI_related = ifelse(
      str_detect(`Technology Focus`, "AI|artificial intelligence|machine learning") |
        str_detect(`Brief Description`, "AI|artificial intelligence|machine learning"),
      "AI", "Non-AI"),
    Scale_challenge = ifelse(
      str_detect(`Technology Focus`, "upscale|scale|bioprocess design"),
      "Scale Tech", "Other")
  ) %>%
  unnest(`Technology Focus`) %>%
  filter(!is.na(`Technology Focus`))

# Step to filter the top 30 protein categories

top_protein_categories <- companies %>%
  
  filter(!is.na(`Protein category`)) %>%
  
  count(`Protein category`) %>%
  
  arrange(desc(n)) %>%
  
  slice_head(n = 30) %>%
  
  pull(`Protein category`)

# Create the protein vs. technology focus plot for top 30 protein categories

protein_tech_plot <- companies %>%
  filter(`Protein category` %in% top_protein_categories & !is.na(`Technology Focus`)) %>%
  count(`Protein category`, `Technology Focus`) %>%
  ggplot(aes(x = `Protein category`, y = `Technology Focus`, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "magma", direction = -1) +  # Heatmap color scale
  labs(title = "Protein Category vs. Technology Focus",
       subtitle = "Distribution of protein sources across technology focuses",
       x = "Protein Category",
       y = "Technology Focus",
       fill = "Number of Companies") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Save as PNG with larger dimensions
ggsave("protein_tech_heatmap.png", protein_tech_plot, width = 16, height = 12, dpi = 300)


# 4. Novel Ingredient Word Cloud
# Read and preprocess data
companies <- read_csv("/Users/gfq/Desktop/生信分析/Manuscript data of my written project/GFI Company Database US Website View .csv") %>%
  mutate(
    across(c("Technology Focus", "Operating Regions"), 
           ~str_split(., ",\\s*")),
    # Corrected AI_related column
    AI_related = ifelse(
      str_detect(`Technology Focus`, "AI|artificial intelligence|machine learning") |
        str_detect(`Brief Description`, "AI|artificial intelligence|machine learning"),
      "AI", "Non-AI"),
    Scale_challenge = ifelse(
      str_detect(`Technology Focus`, "upscale|scale|bioprocess design"),
      "Scale Tech", "Other")
  )

# Create the corpus for ingredient type (ensure 'companies$Ingredient Type' is correctly referenced)
ingredient_corpus <- Corpus(VectorSource(na.omit(companies$`Ingredient Type`))) %>%
  tm_map(content_transformer(tolower)) %>%       # Convert to lowercase
  tm_map(removePunctuation) %>%                  # Remove punctuation
  tm_map(removeNumbers) %>%                      # Remove numbers (if any)
  tm_map(removeWords, stopwords("english")) %>%  # Remove common stopwords
  tm_map(stripWhitespace)                        # Remove extra whitespace

# Generate the word cloud with adjusted settings for clarity and professionalism
wordcloud(
  ingredient_corpus,
  max.words = 50, # Show top 50 words
  colors = viridis(10), # Use viridis color palette
  scale = c(3, 0.5), # Adjust word size scaling
  main = "Emerging Ingredients in Cellular Agriculture",
  random.order = FALSE, # Keep most frequent words in center
  rot.per = 0.2, # Rotate words by 20% for better placement
  min.freq = 1, # Include words with at least a frequency of 1
  family = "sans", # Use a different font for readability
  vfont = c("sans serif", "plain")
)


# Save as PNG with higher resolution
png("ingredient_wordcloud.png", width = 800, height = 800)
wordcloud(
  ingredient_corpus,
  max.words = 50,
  colors = viridis(10),
  scale = c(3, 0.5),
  random.order = FALSE, # Keep frequent words centered
  rot.per = 0.2, # 20% rotation
  min.freq = 2
)
dev.off()

# 5. Market Readiness Analysis
# Read and preprocess data
companies <- read_csv("/Users/gfq/Desktop/生信分析/Manuscript data of my written project/GFI Company Database US Website View .csv") %>%
  mutate(
    across(c("Technology Focus", "Operating Regions"), 
           ~str_split(., ",\\s*")),
    # Corrected AI_related column
    AI_related = ifelse(
      str_detect(`Technology Focus`, "AI|artificial intelligence|machine learning") |
        str_detect(`Brief Description`, "AI|artificial intelligence|machine learning"),
      "AI", "Non-AI"),
    Scale_challenge = ifelse(
      str_detect(`Technology Focus`, "upscale|scale|bioprocess design"),
      "Scale Tech", "Other")
  ) %>%
  unnest(`Technology Focus`) %>%
  filter(!is.na(`Technology Focus`))

# Step 1: Calculate the top 20 Product Types based on frequency (n)
top_product_types <- companies %>%
  filter(`Product Type` != "Ingredients") %>%
  count(`Product Type`) %>%
  top_n(20, n) %>%
  pull(`Product Type`)


# Step 2: Filter the data to keep only the top 20 Product Types and plot
product_tech_plot <- companies %>%
  filter(`Product Type` != "Ingredients") %>%
  count(`Product Type`, `Technology Focus`) %>%
  filter(`Product Type` %in% top_product_types) %>%  # Filter by top 20 Product Types
  filter(n > 1) %>%  # Keep only rows where n > 1
  group_by(`Product Type`) %>%
  top_n(3, n) %>%
  ungroup() %>%
  ggplot(aes(x = n, y = reorder(`Product Type`, n), 
             color = `Technology Focus`)) +
  geom_point(size = 5) +
  geom_text_repel(aes(label = `Technology Focus`), 
                  size = 3, force = 20, max.overlaps = 30) + 
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Technology-Product Matrix",
       subtitle = "Dominant technological approaches for consumer-facing products",
       x = "Number of Companies",
       y = "Product Category") +
  scale_x_continuous(labels = scales::comma_format(accuracy = 1)) + 
  theme_minimal() +
  theme(legend.position = "none")

# Check the filtered product data
product_data <- companies %>%
  filter(`Product Type` != "Ingredients") %>%
  count(`Product Type`, `Technology Focus`) %>%
  filter(`Product Type` %in% top_product_types) %>%
  filter(n > 1)

# View the top filtered data
head(product_data)

# Save as PNG
ggsave("market_readiness_top_20.png", product_tech_plot, width = 12, height = 9, dpi = 300)

