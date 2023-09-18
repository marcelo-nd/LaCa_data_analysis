library(dplyr)
library(ggfortify)

AllASVsAbundances <- read.csv("C:/Users/Marcelo/Desktop/AllASVsAbundances.csv", header=FALSE, sep=";")

asv_table <- as.data.frame(t(AllASVsAbundances))

asv_table <- dplyr::filter(asv_table, asv_table$V2 != "AbundanceRel")

patient_names <- asv_table$V1

asv_table <- as.data.frame(t(asv_table))

asv_table <- asv_table[,9:ncol(asv_table)]

asv_col_names <- make.names(c("species", patient_names[10:length(patient_names)]))

colnames(asv_table) <- asv_col_names

asv_table <- asv_table[3:nrow(asv_table),]

# Collapse rows that have the same Species ID.
# Convert to numeric
asv_table[ , c(2:12)] <- apply(asv_table[ , c(2:12)], 2, function(x) as.numeric(as.character(x)))

summary(asv_table)

# Collapse
asv_table_colapsed <- asv_table %>% 
  group_by(species) %>% 
  summarise(across(where(is.numeric), sum))

#############################
species_names <- asv_table_colapsed$species

asv_table_colapsed <- asv_table_colapsed[,2:ncol(asv_table_colapsed)]

rownames(asv_table_colapsed) <- species_names


patient_names2 <- colnames(asv_table_colapsed)

asv_table_colapsed$species <- species_names
######################


#Run this to choose the 30 most abundant ASVs only
#####

asv_table_colapsed2 <- asv_table_colapsed

asv_table_colapsed2$rowMeansCol <- rowMeans(asv_table_colapsed[,2:12])

asv_table_colapsed2 <- arrange(asv_table_colapsed2, desc(rowMeansCol))

summary(asv_table_colapsed2)


####################################
asv_table_colapsed2[ , c(1:12)] <- apply(asv_table_colapsed2[ , c(1:12)], 2, function(x) as.numeric(as.character(x)))
##################################

asv_table_colapsed <- asv_table_colapsed2[,1:12]

#####

asv_table_colapsed <- as_tibble(t(asv_table_colapsed))

colnames(asv_table_colapsed) <- asv_table_colapsed[1,]

asv_table_colapsed <- asv_table_colapsed[2:12,]

summary(asv_table_colapsed)

asv_table_colapsed$patient <- asv_col_names[2:12]

asv_table_colapsed[ , c(1:127)] <- apply(asv_table_colapsed[ , c(1:127)], 2, function(x) as.numeric(as.character(x)))

summary(asv_table_colapsed)

head(colMeans(asv_table_colapsed[ , c(1:127)]))

### PCA
### Scaling by column(species)
asv_table_colapsed30 <- scale(asv_table_colapsed[,1:30], center = TRUE, scale = TRUE)

pca_res <- prcomp(asv_table_colapsed30, scale. = TRUE)

ggplot2::autoplot(pca_res)

p <- ggplot2::autoplot(pca_res, data = asv_table_colapsed, colour = 'patient')

plotly::ggplotly(p)

p <- ggplot2::autoplot(pca_res, data = asv_table_colapsed, colour = 'patient', loadings = TRUE, loadings.colour = 'blue',
                       loadings.label = TRUE, loadings.label.size = 3)

plotly::ggplotly(p)


plotly::orca(p, "C:/Users/Marcelo/Desktop/plot.pdf")

#### Heatmaps
library(ComplexHeatmap)

table_for_heatmap <- asv_table_colapsed[,1:30]

rownames(table_for_heatmap) <- asv_table_colapsed$patient

table_for_heatmap <- as.matrix(table_for_heatmap)

table_for_heatmap <- t(table_for_heatmap)

summary(table_for_heatmap)

# Scaled by columm (i.e. by species).
table_for_heatmap <- scale(table_for_heatmap, center = TRUE, scale = TRUE)

ComplexHeatmap::Heatmap(table_for_heatmap)

