library(ggplot2)
library(ggfortify)

# Read Data
# Only 10 species: "A. octavius", "C. accolens", "C. propinquum", "C. pseudodiphtericum", "C. tuberculosteraicum"
# "C. acnes", "C. granolosum", "D. pigrum", "S. aureus", "S. epidermidis", "S. lugdunensis
laca_metab <- readxl::read_excel("C:/Users/Marcelo/OneDrive - UT Cloud/1_Postdoc Tü/Sci/NoseSynComProject/LaCa_data/Metabolic_screening_LaCa_collection_230823.xlsx", sheet = "Tabelle1")
# 10 species + "C. avidum"
laca_metab <- readxl::read_excel("C:/Users/Marcelo/OneDrive - UT Cloud/1_Postdoc Tü/Sci/NoseSynComProject/LaCa_data/Metabolic_screening_LaCa_collection_230823.xlsx", sheet = "Tabelle2")

# Remove the rows with missing values.
laca_metab <- na.omit(laca_metab)


# Run to select only "Growth" variables.
laca_metab2 <- dplyr::select(laca_metab, "Strain number", "Species", "name", dplyr::contains("Growth"))
# Select quantitative variables for PCA
laca_metab_q <- laca_metab2[,4:8]

# Run to select only "Activities" variables.
laca_metab2 <- dplyr::select(laca_metab, -dplyr::contains("Growth"))
# Select quantitative variables for PCA
laca_metab_q <- laca_metab2[,5:11]


# Run only for including all variables. Select quantitative variables for PCA
laca_metab_q <- laca_metab[,5:16]

# We have to put row.names with 
row.names(laca_metab_q) <- make.names(laca_metab$`Strain number`)

# Calculate PCA. Important to scale the variables ("scale. = TRUE")
pca_res <- prcomp(laca_metab_q, scale. = TRUE)

# First simple plot.
ggplot2::autoplot(pca_res)

# Graph with only dots colored by bacterial species
p1 <- ggplot2::autoplot(pca_res, data = na.omit(laca_metab), colour = 'Species')

plotly::ggplotly(p1)

# Graph with only dots colored by volunteer
p2 <- ggplot2::autoplot(pca_res, data = na.omit(laca_metab), colour = 'volunteer')

plotly::ggplotly(p2)

# Graph with dots, labels and axis. Dots couloured by bacterial species
p3 <- ggplot2::autoplot(pca_res, data = na.omit(laca_metab), colour = 'Species', loadings = TRUE, loadings.colour = "#0072B2",
                       loadings.label = TRUE, loadings.label.size = 3, loadings.label.colour = "#0072B2", label = TRUE)

plotly::ggplotly(p3)

# Graph without dots, only labels and axis. Dots couloured by bacterial species
p4 <- ggplot2::autoplot(pca_res, data = na.omit(laca_metab), colour = 'Species', loadings = TRUE, loadings.colour = "#0072B2",
                        loadings.label = TRUE, loadings.label.size = 3, loadings.label.colour = "#0072B2", label = TRUE, shape = FALSE)

plotly::ggplotly(p4)

# Graph with only dots, no labels and axis. Dots couloured by bacterial species
p5 <- ggplot2::autoplot(pca_res, data = na.omit(laca_metab), colour = 'Species', loadings = TRUE, loadings.colour = "#0072B2",
                        loadings.label = TRUE, loadings.label.size = 3, loadings.label.colour = "#0072B2", label = FALSE, shape = TRUE)

plotly::ggplotly(p5)
plot(p5)

# Graph with only dots, no labels and axis. Dots couloured by bacterial species
p6 <- ggplot2::autoplot(pca_res, data = na.omit(laca_metab), colour = 'volunteer', loadings = TRUE, loadings.colour = "#0072B2",
                        loadings.label = TRUE, loadings.label.size = 3, loadings.label.colour = "#0072B2", label = FALSE, shape = TRUE)

plotly::ggplotly(p6)
plot(p6)

# Graph with only dots, no labels and axis. Dots shape is "volunteer" and color is "species".
p7 <- ggplot2::autoplot(pca_res, data = na.omit(laca_metab), colour = 'Species', loadings = TRUE, loadings.colour = "#0072B2", size = 2.5,
                        loadings.label = TRUE, loadings.label.size = 3, loadings.label.colour = "#0072B2", label = FALSE, shape = "volunteer")+
  scale_shape_manual(values=c(0,1, 2, 3, 4, 5, 6, 15, 16, 17, 12, 14, 20, 9))

plot(p7)

# Other form of adding labels. Not pretty but labels are easier to read.
p7 + # Show dots
  geom_text(
    label= make.names(na.omit(laca_metab)$`Strain number`), 
    check_overlap = TRUE,
  )
