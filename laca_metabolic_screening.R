library(ggplot2)
library(ggfortify)

# Read Data
laca_metab <- readxl::read_excel("C:/Users/Marcelo/Downloads/Metabolic screening LaCa collection - 230823.xlsx", sheet = "Tabelle1")

laca_metab <- readxl::read_excel("C:/Users/Marcelo/OneDrive - UT Cloud/Metabolic screening LaCa collection - 230823.xlsx", sheet = "Tabelle2")

# Remove the rows with missing values.
laca_metab2 <- na.omit(laca_metab[,5:16])

# We have to put row.names with 
row.names(laca_metab2) <- make.names(na.omit(laca_metab)$`Strain number`)

# Calculate PCA. Important to scale the variables ("scale. = TRUE")
pca_res <- prcomp(laca_metab2, scale. = TRUE)

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
