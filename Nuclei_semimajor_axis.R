library(readxl)
library(ggplot2)
library(ggsignif)

#Set working directory
data <- read_excel("Nuclei_dimensions.xlsx")

#Filter the data for each genotype
oregonr <- subset(data, Genotype == "OregonR")$Ellipse_Major
prtp <- subset(data, Genotype == "prtp")$Ellipse_Major

#Statistics
shapiro.test(oregonr)
shapiro.test(prtp)

#OregonR and prtp dataframe
data_combined <- data.frame(
  Genotype = c(rep("OregonR", length(oregonr)), rep("prtp", length(prtp))),
  Ellipse_Major = c(oregonr, prtp)
)

#OregonR and prtp boxplot
ggplot(data_combined, aes(x = Genotype, y = Ellipse_Major, fill = Genotype)) +
  geom_boxplot(fill = "white", show.legend = FALSE, size = 1) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.4, size = 4, show.legend = FALSE) + 
  labs(x = "", y = expression("Nuclei semimajor axis (µm)")) + 
  scale_y_continuous(limits = c(10, 30), breaks = seq(0, 30, by = 5)) + 
  scale_x_discrete(labels = c("OregonR" = expression(italic("Oregon-R")), 
                              "prtp" = expression(italic("prtp")^italic("Δ1")))) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20)) +
  geom_signif(comparisons = list(c("OregonR", "prtp")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              color = "black",
              size = 1.5,
              textsize = 12,
              y_position = c(28,30))








