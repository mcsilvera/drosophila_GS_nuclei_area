library(readxl)
library(ggplot2)
library(ggsignif)

#Set working directory
data <- read_excel("Nuclei_dimensions.xlsx")

#Filter the data for each genotype and individual
oregonr_1 <- subset(data, Genotype == "OregonR" & Individual == 1)$Nuclei_Area
oregonr_2 <- subset(data, Genotype == "OregonR" & Individual == 2)$Nuclei_Area
oregonr_3 <- subset(data, Genotype == "OregonR" & Individual == 3)$Nuclei_Area

prtp_1 <- subset(data, Genotype == "prtp" & Individual == 1)$Nuclei_Area
prtp_2 <- subset(data, Genotype == "prtp" & Individual == 2)$Nuclei_Area
prtp_3 <- subset(data, Genotype == "prtp" & Individual == 3)$Nuclei_Area


oregonr <- subset(data, Genotype == "OregonR")$Nuclei_Area
prtp <- subset(data, Genotype == "prtp")$Nuclei_Area

#Statistics
shapiro.test(oregonr)
shapiro.test(prtp)

#OregonR and prtp dataframe
data_combined <- data.frame(
  Genotype = c(rep("OregonR", length(oregonr)), rep("prtp", length(prtp))),
  Nuclei_Area = c(oregonr, prtp)
)

#OregonR and prtp boxplot
ggplot(data_combined, aes(x = Genotype, y = Nuclei_Area, fill = Genotype)) +
  geom_boxplot(fill = "white", show.legend = FALSE, size = 1) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.4, size = 4, show.legend = FALSE) + 
  labs(x = "", y = expression("Nuclei area (µm"^2*")")) + # Using expression for superscript
  scale_y_continuous(limits = c(0, 550), breaks = seq(0, 500, by = 100)) + 
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
              y_position = c(500,550))

