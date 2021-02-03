# Hematology and Cytokine Results
library(tidyverse)
library(ggplot2)

cytokines <- read.table("./2015-002_Cytokine_data_final.txt", header=T, sep="\t", check.names = F)
cytokines$`IL-9`[45] <- 10000 # dummy for scales - to be hidden.
cytokines$Treatment <- factor(cytokines$Treatment, levels=c("Control", "Ag10","Ag50"))
cytokines.long <- pivot_longer(cytokines, cols=5:ncol(cytokines), names_to="Cytokine", values_to="concentration")
cytokines.long$Cytokine <- factor(cytokines.long$Cytokine, levels=colnames(cytokines)[5:ncol(cytokines)])

cytokines.up <- cytokines.long %>%
  dplyr::filter(Cytokine %in% c("IL-1a","IL-2","IL-6","IL-9","IL-10","Eotaxin","GM-CSF","IFN-g","TNF-a"))

cytokines.down <- cytokines.long %>%
  dplyr::filter(Cytokine %in% c("IL-4","IL-12 (p40)","MIP-1a","MIP-1b","RANTES"))


pdf(file="cytokines.pdf", width=9, height=5)
ggplot(cytokines.up, aes(x=Ab, y=concentration, color=Treatment)) +
  facet_wrap(~Cytokine, scales = "free", ncol=3) +
  geom_boxplot(position=position_dodge2(padding=0.5), fill="white", outlier.size = 0) +
  geom_point(position=position_jitterdodge(), size=1) +
  scale_colour_brewer(palette = "Dark2",
                      name="Silver concentration",
                      labels = c("0", "1", "5")) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face="bold"),
        aspect.ratio = 1) +
  xlab(element_blank()) +
  ylab("pg/mg protein")

ggplot(cytokines.down, aes(x=Ab, y=concentration, color=Treatment)) +
  facet_wrap(~Cytokine, scales = "free", ncol=2) +
  geom_boxplot(position=position_dodge2(padding=0.5), fill="white", outlier.size = 0) +
  geom_point(position=position_jitterdodge(), size=1) +
  scale_colour_brewer(palette = "Dark2",
                      name="Silver concentration",
                      labels = c("0", "1", "5")) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face="bold"),
        aspect.ratio = 1) +
  xlab(element_blank()) +
  ylab("pg/mg protein")
dev.off()


blood <- read.table("./Haematol                                                            ogySummary.txt", header=T, sep="\t", check.names = F)
blood$Silver <- factor(blood$Silver, levels=c("Control", "Ag10","Ag50"))
blood.long <- pivot_longer(blood, cols=4:ncol(blood), names_to="Marker", values_to="Count")

blood.plot <- blood.long %>%
  dplyr::filter(Marker %in% c("WBC", "LY"))

pdf(file="blood.pdf", width=5, height=5)
ggplot(blood.plot, aes(x=Antibiotics, y=Count, color=Silver)) +
  facet_wrap(~Marker, scales = "free", ncol=2) +
  geom_boxplot(position=position_dodge2(padding=0.5), fill="white", outlier.size = 0, outlier.colour = "white") +
  geom_point(position=position_jitterdodge(), size=1) +
  scale_colour_brewer(palette = "Dark2",
                      name="Silver concentration",
                      labels = c("0", "1", "5")) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face="bold"),
        aspect.ratio = 1) +
  xlab(element_blank()) +
  ylab("Count")
dev.off()