library(tidyverse)
library(janitor)
# Read data ---------------------------------------------------------------####
data_to_plot<-read_delim("data/input_bubblePlot_AbsoluteFinal_2.tsv", 
                         delim = "\t") %>%
  clean_names()
# Transform to long -------------------------------------------------------####
data_long<-data_to_plot %>%
  pivot_longer(!c(groups, colors), names_to = "Genes", values_to = "count") %>%
  filter(count != "0")
# Order ------------------------------------------------------------------####
order_taxa<-unique(data_long$groups)
order_genes<-colnames(data_to_plot[,3:16])
# Color ------------------------------------------------------------------####
unique(data_long$colors)
color_pallet<-c("Escherichia" = "#89CBED", "Vibrio" = "#45AA98", 
                "Staphylococcus" = "#506297")
# create the plot --------------------------------------------------------####
ggplot(data_long,
       aes(x=factor(groups, 
                    levels = order_taxa),
           y= factor(Genes, 
                     levels = rev(order_genes)),
           size= count,
           color=colors)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(1,5)) +
  scale_color_manual(values = color_pallet) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size=6, 
                                   angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        axis.text.y = element_text(size=8)) +
  xlab("Taxa") + 
  ylab("Genes")
