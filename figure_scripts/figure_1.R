library(ggplot2)

setwd('../data')

load('jbc_vs_jbc_135e.RData')

report$volcano_fold <- ifelse(report$tstat >= 0, log2(report$fold), -log2(report$fold))

svg('figure_1a.svg', width=21, height=9)
ggplot(data=report, aes(x=volcano_fold, y=-log10(pvalue), label=round(mzmed, digits=4))) +
  geom_point(data=subset(report, volcano_fold > 1.5 & pvalue <= 0.05), colour='orange', size=3) +
  geom_point(data=subset(report, volcano_fold < -1.5 & pvalue <= 0.05), colour='blue', size=3) +
  geom_point(data=subset(report, pvalue > 0.05), size=3) +
  geom_point(data=subset(report, volcano_fold <= 1.5 & volcano_fold >= -1.5 & pvalue <= 0.05), size=3) +
  geom_vline(xintercept=-1.5) +
  geom_vline(xintercept=1.5) +
  geom_hline(yintercept=-log10(0.05)) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour='black', size=1.5),
        text=element_text(size=32)) +
  labs(title='Penicillium sp. JBC Monoculture vs Penicillium sp. JBC/D. catenulata 135E Co-Culture',
       x='log2(fold change)',
       y='-log10(p-value)')
dev.off()

load('cca_vs_jbc.RData')

report$volcano_fold <- ifelse(report$tstat >= 0, log2(report$fold), -log2(report$fold))

svg('figure_1b.svg', width=21, height=9)
ggplot(data=report, aes(x=volcano_fold, y=-log10(pvalue), label=round(mzmed, digits=4))) +
  geom_point(data=subset(report, volcano_fold > 1.5 & pvalue <= 0.05), colour='orange', size=3) +
  geom_point(data=subset(report, volcano_fold < -1.5 & pvalue <= 0.05), colour='blue', size=3) +
  geom_point(data=subset(report, pvalue > 0.05), size=3) +
  geom_point(data=subset(report, volcano_fold <= 1.5 & volcano_fold >= -1.5 & pvalue <= 0.05), size=3) +
  geom_vline(xintercept=-1.5) +
  geom_vline(xintercept=1.5) +
  geom_hline(yintercept=-log10(0.05)) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour='black', size=1.5),
        text=element_text(size=32)) +
  labs(title='Cheese Curd Agar vs Penicillium sp. JBC Monoculture',
       x='log2(fold change)',
       y='-log10(p-value)')
dev.off()
