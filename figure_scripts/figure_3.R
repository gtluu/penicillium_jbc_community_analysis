library(ggplot2)
library(RColorBrewer)

setwd('../data')

data <- read.csv('sclerotigenin_auc_time_course.csv')
colnames(data)[1] <- 'Culture'

data$Culture <- factor(data$Culture, levels=c('Solvent Blank',
                                              'Media Blank',
                                              'D. catenulata 135E',
                                              'Brevibacterium sp. JB5',
                                              'Penicillium sp. JBC',
                                              'Penicillium sp. JBC + D. catenulata 135E',
                                              'Penicillium sp. JBC + Brevibacterium sp. JB5'))

svg('figure_3.svg', width=21, height=9)
ggplot(data=data, aes(x=Day, y=AUC, color=Culture)) +
  geom_line(size=1.5) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour='black', size=1.5),
        text=element_text(size=32)) +
  labs(title='Relative Abundance of Sclerotigenin Over Time',
       x='Day',
       y='AUC') +
  scale_color_brewer(palette='Dark2')
dev.off()
