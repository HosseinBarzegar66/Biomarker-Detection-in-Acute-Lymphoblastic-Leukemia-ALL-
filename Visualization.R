#### Visualization ####

library("ggplot2")


plotMA(res, ylim=c(-6,10),
       main = "MAplot-0.05",
       xlab= "basemean", ylab="LogFC", 
       alpha = 0.05,
       colNonSig = "grey", colSig = "red", colLine = "black")

plotMA(res, ylim=c(-6,10),
       main = "MAplot-0.01",
       xlab= "basemean", ylab="LogFC", 
       alpha = 0.01, 
       colNonSig = "grey", colSig = "red", colLine = "black")

# Enhanced Volcano plot
library("ggrepel")
library("EnhancedVolcano")
EnhancedVolcano(res,
                lab = rownames(res),
                x = 'log2FoldChange',
                y = 'padj',
                selectLab = c('KYNU','PBX1'),
                title = "Volcano",
                pCutoff = 10e-3,
                FCcutoff = 1.5,
                col=c('black', 'green', 'blue', 'red'),
                legendLabels=c('Not sig.','Log (base 2) FC','p-value',
                               'p-value & Log (base 2) FC'),
                labSize = 3.0,
                labCol = 'black',
                labFace = 'bold',
                boxedLabels = TRUE,
                #drawConnectors = TRUE,
                ylim = c(0,15),
                xlim = c(-6, 6),
                colAlpha = 0.5,
                legendIconSize = 4.0)


# MA plot2

library("ggplot2")
library("ggrepel")
resdata=data.frame(res)
head(resdata)
gene=rownames(resdata)
resdata <- cbind(gene, data.frame(res, row.names=NULL))
head(resdata)
resdata$'log2baseMean' <- log2(resdata[,2])
ggplot(resdata, aes(x = log2baseMean, y = log2FoldChange)) +
  geom_point(aes(colour = padj))+
  scale_color_gradient(low="red", high="green")+
  geom_text_repel(aes(label=ifelse(log2FoldChange>4,as.character(gene),'')),hjust=0.5,vjust=0.5,size=3)+
  geom_text_repel(aes(label=ifelse(log2FoldChange<4*(-1),as.character(gene),'')),hjust=0,vjust=0,size=3)
