### DESeq2 ANALYSIS ####
#Packages Installation
if (!requireNamespace("BiocManager", quietly = TRUE))
install.packages("BiocManager")

install.packages("tidyverse")
install.packages("ashr")
install.packages("devtools")
devtools::install_github("stephenturner/annotables",force=TRUE)
BiocManager::install("DESeq2")
BiocManager::install("gplots")
BiocManager::install("EnhancedVolcano")
BiocManager::install("ggrepel")
BiocManager::install("pheatmap")
BiocManager::install("RColorBrewer")

################### DIFFERNTIAL EXPRESSION ANALYSIS ####################

library("tidyverse")
# tidyverse is an opinionated collection of R packages designed for data science analysis
# Set up for DESeq2 analysis
# Load DESeq2 library
library("DESeq2")

# Set the working directory
directory <- "G:/My_Project"
setwd(directory)
files <- list.files()
sampleNames <- sub(".counts", "", files)
sampleGroup<- factor(c(rep("Cancer",9),rep("Normal",10),rep("Cancer",4)
                       ,rep("Normal",5),rep("Cancer",1),rep("Cancer",5)
                       ,rep("Normal",3)))

sampletable<- data.frame(sample = sampleNames,
                         files = files,
                         group = sampleGroup)


ddsHTSeq <- DESeqDataSetFromHTSeqCount(sampleTable = sampletable,
                                       directory = directory,
                                       design= ~group)
# DESeq2 object created
class(ddsHTSeq)

ddsHTSeq$group

# Setting the factor levels
treatments <- c("Normal","Cancer")
colData(ddsHTSeq)$group <- factor(colData(ddsHTSeq)$group,
                                  levels = treatments)

ddsHTSeq$group

# Differential Expression Analysis
dds <- DESeq(ddsHTSeq)
dds

res <- results(dds)
head(res)
resultsNames(dds)

# Filter results by adjusted p value
res05 = subset(res, padj<0.05)
head(res05)

# Save data results and normalized reads to a csv file
resdata <- merge(as.data.frame(res05),
                 as.data.frame(counts(dds,normalized =TRUE)),
                 by = 'row.names')

write.csv(resdata,file = "resdata.csv")
