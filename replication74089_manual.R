# Identification of hub genes and pathways associated with oxidative stress of cartilage in osteonecrosis of femoral head using bioinformatics analysis
# W Shi, X Zhang, C Xu, R Pang, Z Fan, X Wan, Z Jiang, H Li, Z Li, H Zhang
# Cartilage, 2022•journals.sagepub.com

# If fail to download automatically,then download manually and read it.
rm(list = ls())
library(GEOquery);library(limma);library(umap);library(dplyr);library(purrr)
# 第一部分，数据处理

# 两个冒号的函数是供你调用的，三个冒号的函数是供两个冒号的函数调用的。
setwd('D:/Bioinformatics/GSE74089')
gset <- GEOquery:::parseGSEMatrix('GSE74089_series_matrix.txt.gz')$eset
class(gset)
# 基因注释信息
# featureData returns an object containing information on both variable values and variable meta-data. 
# fvarLabels returns a character vector of measured variable names. 
# fData returns a data frame with features as rows, variables as columns. 
# fvarMetadata returns a data frame with variable names as rows, description tags (e.g., unit of measurement) as columns.

featureData(gset)
fvarLabels(gset)
fData(gset)
fvarMetadata(gset)
fData(gset)

# 第二部分：差异基因分析
# phenotype data
sampleInfo <- pData(gset) %>% rename(group = as.factor('diagnosis:ch1')) 
table(sampleInfo$group)

design <- model.matrix( ~ 0 + sampleInfo$group)
colnames(design) <- c('control','patient')

# fit linear model
fit <- lmFit(gset,design)
# define the contrast:
contrasts <- makeContrasts(contrasts = 'patient - control',
                           levels = design)

fit2 <- contrasts.fit(fit,contrasts) %>% eBayes()

(tT <- fit2 %>% topTable(adjust.method = 'fdr',sort.by = 'p',number = Inf))

table(decideTests(fit2,adjust.method = 'fdr',lfc = 1.5,p.value = .05))

4218+105;1685+3332
# -1     0     1 
# 1685 29166  3332 
# 原文的下调基因为1130,上调基因为2496.

# 火山图
library(ggplot2)
ggplot(data = mydat, 
       aes(x = log2FoldChange, y = -log10(padj), 
                            colour = Condition)) + 
  geom_point(alpha = 0.8, size = 1)  +  
  xlab("log2 fold change") + 
  ylab("-log10 padj") + 
  geom_hline(yintercept = -log10(0.05),linetype = 4) + 
  geom_vline(xintercept = c(-1.5,1.5),linetype = 4) + 
  scale_color_manual(values=c('Up'='red','Down'='green','Normal'='gray'))

# 出版用的火山图
# https://bioconductor.org/packages/release/bioc/vignettes/EnhancedVolcano/inst/doc/EnhancedVolcano.html

# 第三部分：获取感兴趣通路相关的基因集
# http://geneontology.org/
# keywords:Oxidative stress - gene product -  Genes and gene products - 右键另存为
url <- "C:\\Users\\Administrator\\Downloads\\select.txt"
stress <- read.table(url,sep = '\t') %>% rename(GENE_SYMBOL = V2) %>% 
  distinct(GENE_SYMBOL, .keep_all = TRUE)

diff_gene <- tT %>% filter(abs(logFC) < 1.5 | adj.P.Val > .05) %>% 
  distinct(GENE_SYMBOL, .keep_all = TRUE)

# 韦恩图
library (VennDiagram) 
#指定统计的分组列，并设置作图颜色、字体样式等
venn_list <- list(group1 = stress$GENE_SYMBOL, group2 = diff_gene$GENE_SYMBOL)

venn.diagram(venn_list, filename = 'venn2.png', imagetype = 'png', 
             fill = c('red', 'blue'), alpha = 0.50, cat.col = rep('black', 2), 
             col = 'black', cex = 1.5, fontfamily = 'serif', 
             cat.cex = 1.5, cat.fontfamily = 'serif')
# 交集是3485个基因。


# GSE74089, a total of 20,844 genes were found, including 3,626 DEGs.
# A total of 488 genes related to oxidative stress were selected from the GO database,
# of which 440 could be matched in the expression profile of GSE74089. 
# The Venn diagram showed that 88 genes crossed between DEGs and oxidative stress-related genes, including 
# 71 upregulated genes and 17 downregulated genes. 
# The volcanic map of GSE74089 and the heat map of 88 OS-DEGs are 
# shown in Figure 2. The Venn diagram is shown in Figure 3.


# 功能富集分析对于解释转录组学数据至关重要。
# 转录组鉴定了差异表达基因后，通常会进行GO或KEGG富集分析，
# 识别这些差异基因的功能或参与调控的通路，
# 来说明关键基因表达上调/或下调后可能会导致哪功能或通路被激活/或抑制，
# 进而与表型进行联系。
# 基因名称可以是ensembl、entrze或者symbol等类型
# ensembl：en开头；entrze:数字；symbol：TGDS
# 4. GSEA分析   

# 5.GO富集分析    
# 加载背景基因集后，读取基因列表文件后，
# 使用clusterProfiler的内部函数enrichGO()即可进行有参GO富集分析。

library(curl);library(Rcpp);library(reticulate)
BiocManager::install('clusterProfiler')
BiocManager::install('org.Hs.eg.db',force = TRUE)
AnnotationDbi
#manually download package AnnotationDbi and install
# downoad curl from https://curl.se/windows/ and unzip
# add curl.exe to enviornment variable
BiocManager::install('GO.db',force = TRUE)
# 1 下载包：
# 
# http://mirrors.ustc.edu.cn/bioc/3.12/data/annotation/src/contrib/org.Hs.eg.db_3.12.0.tar.gz
# 
# 2 安装包：
# 
# packages.install("/home/bioysy/tmp5/org.Hs.eg.db_3.12.0.tar.gz")

library(AnnotationDbi);library(GO.db);library(clusterProfiler);library(org.Hs.eg.db)

#基因id转换，kegg和go富集用的ID类型是ENTREZID）
gene_list <- intersect(stress$GENE_SYMBOL, diff_gene$GENE_SYMBOL) %>% 
  bitr(fromType="SYMBOL", toType= "ENTREZID", OrgDb="org.Hs.eg.db")

ego <- enrichGO(gene = gene_list$SYMBOL,
                OrgDb = org.Hs.eg.db,
                keyType = "SYMBOL",
                ont = "BP",  # 可以是"BP"（Biological Process）、"MF"（Molecular Function）或"CC"（Cellular Component）
                pAdjustMethod = "BH",
                qvalueCutoff = 0.05,
                readable = TRUE)
ego
# GO富集分析结果可视化
library(fgsea);library(ggplot2)
#clusterProfiler 包里的一些默认作图方法，例如
barplot(enrich.go)  #富集柱形图
dotplot(enrich.go)  #富集气泡图
cnetplot(enrich.go) #网络图展示富集功能和基因的包含关系
emapplot(enrich.go) #网络图展示各富集功能之间共有基因关系
heatplot(enrich.go) #热图展示富集功能和基因的包含关系

# 6.KEGG富集分析    
kego <- enrichKEGG(gene = gene_list$ENTREZID,
                   organism = 'hsa',  # 人类KEGG代码
                   pAdjustMethod = "BH",
                   qvalueCutoff = 0.05)



# KEGG富集分析结果可视化
# plotEnrichment(kego, showCategorySize = TRUE)
dotplot(kego) + ggtitle("KEGG Enrichment Analysis")
# 7.PPI分析     

# 8.转录因子预测   

