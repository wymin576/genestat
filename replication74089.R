# 1.downloading data
library(GEOquery);library(limma);library(umap);library(dplyr);library(purrr)
gset <- getGEO('GSE74089',GSEMatrix = T,AnnotGPL = T)
gset <- gset[[1]]

# 2.differential expression analysis
sampleInfo <- pData(gset) %>% rename(group = 'diagnosis:ch1') %>% 
  select(geo_accession,group)

design <- model.matrix( ~ 0 + sampleInfo$group)
colnames(design) <- c('Normal','Tumor')

# fit linear model
fit <- lmFit(gset,design)
# define the contrast
contrasts <- makeContrasts(Tumor - Normal,levels = design)

fit2 <- contrasts.fit(fit,contrasts) %>% eBayes()

(tT <- fit2 %>% topTable(adjust.method = 'fdr',sort.by = 'p',number = Inf))
class(tT)
names(tT)



table(decideTests(fit2,adjust.method = 'fdr',lfc = 1.5,p.value = .05))
# -1     0     1 
# 1685 29166  3332 
# 原文的下调基因为1130,上调基因为2496.

# 火山图
volcanoplot(fit2)

#go analysis
# 3.从gene ontology数据库中查找得到“oxidative stress”相关的基因。

stress <- data.table::fread("D:/Bioinformatics/GSE102915/select.txt",
                            header = F)
head(stress)
names(stress) <- c('bioentiry','bioentiry_label','bioentiry_name','synonym')

# 差异基因集和“oxidative stress”相关的基因取交集，画韦恩图。


# 4. GSEA分析   

# 5.GO富集分析    


# 6.KEGG富集分析    

# 7.PPI分析     

# 8.转录因子预测    
 



