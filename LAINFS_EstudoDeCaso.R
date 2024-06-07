###############################################
## ESTUDO DE CASO: X-MEETING 2024 / LAINFS ####
###############################################
# por Jessica Magno e Joao Muzzi


# Exemplo de analise com dados biologicos
# Baixando dados de ACC do TCGA pelo portal XenaBrowser 
# https://xenabrowser.net/
setwd("~/Documentos/LAINFS_2024/Aula_estudocaso")
url = "https://tcga-xena-hub.s3.us-east-1.amazonaws.com/download/TCGA.ACC.sampleMap%2FACC_clinicalMatrix"
download.file(url = url, destfile = "./clinical.txt")

clinical_acc <- read.delim("./clinical.txt")
View(clinical_acc)

# Baixando matriz de expressao
url = "https://tcga-xena-hub.s3.us-east-1.amazonaws.com/download/TCGA.ACC.sampleMap%2FHiSeqV2.gz"
download.file(url = url, destfile = "./gexp.txt")

gexp_acc <- read.delim("./gexp.txt", header = T, dec = ".")
View(gexp_acc)

# Adaptando nomes das amostras em gexp semelhante as amostras nos dados clinicos
samples_gexp <- colnames(gexp_acc)
samples_gexp <- stringr::str_replace_all(samples_gexp,"[.]","-")
colnames(gexp_acc) <- samples_gexp

# Modificando rownames de gexp com nome dos genes 
gexp_acc[,1] #[linha, coluna]
rownames(gexp_acc) <- gexp_acc[,1]
gexp_acc <- gexp_acc[,-1]

# Vamos fazer de maneira semelhante para os dados clinicos
rownames(clinical_acc) <- clinical_acc$sampleID 

# Selecionar dados clinicos somente dos pacientes com dados de expressão
nrow(clinical_acc) #nrow() numero de linhas
ncol(gexp_acc)
clinical_acc <- clinical_acc[clinical_acc$sampleID %in% colnames(gexp_acc),]
#[linha,coluna]

nrow(clinical_acc) #nrow() numero de linhas
ncol(gexp_acc)
identical(nrow(clinical_acc), ncol(gexp_acc))

# Ver se as duas tabelas estao alinhadas
colnames(gexp_acc)
rownames(clinical_acc)
identical(colnames(gexp_acc), rownames(clinical_acc)) #FALSE

# Organizando as duas na mesma ordem de pacientes
gexp_acc <- gexp_acc[,rownames(clinical_acc)]
identical(colnames(gexp_acc), rownames(clinical_acc)) #TRUE


# Analisando alguns dados clinicos
## Status vital do paciente
clinical_acc$vital_status
summary(clinical_acc$vital_status)
clinical_acc$vital_status <- as.factor(clinical_acc$vital_status)
summary(clinical_acc$vital_status)

sum(clinical_acc$vital_status == "LIVING")

## Estagio tumoral
clinical_acc$pathologic_stage # alguns valores faltando

# Substituindo valores faltantes por NA
clinical_acc$pathologic_stage <-
  ifelse(clinical_acc$pathologic_stage == "",
         NA,
         clinical_acc$pathologic_stage)
clinical_acc$pathologic_stage
summary(clinical_acc$pathologic_stage)
clinical_acc$pathologic_stage <- as.factor(clinical_acc$pathologic_stage)
summary(clinical_acc$pathologic_stage)

## Producao excessiva de hormonios
clinical_acc$excess_adrenal_hormone_history_type
clinical_acc$excess_adrenal_hormone_history_type <-
  ifelse(clinical_acc$excess_adrenal_hormone_history_type == "",
         NA,
         clinical_acc$excess_adrenal_hormone_history_type)
clinical_acc$excess_adrenal_hormone_history_type

## Criar coluna com excesso de hormonio
clinical_acc$hormone_excess <- 
  ifelse(clinical_acc$excess_adrenal_hormone_history_type == "None",
         "No",
         "Yes")
clinical_acc$hormone_excess
summary(clinical_acc$hormone_excess)
clinical_acc$hormone_excess <- as.factor(clinical_acc$hormone_excess)
summary(clinical_acc$hormone_excess)

## Barplot com estagio tumoral e status vital

#install.packages("ggplot2")
#install.packages("plotly")
library(ggplot2)
library(plotly)

# Confeccionando um grafico de barras simples


g <- ggplot(clinical_acc[complete.cases(clinical_acc$pathologic_stage),],
            aes(x=pathologic_stage, fill=vital_status))
g <- g + geom_bar()
g
ggplotly(g)

# Adicionando e customizando o grafico de barras
g2 <- ggplot(clinical_acc[complete.cases(clinical_acc$pathologic_stage),],
             aes(x = pathologic_stage, fill = vital_status))
g2 <- g2 + geom_bar(position = "fill") + 
  scale_fill_manual(values = c("red3", "steelblue")) +
  labs(title = "Vital satus and pathologic state in ACC",
       x = " Pathologic State",
       y = "Proportion",
       fill = "Vital Status") +
  theme_classic()
g2
ggplotly(g2)

## Status vital e excesso de hormonio
tab <- table(clinical_acc$vital_status, clinical_acc$hormone_excess)
tab
fisher.test(tab)

# Vamos ver a expressao de alguns genes
# Gene MKI67, proteina KI67 - associado a maior proliferacao
# Gene BUB1 marcador molecular de proliferacao

identical(colnames(gexp_acc), rownames(clinical_acc))
which(rownames(gexp_acc)=="MKI67")
gene_exp_MKI67 <- gexp_acc[6377,]
gene_exp_MKI67 <- as.numeric(gene_exp_MKI67)
summary(gene_exp_MKI67)
clinical_acc$KI67 <- gene_exp_MKI67
summary(clinical_acc$KI67)

g_KI67 <- ggplot(clinical_acc, aes(x = KI67))
g_KI67 <- g_KI67 + geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "KI67 expression in ACC",
       x = "KI67 Expression",
       y = "Frequency") +
  theme_classic() # distribuicao nao normal
g_KI67
ggplotly(g_KI67)

which(rownames(gexp_acc)=="BUB1")
gene_exp_BUB1 <- gexp_acc[13291,]
gene_exp_BUB1 <- as.numeric(gene_exp_BUB1)
clinical_acc$BUB1 <- gene_exp_BUB1

#install.packages("wesanderson")
library(wesanderson)

g_BUB1a <- ggplot(clinical_acc[complete.cases(clinical_acc$pathologic_stage),],
                  aes(x = pathologic_stage, y = BUB1, fill = pathologic_stage))
g_BUB1a <- g_BUB1a + geom_boxplot() + 
  scale_fill_manual(values = wes_palette("Royal1")) + 
  theme_minimal() +
  labs("BUB1 expression in ACC",
       x = "Pathologic Stage",
       y = "BUB1 Expression",
       fill = "Pathologic State")
g_BUB1a
ggplotly(g_BUB1a)

g_B_K <- ggplot(clinical_acc,
                aes(x = BUB1, y = KI67, color = vital_status)) +
  geom_point() + scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_classic() +
  labs(title = " BUB1 Expression vs KI67 Expression in ACC",
       x = "BUB1",
       y = "KI67",
       color = "Vital Status")
g_B_K
ggplotly(g_B_K)

# Exportando o plot
ggsave("KI67plot.png", g_KI67, width = 10, height = 10, units = "in")

# Agrupando os plots em uma unica figura
#install.packages("cowplot")
library(cowplot)
group_BUB1_KI67 <- plot_grid(g_BUB1a, g_KI67, g_B_K ,labels = "AUTO", 
                             nrow = 2,
                             rel_widths = c(2,1,2))

# Exemplo de exportacao da figura em pdf
ggsave2(filename = "BUB1_KI67_ACC.pdf", group_BUB1_KI67,
        width = 10, height = 7, units = "in")

### Muito mais:
### analise de expressao diferencial (DESeq2, limma, etc)
### analise sobrevivencia (survival)
### etc etc


#---------------------------------------------------------------------