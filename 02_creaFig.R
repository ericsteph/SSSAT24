library(ggkof)
library(dm)
library(dmkof)
library(data.table)
library(zoo)


ggu <- readRDS(file = "F:/Fust011/KOF/kof-datamodels-xlsx/kof/ggu/2024_10_21/ggu.rds", refhook =  NULL)

ggkof::dt_trimestri_to_mesi(ggu)

dm_ggu <- dm_build(xlsx = "chiavi_grafici_ggu.xlsx")
lista_figure_ggu <- dmkof_figures(
  dm = dm_ggu, dati = ggu, guides = "collect"
)