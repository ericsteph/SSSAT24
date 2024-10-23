
library(dplyr)
library(kableExtra)

d <- readRDS(file = "F:/Fust011/KOF/kof-datamodels-xlsx/kof/ggu/2024_10_21/ggu.rds", refhook =  NULL)

crea_col <- function(x, etichette, b, year = 2024, periodo.x = c("Q1", "Q2", "Q3", "Q4")){
  
  tmp <- paste0(x, b)
  
  tmp <- d %>%
    filter(anno >= year,
           periodo %in% periodo.x,
           series %in% tmp
    ) %>%
    select(date, value)
  
  colnames(tmp) <- c("Inchiesta", etichette)
  
  tmp <- as.data.frame(tmp)
  
  tmp
  
}

# Tab. 1
# Variazione della Cifra d'affari in Ticino, alberghi  e ristoranti, da gennaio 2021

CA0 <- "ch.kof.ggu.ng08.fx.rgn.ti.q_ql_chg_turnover_pqpyq."
         
etichette <- c("Superiore", "Inferiore", "Saldo")

pos <- crea_col(CA0, etichette[1], "share_pos")
neg <- crea_col(CA0, etichette[2], "share_neg")
bal <- crea_col(CA0, etichette[3], "balance")

CA <- "ch.kof.ggu.ng08.fx.rgn.ti.q_qn_chg_pc_turnover_pqpyq.value"

etichette <- c("Var media")

val <- crea_col(CA, etichette[1], "")


tmp <- data.frame(
  Anno = c("2024", "2024", "2024", "2024"),
  Mese = c("Gennaio", "Aprile", "Luglio", "Ottobre*"),
  pos[2],
  neg[2],
  bal[2],
  val[2]
)


tab1 <- tmp %>%
  mutate_if(is.numeric, format, digits = 0L, nsmall = 1L) %>%
  kable(escape = F, align = "llcccc",
        col.names = c("Anno", "Mese", "Superiore", "Inferiore", "Saldo", "Var. media (in %)"),
        caption = "Cifra d'affari negli alberghi e ristoranti (in p.p. e in %), in Ticino") %>%
  kable_styling(full_width = TRUE, font_size = 11) %>%
  column_spec(2, bold = T) %>%
  column_spec(5, background = "gray", color = "white") %>%
  column_spec(6, color = "red", bold = T)

#################################################################################################################
#
# T.2
# Variazione della Cifra d'affari in Ticino, alberghi, da gennaio 2021

CA0.H <- "ch.kof.ggu.ng08.fx.rgn.ti.sector_2d.55.q_ql_chg_turnover_pqpyq."
etichette <- c("Superiore", "Inferiore", "Saldo")

pos <- crea_col(CA0.H, etichette[1], "share_pos", 2021, periodo = "Q3")
neg <- crea_col(CA0.H, etichette[2], "share_neg", 2021, periodo = "Q3")
bal <- crea_col(CA0.H, etichette[3], "balance", 2021, periodo = "Q3")

CA <- "ch.kof.ggu.ng08.fx.rgn.ti.sector_2d.55.q_qn_chg_pc_turnover_pqpyq.value"
etichette <- c("Var_Media")
val <- crea_col(CA, etichette[1], "", 2021, periodo = "Q3")

tmp <- data.frame(
  Anno = c("2021":"2024"),
  # Mese = c(rep("Luglio", 4)),
  pos[2],
  neg[2],
  bal[2],
  val[2])

tab2 <- tmp %>%
  mutate_if(is.numeric, format, digits = 0L, nsmall = 1L) %>%
  kable(escape = F, align = "lcccc",
        caption = "T.2 Alberghi, in Ticino, a luglio, dal 2021") %>%
  kable_styling(full_width=FALSE, font_size = 11) %>%
  column_spec(1, bold = T) %>%
  # column_spec(2, color = "gray", italic = T) %>%
  column_spec(5, color = "red")


#################################################################################################################
#
# T.3
# Variazione della Cifra d'affari in Ticino, ristoranti, da gennaio 2021

CA0.R <- "ch.kof.ggu.ng08.fx.rgn.ti.sector_2d.56.q_ql_chg_turnover_pqpyq."
etichette <- c("Superiore", "Inferiore", "Saldo")

pos <- crea_col(CA0.R, etichette[1], "share_pos", 2021, periodo = "Q3")
neg <- crea_col(CA0.R, etichette[2], "share_neg", 2021, periodo = "Q3")
bal <- crea_col(CA0.R, etichette[3], "balance", 2021, periodo = "Q3")

CA <- "ch.kof.ggu.ng08.fx.rgn.ti.sector_2d.56.q_qn_chg_pc_turnover_pqpyq.value"
etichette <- c("Var_Media")
val <- crea_col(CA, etichette[1], "", 2021, periodo = "Q3")

tmp <- data.frame(
  Anno = c("2021":"2024"),
  # Mese = c(rep("Luglio", 4)),
  pos[2],
  neg[2],
  bal[2],
  val[2])


tab3 <- tmp %>%
  mutate_if(is.numeric, format, digits = 0L, nsmall = 1L) %>%
  kable(escape = F, align = "lcccc",
        caption = "T.3 Ristoranti, in Ticino, a luglio, dal 2021") %>%
  kable_styling(full_width=FALSE, font_size = 11) %>%
  column_spec(1, bold = T) %>%
  # column_spec(2, color = "gray", italic = T) %>%
  column_spec(5, color = "red")

