
library(dplyr)
library(kableExtra)

d <- readRDS(file = "F:/Fust011/KOF/kof-datamodels-xlsx/kof/ggu/2024_10_21/ggu.rds", refhook =  NULL)


crea_col <- function(d, x, etichette, b, year = 2024, periodo.x = c("Q1", "Q2", "Q3", "Q4")){
  
  tmp <- paste0(x, b)
  
  tmp <- d |>
    dplyr::filter(anno >= year,
           periodo %in% periodo.x,
           series %in% tmp
    ) |>
    dplyr::select(date, value)
  
  colnames(tmp) <- c("Inchiesta", etichette)
  
  tmp <- as.data.frame(tmp)
  
  tmp
  
}

# Tab. 1
# Situazione attuale degli affari in Ticino, alberghi  e ristoranti, da gennaio 2021

AFF0 <- "ch.kof.ggu.ng08.fx.rgn.ti.q_ql_ass_bs."
etichette <- c("Buoni", "Mediocri", "Saldo")

pos <- crea_col(d, AFF0, etichette[1], "share_pos")
neg <- crea_col(d, AFF0, etichette[2], "share_neg")
bal <- crea_col(d, AFF0, etichette[3], "balance")


tmp <- data.frame(
  Anno = c("2024", "2024", "2024", "2024"),
  Mese = c("Gennaio", "Aprile", "Luglio", "Ottobre*"),
  pos[2],
  neg[2],
  bal[2]
  )

tab1 <- tmp %>%
  mutate_if(is.numeric, format, digits = 0L, nsmall = 1L) %>%
  kable(escape = F, align = "llccc",
        caption = "Situazione degli affari (in p.p.), in Ticino") %>%
  kable_styling(full_width = TRUE, font_size = 11) %>%
  column_spec(2, bold = T) %>%
  column_spec(5, background = "gray", color = "white")


# Situazione attuale degli affari in Ticino, alberghi, in ottobre dal 2018

AFF.H <- "ch.kof.ggu.ng08.fx.rgn.ti.sector_2d.55.q_ql_ass_bs."

pos <- crea_col(d, AFF.H, etichette[1], "share_pos", 2019, periodo = "Q3")
neg <- crea_col(d, AFF.H, etichette[2], "share_neg", 2019, periodo = "Q3")
bal <- crea_col(d, AFF.H, etichette[3], "balance", 2019, periodo = "Q3")

tmp <- data.frame(
  Anno = c("2019", "2020", "2021", "2022", "2023", "2024"),
  Mese = c(rep("Luglio", 6)),
  pos[2],
  neg[2],
  bal[2]
  )

tab2 <- kable(tmp, escape = F, caption = "T.2 Alberghi, in Ticino, dal 2019") %>%
  kable_styling(full_width=FALSE, font_size = 11) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, color = "gray", italic = T)


# Situazione attuale degli affari in Ticino, alberghi, in ottobre dal 2018

AFF.R <- "ch.kof.ggu.ng08.fx.rgn.ti.sector_2d.56.q_ql_ass_bs."

pos <- crea_col(d, AFF.R, etichette[1], "share_pos", 2019, periodo = "Q3")
neg <- crea_col(d, AFF.R, etichette[2], "share_neg", 2019, periodo = "Q3")
bal <- crea_col(d, AFF.R, etichette[3], "balance", 2019, periodo = "Q3")

tmp <- data.frame(
  Anno = c("2019", "2020", "2021", "2022", "2023", "2024"),
  Mese = c(rep("Luglio", 6)),
  pos[2],
  neg[2],
  bal[2]
  )

tab3 <- kable(tmp, escape = F, caption = "T.3  Ristoranti, in Ticino, dal 2019") %>%
  kable_styling(full_width=FALSE, font_size = 11) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, color = "gray", italic = T)





# Salvo a mano, con il comando "Export, dimensioni: width: 400, height: 250)

# save_kable(tmp, "immagini/prova.png", zoom = 1.5)


