# ==========================================
# scripts/04_compare_policies.R
# Comparador de políticas: Baseline vs Recolección+ vs Vida Útil+ vs Eficiencia+
# - Lee resultados deterministas (series y resumen)
# - Lee resultados MC (bandas y resumen)
# - Genera tablas y figuras de comparación
# Requiere: que previamente hayas corrido los scripts 01/02/03 de cada política.
# ==========================================

source("scripts/01_run_rec_plus.R")
source("scripts/02_kpis_det_rec.R")
source("scripts/03_mc_rec_vs_base.R")

source("scripts/01_run_life_plus.R")
source("scripts/02_kpis_det_life.R")
source("scripts/03_mc_life_vs_base.R")

source("scripts/01_run_eff_plus.R")
source("scripts/02_kpis_det_eff.R")
source("scripts/03_mc_eff_vs_base.R")


dir.create("resultsEC", showWarnings = FALSE)
dir.create("figsEC", showWarnings = FALSE)

read_or_null <- function(path) if (file.exists(path)) read.csv(path) else NULL

# -------------------------------
# 1) Determinista — Series KPI
# -------------------------------
kpi_base_rec <- read_or_null("resultsEC/baseline_kpis_series_rec.csv")
kpi_rec      <- read_or_null("resultsEC/esc_rec_kpis_series.csv")

kpi_base_life <- read_or_null("resultsEC/baseline_kpis_series_life.csv")
kpi_life      <- read_or_null("resultsEC/esc_life_kpis_series.csv")

kpi_base_eff <- read_or_null("resultsEC/baseline_kpis_series_eff.csv")
kpi_eff      <- read_or_null("resultsEC/esc_eff_kpis_series.csv")

# Toma baseline de cualquiera que exista
kpi_base <- kpi_base_rec %||% kpi_base_life %||% kpi_base_eff
if (is.null(kpi_base)) stop("No encuentro series deterministas de baseline (kpi_base_*).")

# Arma lista de escenarios disponibles
det_list <- list(
  baseline     = kpi_base,
  "Recolección+" = kpi_rec,
  "Vida Útil+"   = kpi_life,
  "Eficiencia+"  = kpi_eff
)
det_list <- det_list[!vapply(det_list, is.null, TRUE)]

# -------------------------------
# 2) Determinista — Resumen KPI
# -------------------------------
sum_det_rec  <- read_or_null("resultsEC/kpis_resumen_det_rec.csv")
sum_det_life <- read_or_null("resultsEC/kpis_resumen_det_life.csv")
sum_det_eff  <- read_or_null("resultsEC/kpis_resumen_det_eff.csv")

sum_det_all <- do.call(
  rbind,
  Filter(Negate(is.null), list(sum_det_rec, sum_det_life, sum_det_eff))
)
if (is.null(sum_det_all) || nrow(sum_det_all) == 0) {
  warning("No hay resúmenes deterministas (sum_det_*). Saltando esta tabla.")
} else {
  write.csv(sum_det_all, "resultsEC/kpis_resumen_det_all.csv", row.names = FALSE)
}

# -------------------------------
# 3) Monte Carlo — Bandas
# -------------------------------
bands_base <- read_or_null("resultsEC/baseline_kpis_bands_mc.csv")
bands_rec  <- read_or_null("resultsEC/esc_rec_kpis_bands_mc.csv")
bands_life <- read_or_null("resultsEC/esc_life_kpis_bands_mc.csv")
bands_eff  <- read_or_null("resultsEC/esc_eff_kpis_bands_mc.csv")

bands_list <- list(
  baseline      = bands_base,
  "Recolección+" = bands_rec,
  "Vida Útil+"   = bands_life,
  "Eficiencia+"  = bands_eff
)
bands_list <- bands_list[!vapply(bands_list, is.null, TRUE)]

# -------------------------------
# 4) Monte Carlo — Resumen
# -------------------------------
sum_mc_rec  <- read_or_null("resultsEC/kpis_resumen_mc_rec.csv")
sum_mc_life <- read_or_null("resultsEC/kpis_resumen_mc_life.csv")
sum_mc_eff  <- read_or_null("resultsEC/kpis_resumen_mc_eff.csv")

sum_mc_all <- do.call(
  rbind,
  lapply(Filter(Negate(is.null), list(sum_mc_rec, sum_mc_life, sum_mc_eff)), function(df) df)
)
if (!is.null(sum_mc_all) && nrow(sum_mc_all) > 0) {
  # Quita duplicados de baseline si aparecen
  sum_mc_all <- sum_mc_all[!duplicated(sum_mc_all), ]
  write.csv(sum_mc_all, "resultsEC/kpis_resumen_mc_all.csv", row.names = FALSE)
} else {
  warning("No hay resúmenes MC (sum_mc_*).")
}

# -------------------------------
# 5) Gráficos deterministas — C y W
# -------------------------------
colors <- c(
  baseline      = "#1f77b4",
  "Recolección+" = "#2ca02c",
  "Vida Útil+"   = "#ff7f0e",
  "Eficiencia+"  = "#9467bd"
)
linet <- c(
  baseline      = 1,
  "Recolección+" = 2,
  "Vida Útil+"   = 3,
  "Eficiencia+"  = 4
)

plot_det_two <- function(det_list, outfile, ylabC="C(t)", ylabW="W(t)", mainC="", mainW="", zoom=NULL) {
  png(outfile, width=1400, height=900)
  par(mfrow=c(2,1), mar=c(4,4,3,1))
  # Conjunto de tiempos (usa el de baseline)
  time <- det_list[[1]]$time
  idx <- if (is.null(zoom)) rep(TRUE, length(time)) else (time >= zoom[1] & time <= zoom[2])

  # Panel C
  ylC <- range(unlist(lapply(det_list, function(df) df$C[idx])), na.rm=TRUE)
  plot(time[idx], det_list[[1]]$C[idx], type="n", xlab="Tiempo", ylab=ylabC,
       main=mainC, ylim=ylC)
  for (nm in names(det_list)) {
    lines(time[idx], det_list[[nm]]$C[idx], lwd=2, col=colors[nm], lty=linet[nm])
  }
  legend("bottomright", legend=names(det_list), col=colors[names(det_list)],
         lty=linet[names(det_list)], lwd=2, bty="n")

  # Panel W
  ylW <- range(unlist(lapply(det_list, function(df) df$W[idx])), na.rm=TRUE)
  plot(time[idx], det_list[[1]]$W[idx], type="n", xlab="Tiempo", ylab=ylabW,
       main=mainW, ylim=ylW)
  for (nm in names(det_list)) {
    lines(time[idx], det_list[[nm]]$W[idx], lwd=2, col=colors[nm], lty=linet[nm])
  }
  legend("topright", legend=names(det_list), col=colors[names(det_list)],
         lty=linet[names(det_list)], lwd=2, bty="n")
  dev.off()
}

# Full horizon
plot_det_two(
  det_list,
  "figsEC/compare_det_CW_full.png",
  mainC="Circularidad — Comparación determinista (todo el horizonte)",
  mainW="Disposición — Comparación determinista (todo el horizonte)"
)

# Zoom (ajusta si tu rampa está en otra zona)
plot_det_two(
  det_list,
  "figsEC/compare_det_CW_zoom.png",
  mainC="Circularidad — Comparación determinista (zoom)",
  mainW="Disposición — Comparación determinista (zoom)",
  zoom=c(24, 84)
)

# -------------------------------
# 6) Gráficos MC — Bandas y medianas
#    (superpone escenarios con semi-transparencia)
# -------------------------------
alpha <- function(hex, a=0.25) {
  rgb_val <- col2rgb(hex)/255
  rgb(rgb_val[1], rgb_val[2], rgb_val[3], alpha=a)
}

plot_mc_bands_overlay <- function(bands_list, outfile, kpi=c("C","W"),
                                  main_title="Bandas Monte Carlo (q05–q95) y medianas") {
  if (length(bands_list) == 0) {
    warning("No hay bandas MC para graficar.")
    return(invisible(NULL))
  }
  # usa el time de la primera
  time <- bands_list[[1]]$time
  idx <- time >= 24 & time <= 84  # zoom donde suelen notarse

  png(outfile, width=1400, height=1000)
  par(mfrow=c(length(kpi),1), mar=c(4,4,3,1))
  for (kk in kpi) {
    # Rango Y
    all_q <- unlist(lapply(bands_list, function(bd) {
      c(bd[[paste0(kk,"_q05")]][idx], bd[[paste0(kk,"_q95")]][idx])
    }))
    yrg <- range(all_q, na.rm=TRUE)

    plot(time[idx], rep(NA_real_, sum(idx)), type="n", ylim=yrg,
         xlab="Tiempo", ylab=paste0(kk,"(t)"), main=paste(main_title, "—", kk))
    for (nm in names(bands_list)) {
      bd <- bands_list[[nm]]
      q05 <- bd[[paste0(kk,"_q05")]]
      q50 <- bd[[paste0(kk,"_q50")]]
      q95 <- bd[[paste0(kk,"_q95")]]
      # banda
      polygon(c(time[idx], rev(time[idx])),
              c(q05[idx], rev(q95[idx])),
              border=NA, col=alpha(colors[nm], 0.25))
      # mediana
      lines(time[idx], q50[idx], lwd=2, col=colors[nm], lty=linet[nm])
    }
    legend("bottomright", legend=names(bands_list), col=colors[names(bands_list)],
           lty=linet[names(bands_list)], lwd=2, bty="n")
  }
  dev.off()
}

# C y W con bandas (si existen)
plot_mc_bands_overlay(
  Filter(Negate(is.null), bands_list),
  "figsEC/compare_mc_CW_bands_zoom.png",
  kpi=c("C","W"),
  main_title="MC q05–q95 + medianas (zoom 24–84)"
)

cat("Comparación completa lista.\n",
    "- Resúmenes deterministas: resultsEC/kpis_resumen_det_all.csv (si existían)\n",
    "- Resúmenes MC:           resultsEC/kpis_resumen_mc_all.csv (si existían)\n",
    "- Figuras deterministas:  figsEC/compare_det_CW_full.png y figsEC/compare_det_CW_zoom.png\n",
    "- Figuras MC:             figsEC/compare_mc_CW_bands_zoom.png\n", sep = "")
