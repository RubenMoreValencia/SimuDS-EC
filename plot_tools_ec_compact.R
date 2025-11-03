# ==========================================
# plot_tools_ec_compact.R — SimuDS-EC
# Gráficos 4:3 vertical, 300 dpi, fondo #F5F5F5,
# paleta viridis, tipografía 11 pt y Δ% anotado.
# Requiere: ggplot2, viridisLite
# Opcional: ggrepel, patchwork
# ==========================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(viridisLite)
})

.has_ggrepel <- requireNamespace("ggrepel",  quietly = TRUE)
.has_patch   <- requireNamespace("patchwork", quietly = TRUE)

# ---------- Estilo base
theme_sci <- function() {
  ggplot2::theme_minimal(base_size = 11) %+replace% theme(
    plot.background  = element_rect(fill = "#F5F5F5", colour = NA),
    panel.background = element_rect(fill = "#F5F5F5", colour = NA),
    legend.background= element_rect(fill = "#F5F5F5", colour = NA),
    legend.key       = element_rect(fill = "#F5F5F5", colour = NA),
    plot.title       = element_text(face = "bold", margin = margin(b=6)),
    plot.subtitle    = element_text(margin = margin(b=8)),
    axis.title.x     = element_text(margin = margin(t=6)),
    axis.title.y     = element_text(margin = margin(r=6))
  )
}

# ---------- Guardado 4:3 vertical (6x8 in), 300 dpi
save_fig <- function(p, file, width = 6, height = 8, dpi = 300) {
  dir.create(dirname(file), FALSE, TRUE)
  ggsave(filename = file, plot = p, width = width, height = height, dpi = dpi, bg = "#F5F5F5")
}

# ---------- Paleta viridis coherente por escenario
scale_color_simu <- function() {
  scale_color_manual(values = c(
    "Baseline"     = viridis(5)[1],  # azul profundo
    "Collection+"  = viridis(5)[2],  # turquesa
    "Useful Life+" = viridis(5)[3],  # verde azulado
    "Efficiency+"  = viridis(5)[4]   # verde medio, evita amarillo
  ))
}

scale_fill_simu <- function(alpha = 0.25) {
  scale_fill_manual(values = scales::alpha(c(
    "Baseline"     = viridis(5)[1],
    "Collection+"  = viridis(5)[2],
    "Useful Life+" = viridis(5)[3],
    "Efficiency+"  = viridis(5)[4]
  ), alpha))
}


# ---------- Δ% vs Baseline en el último tiempo
# df_long: time, value, scenario
delta_vs_baseline <- function(df_long) {
  stopifnot(all(c("time","value","scenario") %in% names(df_long)))
  tmax <- max(df_long$time, na.rm = TRUE)
  end_vals <- subset(df_long, time == tmax)
  base_end <- end_vals$value[end_vals$scenario == "Baseline"]
  if (!length(base_end)) return(data.frame())
  out <- end_vals[end_vals$scenario != "Baseline", c("scenario","time","value")]
  out$delta_pct <- 100 * (out$value - base_end) / base_end
  out
}

# ---------- Añadir etiquetas Δ% (usa ggrepel si está)
add_delta_labels <- function(p, df_anno) {
  if (!nrow(df_anno)) return(p)
  df_anno$label <- sprintf("%+.1f%%", df_anno$delta_pct)

  if (.has_ggrepel) {
    p + ggrepel::geom_label_repel(
      data = df_anno,
      mapping = aes(x = time, y = value, label = label, colour = scenario),
      size = 3, fill = "white", label.size = 0.25, segment.size = 0.2,
      seed = 123, max.overlaps = 20,
      inherit.aes = FALSE
    )
  } else {
    p + geom_label(
      data = df_anno,
      mapping = aes(x = time, y = value, label = label, colour = scenario),
      size = 3, fill = "white", label.size = 0.25,
      inherit.aes = FALSE
    )
  }
}


# ---------- 1) Comparación de STOCKS: S_res / S_rec / S_uso
# dfs: lista nombrada con data.frames que tienen cols 'time' y var_name
plot_stock_compare <- function(dfs, var_name, title, outfile) {
  scenarios <- names(dfs)
  long <- do.call(rbind, lapply(scenarios, function(sc){
    df <- dfs[[sc]]
    if (!all(c("time", var_name) %in% names(df))) stop(paste("missing columns in", sc))
    data.frame(time = df$time, value = df[[var_name]], scenario = sc)
  }))
  long$scenario <- factor(long$scenario, levels = c("Baseline","Collection+","Useful Life+","Efficiency+"))

  p <- ggplot(long, aes(time, value, colour = scenario)) +
    geom_line(size = 1.1) +
    scale_color_simu() +
    labs(title = title, x = "t (time units)", y = sprintf("%s(t)", var_name), colour = "Scenario") +
    theme_sci()

  p <- add_delta_labels(p, delta_vs_baseline(long))
  save_fig(p, outfile)
  invisible(p)
}

# ---------- 2) KPIs deterministas C y W (opción zoom)
# kpi_list: lista nombrada con data.frames (cols: time, C, W)
plot_kpis_deterministic <- function(kpi_list, title_suffix, outfile, zoom = NULL) {
  L <- names(kpi_list)
  C_long <- do.call(rbind, lapply(L, function(sc)
    transform(kpi_list[[sc]][, c("time","C")], scenario = sc, var = "C")))
  W_long <- do.call(rbind, lapply(L, function(sc)
    transform(kpi_list[[sc]][, c("time","W")], scenario = sc, var = "W")))
  C_long <- setNames(C_long, c("time","value","scenario","var"))
  W_long <- setNames(W_long, c("time","value","scenario","var"))
  both <- rbind(C_long, W_long)
  both$scenario <- factor(both$scenario, levels = c("Baseline","Collection+","Useful Life+","Efficiency+"))
  if (!is.null(zoom)) both <- subset(both, time >= zoom[1] & time <= zoom[2])

  p <- ggplot(both, aes(time, value, colour = scenario, linetype = var)) +
    geom_line(size = 1.05) +
    scale_color_simu() +
    scale_linetype_manual(values = c(C = "solid", W = "dashed"), name = "") +
    labs(title = sprintf("Circularity and Waste Rates — %s", title_suffix),
         x = "t (time units)", y = "Value", colour = "Scenario") +
    theme_sci()

  p <- add_delta_labels(p, delta_vs_baseline(subset(both, var == "C")))
  p <- add_delta_labels(p, delta_vs_baseline(subset(both, var == "W")))
  save_fig(p, outfile)
  invisible(p)
}

# ---------- 3) KPIs con bandas Monte Carlo (q05–q95 + medianas)
# bands_list: lista nombrada con DF cols: time, C_q05,C_q50,C_q95,W_q05,W_q50,W_q95
plot_kpis_mc_bands <- function(bands_list, title_suffix, outfile, zoom = NULL) {
  L <- names(bands_list)

  to_long <- function(df, var) {
    if (!is.null(zoom)) df <- subset(df, time >= zoom[1] & time <= zoom[2])
    data.frame(time = df$time,
               q05  = df[[paste0(var,"_q05")]],
               q50  = df[[paste0(var,"_q50")]],
               q95  = df[[paste0(var,"_q95")]],
               var  = var)
  }

  C_all <- do.call(rbind, lapply(L, function(sc) cbind(to_long(bands_list[[sc]], "C"), scenario = sc)))
  W_all <- do.call(rbind, lapply(L, function(sc) cbind(to_long(bands_list[[sc]], "W"), scenario = sc)))

  make_panel <- function(df_panel, ylab) {
    df_panel$scenario <- factor(df_panel$scenario, levels = c("Baseline","Collection+","Useful Life+","Efficiency+"))
    ggplot(df_panel, aes(x = time, colour = scenario, fill = scenario)) +
      geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.12, colour = NA) +
      geom_line(aes(y = q50), size = 1.05) +
      scale_color_simu() + scale_fill_simu(0.25) +
      labs(x = "t (time units)", y = ylab, colour = "Scenario") +
      theme_sci()
  }

  pC <- make_panel(C_all, "C(t)")
  pW <- make_panel(W_all, "W(t)")

  if (.has_patch) {
    p <- pC / pW + patchwork::plot_annotation(
      title = sprintf("MC q05–q95 bands + medians — %s", title_suffix),
      theme = theme(plot.title = element_text(face = "bold"))
    )
  } else {
    p <- pC + labs(title = sprintf("MC q05–q95 bands + medians — %s (C only)", title_suffix))
  }
  save_fig(p, outfile)
  invisible(p)
}
