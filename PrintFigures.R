source("scripts/plot_tools_ec_compact.R")
plot_stock_compare(
  dfs = list(Baseline = res_base, `Collection+` = res_rec),
  var_name = "S_res",
  title = "Evolution of S_res(t) — Baseline vs Collection+",
  outfile = "figsEC/Fig01_Sres_Baseline_vs_Collection.png"
)
plot_stock_compare(
  dfs = list(Baseline = res_base, `Collection+` = res_rec),
  var_name = "S_rec",
  title = "Evolution of S_rec(t) — Baseline vs Collection+",
  outfile = "figsEC/Fig02_Srec_Baseline_vs_Collection.png"
)
plot_stock_compare(
  dfs = list(Baseline = res_base, `Useful Life+` = res_life),
  var_name = "S_res",
  title = "Evolution of S_res(t) — Baseline vs Useful Life+",
  outfile = "figsEC/Fig03_Sres_Baseline_vs_UsefulLife.png"
)
plot_stock_compare(
  dfs = list(Baseline = res_base, `Useful Life+` = res_life),
  var_name = "S_uso",
  title = "Evolution of S_uso(t) — Baseline vs Useful Life+",
  outfile = "figsEC/Fig04_Suso_Baseline_vs_UsefulLife.png"
)

# =====================================================
# Fig05 — Residual stock (Baseline vs Efficiency+)
# =====================================================

plot_stock_compare(
  dfs = list(Baseline = res_base, `Efficiency+` = res_eff),
  var_name = "S_res",
  title = "Evolution of S_res(t) — Baseline vs Efficiency+",
  outfile = "figsEC/Fig05_Sres_Baseline_vs_Efficiency.png"
)

# =====================================================
# Fig06 — Recycled stock (Baseline vs Efficiency+)
# =====================================================
plot_stock_compare(
  dfs = list(Baseline = res_base, `Efficiency+` = res_eff),
  var_name = "S_rec",
  title = "Evolution of S_rec(t) — Baseline vs Efficiency+",
  outfile = "figsEC/Fig06_Srec_Baseline_vs_Efficiency.png"
)


plot_kpis_deterministic(
  kpi_list = list(
    Baseline     = kpi_base,
    `Collection+`  = kpi_rec,
    `Useful Life+` = kpi_life,
    `Efficiency+`  = kpi_eff
  ),
  title_suffix = "Deterministic (full horizon)",
  outfile = "figsEC/Fig07_CW_det_full.png"
)


plot_kpis_deterministic(
  kpi_list = list(Baseline = kpi_base, `Collection+` = kpi_rec,
                  `Useful Life+` = kpi_life, `Efficiency+` = kpi_eff),
  title_suffix = "Deterministic (zoom 24–84)",
  outfile = "figsEC/Fig08_CW_det_zoom.png",
  zoom = c(24, 84)
)

plot_kpis_mc_bands(
  bands_list = list(Baseline = mc_base$bands, `Collection+` = mc_rec$bands,
                    `Useful Life+` = mc_life$bands, `Efficiency+` = mc_eff$bands),
  title_suffix = "zoom 24–84",
  outfile = "figsEC/Fig09_CW_mc_bands_zoom.png",
  zoom = c(24, 84)
)

