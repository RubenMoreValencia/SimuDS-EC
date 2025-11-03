# ==========================================
# scripts/02_kpis_det_life.R
# KPIs para baseline vs Vida Útil+ (determinista)
# Lee CSV de stocks y reconstruye flujos con P + factores (vt)
# ==========================================

source("R/kpi_tools_ec.R")
source("R/policy_tools_ec.R")

dir.create("resultsEC", showWarnings = FALSE)
dir.create("figsEC", showWarnings = FALSE)

# 1) Lee los CSV creados por 01_run_life_plus.R
res_base <- read.csv("resultsEC/baseline_det.csv")
res_life <- read.csv("resultsEC/esc_life_det.csv")

# 2) Parámetros (los mismos usados al simular)
P <- list(
  alpha_ext = 5e3,
  alpha_out = 0.15,
  tau_life  = 36,
  rho_rec   = 0.45,
  eps_sort  = 0.75,
  eps_rec   = 0.80,
  beta_feed = 0.10
)

# 3) Reconstruye factores temporales para cada escenario
time <- res_base$time

# baseline: todos = 1
vt_base <- list(
  rec_factor  = data.frame(valor = rep(1, length(time))),
  life_factor = data.frame(valor = rep(1, length(time))),
  sort_factor = data.frame(valor = rep(1, length(time))),
  recy_factor = data.frame(valor = rep(1, length(time)))
)

# Vida Útil+ (igual que en 01_run_life_plus.R)
scen_life <- list(policy = list(type = "life_boost",
                                ramp = list(delta = 0.30, t_mid = 48, steep = 0.10)))
vt_life <- build_policy_temporals_ec(time, scen_life)

# 4) Tolerancia opcional para micro-negativos
tol <- 1e-9
if ("S_post" %in% names(res_base)) res_base$S_post <- pmax(res_base$S_post, -tol)
if ("S_post" %in% names(res_life)) res_life$S_post <- pmax(res_life$S_post, -tol)

# 5) KPIs en el tiempo (reconstruyendo flujos con P + vt)
kpi_base <- compute_kpis_time(res_base, P = P, vt = vt_base)
kpi_life <- compute_kpis_time(res_life, P = P, vt = vt_life)

# 6) Guardar series
write.csv(kpi_base, "resultsEC/baseline_kpis_series_life.csv", row.names = FALSE)
write.csv(kpi_life, "resultsEC/esc_life_kpis_series.csv",   row.names = FALSE)

# 7) Resumen agregado
sum_base <- summarize_kpis(kpi_base); sum_base$escenario <- "baseline"
sum_life <- summarize_kpis(kpi_life); sum_life$escenario <- "Vida Útil+"

sum_all <- rbind(
  sum_base[, c("escenario","C_med","D_med","W_med","AUC_C","AUC_D","AUC_W")],
  sum_life[, c("escenario","C_med","D_med","W_med","AUC_C","AUC_D","AUC_W")]
)
write.csv(sum_all, "resultsEC/kpis_resumen_det_life.csv", row.names = FALSE)

# 8) Gráficos (C(t) y W(t))
png("figsEC/kpi_C_time_base_vs_life.png", width = 1100, height = 700)
plot(kpi_base$time, kpi_base$C, type="l", lwd=2, xlab="Tiempo", ylab="C(t)",
     main="Circularidad en el tiempo — baseline vs Vida Útil+")
lines(kpi_life$time,  kpi_life$C,  lwd=2, lty=2)
legend("bottomright", legend=c("baseline","Vida Útil+"), lwd=2, lty=c(1,2), bty="n")
dev.off()

png("figsEC/kpi_W_time_base_vs_life.png", width = 1100, height = 700)
plot(kpi_base$time, kpi_base$W, type="l", lwd=2, xlab="Tiempo", ylab="W(t)",
     main="Tasa de disposición — baseline vs Vida Útil+")
lines(kpi_life$time,  kpi_life$W,  lwd=2, lty=2)
legend("topright", legend=c("baseline","Vida Útil+"), lwd=2, lty=c(1,2), bty="n")
dev.off()

cat("Listo: KPIs (series y resumen) en resultsEC/ y figuras en figsEC/ para Vida Útil+.\n")
