# ==========================================
# scripts/02_kpis_det_rec.R
# KPIs deterministas: baseline vs Recolección+
# ==========================================

source("R/kpi_tools_ec.R")
source("R/policy_tools_ec.R")

dir.create("resultsEC", FALSE)
dir.create("figsEC", FALSE)

# Datos de 01_run_rec_plus.R
res_base <- read.csv("resultsEC/baseline_det.csv")
res_rec  <- read.csv("resultsEC/esc_rec_det.csv")

# Parámetros y tiempo
P <- list(alpha_ext=5e3, alpha_out=0.15, tau_life=36,
          rho_rec=0.45, eps_sort=0.75, eps_rec=0.80, beta_feed=0.10)
time <- res_base$time

# Factores
vt_base <- list(
  rec_factor  = data.frame(valor = rep(1, length(time))),
  life_factor = data.frame(valor = rep(1, length(time))),
  sort_factor = data.frame(valor = rep(1, length(time))),
  recy_factor = data.frame(valor = rep(1, length(time)))
)
scen_rec <- list(policy = list(type = "rec_boost",
                               ramp = list(delta = 0.45, t_mid = 36, steep = 0.18)))
vt_rec <- build_policy_temporals_ec(time, scen_rec)

# Micro-tolerancia
tol <- 1e-9
if ("S_post" %in% names(res_base)) res_base$S_post <- pmax(res_base$S_post, -tol)
if ("S_post" %in% names(res_rec))  res_rec$S_post  <- pmax(res_rec$S_post,  -tol)

# KPIs en el tiempo (reconstruyendo flujos)
kpi_base <- compute_kpis_time(res_base, P = P, vt = vt_base)
kpi_rec  <- compute_kpis_time(res_rec,  P = P, vt = vt_rec)

write.csv(kpi_base, "resultsEC/baseline_kpis_series_rec.csv", row.names = FALSE)
write.csv(kpi_rec,  "resultsEC/esc_rec_kpis_series.csv",     row.names = FALSE)

# Resumen
sum_base <- summarize_kpis(kpi_base); sum_base$escenario <- "baseline"
sum_rec  <- summarize_kpis(kpi_rec);  sum_rec$escenario  <- "Recolección+"

sum_all <- rbind(sum_base[, c("escenario","C_med","D_med","W_med","AUC_C","AUC_D","AUC_W")],
                 sum_rec[,  c("escenario","C_med","D_med","W_med","AUC_C","AUC_D","AUC_W")])
write.csv(sum_all, "resultsEC/kpis_resumen_det_rec.csv", row.names = FALSE)

# Gráficos (zoom 24–84)
rng <- time >= 24 & time <= 84

png("figsEC/rec_kpi_C_zoom.png", 1200, 800)
plot(kpi_base$time[rng], kpi_base$C[rng], type="l", lwd=2, xlab="Tiempo", ylab="C(t)",
     main="Circularidad — Baseline vs Recolección+ (zoom 24–84)")
lines(kpi_rec$time[rng],  kpi_rec$C[rng],  lwd=2, lty=2)
legend("bottomright", c("baseline","Recolección+"), lwd=2, lty=c(1,2), bty="n")
dev.off()

png("figsEC/rec_kpi_W_zoom.png", 1200, 800)
plot(kpi_base$time[rng], kpi_base$W[rng], type="l", lwd=2, xlab="Tiempo", ylab="W(t)",
     main="Disposición — Baseline vs Recolección+ (zoom 24–84)")
lines(kpi_rec$time[rng],  kpi_rec$W[rng],  lwd=2, lty=2)
legend("topright", c("baseline","Recolección+"), lwd=2, lty=c(1,2), bty="n")
dev.off()

cat("Listo KPIs deterministas: resultados en resultsEC/ y figs en figsEC/.\n")

