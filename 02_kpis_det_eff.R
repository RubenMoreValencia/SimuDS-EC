# ==========================================
# scripts/02_kpis_det_eff.R
# KPIs deterministas: baseline vs Eficiencia+
# ==========================================

source("R/kpi_tools_ec.R")
source("R/policy_tools_ec.R")

dir.create("resultsEC", FALSE)
dir.create("figsEC", FALSE)

# Datos de 01_run_eff_plus.R
res_base <- read.csv("resultsEC/baseline_det.csv")
res_eff  <- read.csv("resultsEC/esc_eff_det.csv")

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
scen_eff <- list(policy = list(type = "eff_boost",
                               ramp = list(delta = 0.45, t_mid = 36, steep = 0.20)))
vt_eff <- build_policy_temporals_ec(time, scen_eff)

# Micro-tolerancia
tol <- 1e-9
if ("S_post" %in% names(res_base)) res_base$S_post <- pmax(res_base$S_post, -tol)
if ("S_post" %in% names(res_eff))  res_eff$S_post  <- pmax(res_eff$S_post,  -tol)

# KPIs en el tiempo (reconstruyendo flujos con P + vt)
kpi_base <- compute_kpis_time(res_base, P = P, vt = vt_base)
kpi_eff  <- compute_kpis_time(res_eff,  P = P, vt = vt_eff)

write.csv(kpi_base, "resultsEC/baseline_kpis_series_eff.csv", row.names = FALSE)
write.csv(kpi_eff,  "resultsEC/esc_eff_kpis_series.csv",     row.names = FALSE)

# Resumen
sum_base <- summarize_kpis(kpi_base); sum_base$escenario <- "baseline"
sum_eff  <- summarize_kpis(kpi_eff);  sum_eff$escenario  <- "Eficiencia+"

sum_all <- rbind(sum_base[, c("escenario","C_med","D_med","W_med","AUC_C","AUC_D","AUC_W")],
                 sum_eff[,  c("escenario","C_med","D_med","W_med","AUC_C","AUC_D","AUC_W")])
write.csv(sum_all, "resultsEC/kpis_resumen_det_eff.csv", row.names = FALSE)

# Gráficos (zoom 24–84)
rng <- time >= 24 & time <= 84

png("figsEC/eff_kpi_C_zoom.png", 1200, 800)
plot(kpi_base$time[rng], kpi_base$C[rng], type="l", lwd=2, xlab="Tiempo", ylab="C(t)",
     main="Circularidad — Baseline vs Eficiencia+ (zoom 24–84)")
lines(kpi_eff$time[rng],  kpi_eff$C[rng],  lwd=2, lty=2)
legend("bottomright", c("baseline","Eficiencia+"), lwd=2, lty=c(1,2), bty="n")
dev.off()

png("figsEC/eff_kpi_W_zoom.png", 1200, 800)
plot(kpi_base$time[rng], kpi_base$W[rng], type="l", lwd=2, xlab="Tiempo", ylab="W(t)",
     main="Disposición — Baseline vs Eficiencia+ (zoom 24–84)")
lines(kpi_eff$time[rng],  kpi_eff$W[rng],  lwd=2, lty=2)
legend("topright", c("baseline","Eficiencia+"), lwd=2, lty=c(1,2), bty="n")
dev.off()

cat("Listo KPIs deterministas Eficiencia+: resultados en resultsEC/ y figs en figsEC/.\n")
