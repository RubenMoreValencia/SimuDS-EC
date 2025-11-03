# ==========================================
# scripts/01_run_rec_plus.R
# Ejecuta baseline y escenario Recolección+ (determinista)
# Requiere: library(SimuDS)
# ==========================================

# 0) Carga de utilitarios
source("R/build_model_ec.R")
source("R/policy_tools_ec.R")

dir.create("figsEC", showWarnings = FALSE)
dir.create("resultsEC", showWarnings = FALSE)

# 1) Definiciones comunes
S0 <- list(S_virgen=1e7, S_prod=0, S_uso=2e4, S_post=0, S_rec=0, S_res=0)
P  <- list(alpha_ext=5e3, alpha_out=0.15, tau_life=36,
           rho_rec=0.45, eps_sort=0.75, eps_rec=0.80, beta_feed=0.10)
time <- seq(0, 90, by=1)

# 2) Baseline (todos los factores = 1)
m_base <- build_model_ec(S0, P)
vt_baseline <- list(
  rec_factor  = data.frame(valor = rep(1, length(time))),
  life_factor = data.frame(valor = rep(1, length(time))),
  sort_factor = data.frame(valor = rep(1, length(time))),
  recy_factor = data.frame(valor = rep(1, length(time)))
)

res_base <- simular_modelo_ode_estocastico(
  m_base, variables_temporales = vt_baseline, tiempo = time, rtol = 1e-6, atol = 1e-6
)

# 3) Escenario Recolección+ (rampa rec_factor)
scen_rec <- list(policy = list(type = "rec_boost", ramp = list(delta=0.35, t_mid=40, steep=0.12)))
vt_rec <- build_policy_temporals_ec(time, scen_rec)

m_rec <- build_model_ec(S0, P)  # mismo P; cambia solo política (variables temporales)
res_rec <- simular_modelo_ode_estocastico(
  m_rec, variables_temporales = vt_rec, tiempo = time, rtol = 1e-6, atol = 1e-6
)

# 4) Chequeos rápidos
tol <- 1e-9
stopifnot(!any(!is.finite(as.matrix(res_base))))
stopifnot(!any(!is.finite(as.matrix(res_rec))))
# Corrige micro-negativos solo para presentación
res_base$S_post <- pmax(res_base$S_post, -tol)
res_rec$S_post  <- pmax(res_rec$S_post,  -tol)

# 5) Exportes mínimos (CSV) para comparar
write.csv(res_base, file = "resultsEC/baseline_det.csv", row.names = FALSE)
write.csv(res_rec,  file = "resultsEC/esc_rec_det.csv",  row.names = FALSE)

# 6) Gráficos rápidos (base R) – cámbialos por tus graficadores del framework si los tienes
png("figsEC/baseline_vs_rec_Srec.png", width = 1100, height = 700)
plot(time, res_base$S_rec, type="l", lwd=2, xlab="Tiempo", ylab="S_rec",
     main="Stock reciclado (baseline vs Recolección+)")
lines(time, res_rec$S_rec, lwd=2, lty=2)
legend("topleft", legend=c("baseline","Recolección+"), lwd=2, lty=c(1,2), bty="n")
dev.off()

png("figsEC/baseline_vs_rec_Sres.png", width = 1100, height = 700)
plot(time, res_base$S_res, type="l", lwd=2, xlab="Tiempo", ylab="S_res",
     main="Residuo acumulado (baseline vs Recolección+)")
lines(time, res_rec$S_res, lwd=2, lty=2)
legend("topleft", legend=c("baseline","Recolección+"), lwd=2, lty=c(1,2), bty="n")
dev.off()

cat("Listo: resultados en resultsEC/ y figuras en figsEC/ (determinista).\n")
