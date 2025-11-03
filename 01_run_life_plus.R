# ==========================================
# scripts/01_run_life_plus.R
# Baseline vs Vida Útil+ (determinista)
# ==========================================

source("R/build_model_ec.R")
source("R/policy_tools_ec.R")

dir.create("figsEC", showWarnings = FALSE)
dir.create("resultsEC", showWarnings = FALSE)

# Definiciones comunes
S0 <- list(S_virgen=1e7, S_prod=0, S_uso=2e4, S_post=0, S_rec=0, S_res=0)
P  <- list(alpha_ext=5e3, alpha_out=0.15, tau_life=36,
           rho_rec=0.45, eps_sort=0.75, eps_rec=0.80, beta_feed=0.10)
time <- seq(0, 90, by=1)

# Baseline (factores = 1)
m_base <- build_model_ec(S0, P)
vt_base <- list(
  rec_factor  = data.frame(valor = rep(1, length(time))),
  life_factor = data.frame(valor = rep(1, length(time))),
  sort_factor = data.frame(valor = rep(1, length(time))),
  recy_factor = data.frame(valor = rep(1, length(time)))
)
res_base <- simular_modelo_ode_estocastico(m_base, vt_base, tiempo=time, rtol=1e-6, atol=1e-6)

# Vida Útil+ (incremento progresivo de vida útil: life_factor>1)
scen_life <- list(policy = list(type = "life_boost",
                                ramp = list(delta = 0.30, t_mid = 48, steep = 0.10)))
vt_life <- build_policy_temporals_ec(time, scen_life)
m_life <- build_model_ec(S0, P)
res_life <- simular_modelo_ode_estocastico(m_life, vt_life, tiempo=time, rtol=1e-6, atol=1e-6)

# Guardar
write.csv(res_base, "resultsEC/baseline_det.csv", row.names = FALSE)
write.csv(res_life, "resultsEC/esc_life_det.csv", row.names = FALSE)

# Gráficos rápidos
png("figsEC/baseline_vs_life_Suso.png", width = 1100, height = 700)
plot(time, res_base$S_uso, type="l", lwd=2, xlab="Tiempo", ylab="S_uso",
     main="Stock en uso (baseline vs Vida Útil+)")
lines(time, res_life$S_uso, lwd=2, lty=2)
legend("topleft", legend=c("baseline","Vida Útil+"), lwd=2, lty=c(1,2), bty="n")
dev.off()

png("figsEC/baseline_vs_life_Sres.png", width = 1100, height = 700)
plot(time, res_base$S_res, type="l", lwd=2, xlab="Tiempo", ylab="S_res",
     main="Residuo acumulado (baseline vs Vida Útil+)")
lines(time, res_life$S_res, lwd=2, lty=2)
legend("topleft", legend=c("baseline","Vida Útil+"), lwd=2, lty=c(1,2), bty="n")
dev.off()

cat("Listo determinista: CSV en resultsEC/ y figuras en figsEC/ (Vida Útil+).\n")
