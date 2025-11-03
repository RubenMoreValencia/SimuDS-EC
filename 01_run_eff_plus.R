# ==========================================
# scripts/01_run_eff_plus.R
# Baseline vs Eficiencia+ (determinista)
# ==========================================

source("R/build_model_ec.R")
source("R/policy_tools_ec.R")

dir.create("resultsEC", FALSE)
dir.create("figsEC", FALSE)

# Base
S0 <- list(S_virgen=1e7, S_prod=0, S_uso=2e4, S_post=0, S_rec=0, S_res=0)
P  <- list(alpha_ext=5e3, alpha_out=0.15, tau_life=36,
           rho_rec=0.45, eps_sort=0.75, eps_rec=0.80, beta_feed=0.10)
time <- 0:90

# Factores baseline (=1)
vt_base <- list(
  rec_factor  = data.frame(valor = rep(1, length(time))),
  life_factor = data.frame(valor = rep(1, length(time))),
  sort_factor = data.frame(valor = rep(1, length(time))),
  recy_factor = data.frame(valor = rep(1, length(time)))
)

# Eficiencia+ (eff_boost): mejora de clasificación y reciclaje (más visible)
scen_eff <- list(policy = list(
  type = "eff_boost",
  # puedes ajustar ramp_sort / ramp_recy si quieres distintos timings
  ramp = list(delta = 0.45, t_mid = 36, steep = 0.20)  # default si no pasas los específicos
))
vt_eff <- build_policy_temporals_ec(time, scen_eff)

# Simulación determinista
m_base <- build_model_ec(S0, P)
res_base <- simular_modelo_ode_estocastico(m_base, vt_base, tiempo=time, rtol=1e-6, atol=1e-6)

m_eff <- build_model_ec(S0, P)
res_eff <- simular_modelo_ode_estocastico(m_eff, vt_eff, tiempo=time, rtol=1e-6, atol=1e-6)

# Guardar
write.csv(res_base, "resultsEC/baseline_det.csv", row.names = FALSE)
write.csv(res_eff,  "resultsEC/esc_eff_det.csv",  row.names = FALSE)

# Figuras rápidas (zoom 24–84)
rng <- time >= 24 & time <= 84

png("figsEC/eff_Srec_zoom.png", 1200, 800)
plot(time[rng], res_base$S_rec[rng], type="l", lwd=2, xlab="Tiempo", ylab="S_rec",
     main="Stock reciclado — Baseline vs Eficiencia+ (zoom 24–84)")
lines(time[rng], res_eff$S_rec[rng],  lwd=2, lty=2)
legend("topleft", c("baseline","Eficiencia+"), lwd=2, lty=c(1,2), bty="n")
dev.off()

png("figsEC/eff_Sres_zoom.png", 1200, 800)
plot(time[rng], res_base$S_res[rng], type="l", lwd=2, xlab="Tiempo", ylab="S_res",
     main="Residuo acumulado — Baseline vs Eficiencia+ (zoom 24–84)")
lines(time[rng], res_eff$S_res[rng], lwd=2, lty=2)
legend("topleft", c("baseline","Eficiencia+"), lwd=2, lty=c(1,2), bty="n")
dev.off()

cat("Listo determinista Eficiencia+: CSV en resultsEC/ y figuras en figsEC/.\n")
