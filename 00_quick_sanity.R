# scripts/00_quick_sanity.R
source("R/build_model_ec.R")
source("R/kpi_tools_ec.R")

dir.create("figsEC", showWarnings = FALSE)
dir.create("resultsEC", showWarnings = FALSE)

S0 <- list(S_virgen=1e7, S_prod=0, S_uso=2e4, S_post=0, S_rec=0, S_res=0)
P  <- list(alpha_ext=5e3, alpha_out=0.15, tau_life=36,
           rho_rec=0.45, eps_sort=0.75, eps_rec=0.80, beta_feed=0.10)
time <- 0:120
vt1 <- list(
  rec_factor  = data.frame(valor = rep(1, length(time))),
  life_factor = data.frame(valor = rep(1, length(time))),
  sort_factor = data.frame(valor = rep(1, length(time))),
  recy_factor = data.frame(valor = rep(1, length(time)))
)

# baseline
m0   <- build_model_ec(S0, P)
res0 <- simular_modelo_ode_estocastico(m0, vt1, tiempo=time)
k0   <- compute_kpis_time(res0, P=P, vt=vt1)

# variante (rho_rec alto)
P2 <- P; P2$rho_rec <- 0.70
m1   <- build_model_ec(S0, P2)
res1 <- simular_modelo_ode_estocastico(m1, vt1, tiempo=time)
k1   <- compute_kpis_time(res1, P=P2, vt=vt1)

delta <- data.frame(
  time = time,
  dC = k1$C - k0$C,
  dW = k1$W - k0$W,
  dD = k1$D - k0$D
)
write.csv(delta, "resultsEC/quick_delta_rho_rec.csv", row.names = FALSE)

png("figsEC/quick_delta_rho_rec.png", width=1200, height=800)
par(mfrow=c(3,1), mar=c(4,4,3,1))
plot(delta$time, delta$dC, type="l", lwd=2, main="ΔC (rho_rec: 0.45 → 0.70)", xlab="Tiempo", ylab="ΔC")
abline(h=0, lty=3)
plot(delta$time, delta$dW, type="l", lwd=2, main="ΔW (rho_rec: 0.45 → 0.70)", xlab="Tiempo", ylab="ΔW")
abline(h=0, lty=3)
plot(delta$time, delta$dD, type="l", lwd=2, main="ΔD (rho_rec: 0.45 → 0.70)", xlab="Tiempo", ylab="ΔD")
abline(h=0, lty=3)
dev.off()

summ <- data.frame(
  kpi = c("C","W","D"),
  min = c(min(delta$dC), min(delta$dW), min(delta$dD)),
  q25 = c(quantile(delta$dC, .25), quantile(delta$dW, .25), quantile(delta$dD, .25)),
  med = c(median(delta$dC), median(delta$dW), median(delta$dD)),
  mean= c(mean(delta$dC), mean(delta$dW), mean(delta$dD)),
  q75 = c(quantile(delta$dC, .75), quantile(delta$dW, .75), quantile(delta$dD, .75)),
  max = c(max(delta$dC), max(delta$dW), max(delta$dD))
)
write.csv(summ, "resultsEC/quick_delta_rho_rec_summary.csv", row.names = FALSE)

cat("Sanity listo: figsEC/quick_delta_rho_rec.png y resultsEC/quick_delta_rho_rec*.csv\n")
