# ==========================================
# scripts/03_mc_eff_vs_base.R
# Monte Carlo: baseline vs Eficiencia+ (bandas visibles)
# ==========================================

source("R/build_model_ec.R")
source("R/policy_tools_ec.R")
source("R/kpi_tools_ec.R")

dir.create("resultsEC", FALSE)
dir.create("figsEC", FALSE)

# Común
S0 <- list(S_virgen=1e7, S_prod=0, S_uso=2e4, S_post=0, S_rec=0, S_res=0)
P  <- list(alpha_ext=5e3, alpha_out=0.15, tau_life=36,
           rho_rec=0.45, eps_sort=0.75, eps_rec=0.80, beta_feed=0.10)
time <- 0:90

vt_base <- list(
  rec_factor  = data.frame(valor = rep(1, length(time))),
  life_factor = data.frame(valor = rep(1, length(time))),
  sort_factor = data.frame(valor = rep(1, length(time))),
  recy_factor = data.frame(valor = rep(1, length(time)))
)
scen_eff <- list(policy = list(type = "eff_boost",
                               ramp = list(delta = 0.45, t_mid = 36, steep = 0.20)))
vt_eff <- build_policy_temporals_ec(time, scen_eff)

# Monte Carlo 500
mc_runs <- 50
set.seed(4321)
sdlog_params <- list(   # mayor incertidumbre para que se noten bandas
  rho_rec  = 0.12,
  eps_sort = 0.15,  # clave para Eficiencia+
  eps_rec  = 0.15,  # clave para Eficiencia+
  tau_life = 0.10
)

run_mc_scenario <- function(S0, P, time, vt, mc_runs, sdlog_params, scen_id) {
  m  <- build_model_ec(S0, P)
  mc <- mkc_simular_stoch(
    m_base = m,
    variables_temporales = vt,
    tiempo = time,
    runs = mc_runs,
    sdlog_params = sdlog_params,
    sdlog_F = 0.00,
    rtol = 1e-6, atol = 1e-6, quiet = TRUE
  )
  kpi_list <- lapply(mc$res_list, function(df){
    compute_kpis_time(df, P = P, vt = vt)
  })
  kpi_all <- do.call(rbind, Map(function(k, i) transform(k, run=i), kpi_list, seq_along(kpi_list)))

  qfun <- function(x) quantile(x, probs = c(0.05, 0.5, 0.95), na.rm=TRUE, names=FALSE)
  by_time <- split(kpi_all, kpi_all$time)
  band_tbl <- do.call(rbind, lapply(by_time, function(dd){
    c_time <- dd$time[1]
    c_qC <- qfun(dd$C); c_qD <- qfun(dd$D); c_qW <- qfun(dd$W)
    data.frame(
      time = c_time,
      C_q05 = c_qC[1], C_q50 = c_qC[2], C_q95 = c_qC[3],
      D_q05 = c_qD[1], D_q50 = c_qD[2], D_q95 = c_qD[3],
      W_q05 = c_qW[1], W_q50 = c_qW[2], W_q95 = c_qW[3]
    )
  }))

  collapse_by_run <- aggregate(. ~ run, data = kpi_all[,c("run","C","D","W")], median, na.rm=TRUE)
  qagg <- function(v) as.numeric(quantile(v, probs=c(0.05,0.5,0.95), na.rm=TRUE))
  agg_row <- data.frame(
    escenario = scen_id,
    C_med_q05 = qagg(collapse_by_run$C)[1],
    C_med_q50 = qagg(collapse_by_run$C)[2],
    C_med_q95 = qagg(collapse_by_run$C)[3],
    D_med_q05 = qagg(collapse_by_run$D)[1],
    D_med_q50 = qagg(collapse_by_run$D)[2],
    D_med_q95 = qagg(collapse_by_run$D)[3],
    W_med_q05 = qagg(collapse_by_run$W)[1],
    W_med_q50 = qagg(collapse_by_run$W)[2],
    W_med_q95 = qagg(collapse_by_run$W)[3]
  )

  list(bands = band_tbl, summary = agg_row)
}

# Ejecutar
mc_base <- run_mc_scenario(S0, P, time, vt_base, mc_runs, sdlog_params, "baseline")
mc_eff  <- run_mc_scenario(S0, P, time, vt_eff,  mc_runs, sdlog_params, "Eficiencia+")

# Exportes
write.csv(mc_base$bands, "resultsEC/baseline_kpis_bands_mc.csv", row.names = FALSE)
write.csv(mc_eff$bands,  "resultsEC/esc_eff_kpis_bands_mc.csv",  row.names = FALSE)
sum_mc <- rbind(mc_base$summary, mc_eff$summary)
write.csv(sum_mc, "resultsEC/kpis_resumen_mc_eff.csv", row.names = FALSE)

# Gráficos: bandas C y W (zoom 24–84)
plot_bands <- function(bands, outfile, kpi=c("C","W"), main="") {
  png(outfile, 1200, 850)
  par(mfrow=c(length(kpi),1), mar=c(4,4,3,1))
  rng <- bands$time >= 24 & bands$time <= 84
  for (kk in kpi) {
    q05 <- bands[[paste0(kk, "_q05")]]
    q50 <- bands[[paste0(kk, "_q50")]]
    q95 <- bands[[paste0(kk, "_q95")]]
    plot(bands$time[rng], q50[rng], type="l", lwd=2,
         ylim=range(c(q05[rng],q95[rng]), na.rm=TRUE),
         xlab="Tiempo", ylab=paste0(kk,"(t)"), main=paste(main, "—", kk))
    polygon(c(bands$time[rng], rev(bands$time[rng])),
            c(q05[rng], rev(q95[rng])), border=NA, col=adjustcolor("gray", 0.35))
    lines(bands$time[rng], q50[rng], lwd=2)
  }
  dev.off()
}

plot_bands(mc_base$bands, "figsEC/baseline_kpis_bands_mc_eff.png", main="Baseline")
plot_bands(mc_eff$bands,  "figsEC/esc_eff_kpis_bands_mc.png",    main="Eficiencia+")

cat("MC Eficiencia+ listo: bands en resultsEC/, figs en figsEC/, resumen en kpis_resumen_mc_eff.csv\n")
