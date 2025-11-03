# ==========================================
# policy_tools_ec.R — extensión para Eficiencia+
# ==========================================

# Rampa logística suave (ya existente)
policy_ramp_logistic_seq <- function(time, delta = 0.35, t_mid = 40, steep = 0.12) {
  1 + delta / (1 + exp(-steep * (time - t_mid)))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Construye lista de factores temporales como data.frames con columna 'valor'
# Soporta: rec_boost, life_boost, eff_boost (Eficiencia+)
build_policy_temporals_ec <- function(time, scen = NULL) {
  rec_factor  <- rep(1, length(time))
  life_factor <- rep(1, length(time))
  sort_factor <- rep(1, length(time))
  recy_factor <- rep(1, length(time))

  if (!is.null(scen) && !is.null(scen$policy) && !is.null(scen$policy$type)) {
    if (identical(scen$policy$type, "rec_boost")) {
      pr <- scen$policy$ramp
      rec_factor <- policy_ramp_logistic_seq(
        time,
        delta = pr$delta %||% 0.35,
        t_mid = pr$t_mid %||% 40,
        steep = pr$steep %||% 0.12
      )

    } else if (identical(scen$policy$type, "life_boost")) {
      pr <- scen$policy$ramp
      life_factor <- policy_ramp_logistic_seq(
        time,
        delta = pr$delta %||% 0.30,
        t_mid = pr$t_mid %||% 48,
        steep = pr$steep %||% 0.10
      )

    } else if (identical(scen$policy$type, "eff_boost")) {
      # Puedes dar parámetros distintos para clasificación y reciclaje
      pr <- scen$policy$ramp
      # Si te pasan ramp_sort / ramp_recy, úsalo; si no, usa 'ramp'
      prs <- scen$policy$ramp_sort %||% pr
      prr <- scen$policy$ramp_recy %||% pr

      sort_factor <- policy_ramp_logistic_seq(
        time,
        delta = prs$delta %||% 0.40,   # +40% eficiencia de clasificación
        t_mid = prs$t_mid %||% 36,
        steep = prs$steep %||% 0.18
      )
      recy_factor <- policy_ramp_logistic_seq(
        time,
        delta = prr$delta %||% 0.35,   # +35% eficiencia del reciclaje
        t_mid = prr$t_mid %||% 42,
        steep = prr$steep %||% 0.16
      )
    }
  }

  list(
    rec_factor  = data.frame(valor = rec_factor),
    life_factor = data.frame(valor = life_factor),
    sort_factor = data.frame(valor = sort_factor),
    recy_factor = data.frame(valor = recy_factor)
  )
}

