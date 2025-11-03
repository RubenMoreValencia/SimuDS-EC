# ==========================================
# kpi_tools_ec.R  (SimuDS-EC)
# KPIs de Economía Circular + agregaciones
# Incluye helpers para reconstruir flujos si faltan en el CSV.
# ==========================================

# División segura (evita NaN/Inf)
.safe_div <- function(num, den, eps = .Machine$double.eps) {
  den2 <- ifelse(abs(den) < eps, eps, den)
  num / den2
}

# Reconstruye flujos si no están en df, usando P y factores temporales (vt)
# P: list(alpha_ext, alpha_out, tau_life, rho_rec, eps_sort, eps_rec, ...)
# vt: list de data.frames con columna 'valor': rec_factor, life_factor, sort_factor, recy_factor
augment_with_flows <- function(df, P, vt) {
  n <- nrow(df)
  # Asegurar factores (si no vienen, asumir 1)
  getf <- function(name) {
    if (!is.null(vt[[name]]) && "valor" %in% names(vt[[name]]) && nrow(vt[[name]]) == n) {
      as.numeric(vt[[name]]$valor)
    } else {
      rep(1, n)
    }
  }
  rec_factor  <- getf("rec_factor")
  life_factor <- getf("life_factor")
  sort_factor <- getf("sort_factor")
  recy_factor <- getf("recy_factor")

  # Qext
  if (!"Qext" %in% names(df)) {
    # si el modelo usó Qext = alpha_ext (constante)
    df$Qext <- rep(P$alpha_ext, n)
  }

  # Qout
  if (!"Qout" %in% names(df)) {
    if (!"S_prod" %in% names(df)) stop("Falta S_prod para reconstruir Qout.")
    df$Qout <- P$alpha_out * df$S_prod
  }

  # Qobs
  if (!"Qobs" %in% names(df)) {
    if (!"S_uso" %in% names(df)) stop("Falta S_uso para reconstruir Qobs.")
    df$Qobs <- .safe_div(df$S_uso, P$tau_life * life_factor)
  }

  # Qrecy y pérdidas desde postconsumo
  need_post <- any(!c("Qrecy","Qloss_post","Qloss_sort","Qloss_rec") %in% names(df))
  if (need_post) {
    rho_eff  <- pmin(1, pmax(0, P$rho_rec  * rec_factor))
    sort_eff <- pmin(1, pmax(0, P$eps_sort * sort_factor))
    recy_eff <- pmin(1, pmax(0, P$eps_rec  * recy_factor))


    Qloss_post <- df$Qobs * (1 - rho_eff)
    Qloss_sort <- df$Qobs * (rho_eff) * (1 - sort_eff)
    Qrecy      <- df$Qobs * (rho_eff) * (sort_eff) * (recy_eff)
    Qloss_rec  <- df$Qobs * (rho_eff) * (sort_eff) * (1 - recy_eff)

    if (!"Qloss_post" %in% names(df)) df$Qloss_post <- Qloss_post
    if (!"Qloss_sort" %in% names(df)) df$Qloss_sort <- Qloss_sort
    if (!"Qrecy"      %in% names(df)) df$Qrecy      <- Qrecy
    if (!"Qloss_rec"  %in% names(df)) df$Qloss_rec  <- Qloss_rec
  }

  nn <- function(x) { x[!is.finite(x)] <- 0; pmax(0, x) }
  cols <- intersect(c("Qext","Qout","Qobs","Qrecy","Qloss_post","Qloss_sort","Qloss_rec"), names(df))
  for (cc in cols) df[[cc]] <- nn(df[[cc]])
  df
}

# C(t): Circularidad instantánea = Qrecy / (Qrecy + Qloss_total)
kpi_Ct <- function(df) {
  Qloss_total <- rowSums(df[, intersect(c("Qloss_post","Qloss_sort","Qloss_rec"), names(df)), drop=FALSE])
  .safe_div(df$Qrecy, df$Qrecy + Qloss_total)
}

# D(t): Desplazamiento de virgen = Qrecy / Qext  (ajusta si prefieres otra base)
kpi_Dt <- function(df) {
  if (!"Qext" %in% names(df)) return(rep(NA_real_, nrow(df)))
  .safe_div(df$Qrecy, df$Qext)
}

# W(t): Tasa de disposición = Qloss_total / Qobs
kpi_Wt <- function(df) {
  if (!"Qobs" %in% names(df)) return(rep(NA_real_, nrow(df)))
  Qloss_total <- rowSums(df[, intersect(c("Qloss_post","Qloss_sort","Qloss_rec"), names(df)), drop=FALSE])
  .safe_div(Qloss_total, df$Qobs)
}

# AUC (área bajo la curva) por trapecios
auc_trapz <- function(time, y) {
  if (length(time) != length(y)) stop("time e y deben tener la misma longitud")
  n <- length(time)
  if (n < 2) return(0)
  sum( (y[-1] + y[-n]) * (time[-1] - time[-n]) / 2 )
}

# KPIs en el tiempo (requiere df con flujos, o se reconstruyen con P y vt)
compute_kpis_time <- function(df, P = NULL, vt = NULL) {

  stopifnot(!missing(P), !missing(vt))
  rf  <- if (!is.null(vt$rec_factor))  vt$rec_factor$valor  else rep(1, nrow(df))
  sf  <- if (!is.null(vt$sort_factor)) vt$sort_factor$valor else rep(1, nrow(df))
  rcf <- if (!is.null(vt$recy_factor)) vt$recy_factor$valor else rep(1, nrow(df))
  lf  <- if (!is.null(vt$life_factor)) vt$life_factor$valor else rep(1, nrow(df))

  rho_eff      <- pmin(1, P$rho_rec  * rf)
  eps_sort_eff <- pmin(1, P$eps_sort * sf)
  eps_rec_eff  <- pmin(1, P$eps_rec  * rcf)
  tau_eff      <- P$tau_life * lf

  C <- rho_eff * eps_sort_eff * eps_rec_eff
  W <- 1 - C
  # D: usa tu fórmula (si la tenías a partir de flujos, reconsidérala con parámetros efectivos)

  if (!is.null(P)) {
    if (is.null(vt)) stop("Si pasas P, también debes pasar vt (factores temporales).")
    df <- augment_with_flows(df, P, vt)
  } else {
    # Suponemos que ya vienen los flujos en df
    needed <- c("Qrecy","Qobs","Qext","Qloss_post","Qloss_sort","Qloss_rec")
    if (!all(needed %in% names(df))) {
      stop("Faltan columnas de flujos; pasa P y vt a compute_kpis_time() para reconstruir.")
    }
  }

  data.frame(
    time = df$time,
    C = kpi_Ct(df),
    D = kpi_Dt(df),
    W = kpi_Wt(df)
  )
}

# Resumen agregado (medianas + AUC)
summarize_kpis <- function(kpi_df) {
  with(kpi_df, {
    data.frame(
      C_med = median(C, na.rm=TRUE),
      D_med = median(D, na.rm=TRUE),
      W_med = median(W, na.rm=TRUE),
      AUC_C = auc_trapz(time, C),
      AUC_D = auc_trapz(time, D),
      AUC_W = auc_trapz(time, W)
    )
  })
}

