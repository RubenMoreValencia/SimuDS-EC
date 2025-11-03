# ================================
# build_model_ec.R  (SimuDS-EC)
# Modelo base de Economía Circular sobre SimuDS
# Estructura: stocks y flujos + parámetros y factores de política
# Requiere: library(SimuDS)
# ================================

#' Construye el modelo EC (Economía Circular) sobre SimuDS
#'
#' @param S0 list con stocks iniciales (numéricos), por ejemplo:
#'   list(
#'     S_virgen = 1e7,  # grande para no agotar en pruebas
#'     S_prod   = 0,
#'     S_uso    = 2e4,
#'     S_post   = 0,
#'     S_rec    = 0,
#'     S_res    = 0
#'   )
#'
#' @param P list con parámetros escalares (numéricos), por ejemplo:
#'   list(
#'     alpha_ext = 5e3,   # extracción de virgen (unid/tiempo)
#'     alpha_out = 0.15,  # salida de productos S_prod->S_uso (1/tiempo)
#'     tau_life  = 36,    # vida media en uso (tiempo)
#'     rho_rec   = 0.45,  # fracción recolectada del flujo obsoleto
#'     eps_sort  = 0.75,  # eficiencia de clasificación
#'     eps_rec   = 0.80,  # eficiencia de reciclaje
#'     beta_feed = 0.10   # reintegro S_rec->S_prod (1/tiempo)
#'   )
#'
#' @return objeto modelo SimuDS (para simular con funciones del framework)
#'
#' @details
#' Stocks:
#'   - S_virgen: reservorio de material virgen
#'   - S_prod  : material en proceso / manufactura / inventario de salida
#'   - S_uso   : material en uso
#'   - S_post  : material postconsumo en tránsito
#'   - S_rec   : material reciclado utilizable
#'   - S_res   : residuo (disposición final)
#'
#' Flujos principales:
#'   - Qext      : extracción (S_virgen -> S_prod)
#'   - Qout      : salida a uso (S_prod -> S_uso)
#'   - Qobs      : obsolescencia (S_uso -> S_post)
#'   - Qloss_post: residuo por NO recolección (S_post -> S_res)
#'   - Qloss_sort: residuo por clasificación (S_post -> S_res)
#'   - Qrecy     : reciclaje efectivo (S_post -> S_rec)
#'   - Qloss_rec : residuo por reciclaje (S_post -> S_res)
#'   - Qfeed     : reintegro de S_rec a S_prod
#'
#' Factores de política (multiplicadores temporales):
#'   - rec_factor(t)  → multiplica rho_rec
#'   - life_factor(t) → multiplica tau_life (a >1 ⇒ vida más larga ⇒ menor Qobs)
#'   - sort_factor(t) → multiplica eps_sort
#'   - recy_factor(t) → multiplica eps_rec
#'
#' NOTA: los factores temporales deben proveerse en `variables_temporales`
#' como data.frames con columna `valor`, del mismo largo que el vector `tiempo`.
#'
build_model_ec <- function(S0, P) {

  # --- 1) Inicializar modelo base ---
  m <- inicializar_modelo()

  # --- 2) Declarar stocks ---
  m <- agregar_stock(m, "S_virgen", valor_inicial = S0$S_virgen)
  m <- agregar_stock(m, "S_prod",   valor_inicial = S0$S_prod)
  m <- agregar_stock(m, "S_uso",    valor_inicial = S0$S_uso)
  m <- agregar_stock(m, "S_post",   valor_inicial = S0$S_post)
  m <- agregar_stock(m, "S_rec",    valor_inicial = S0$S_rec)
  m <- agregar_stock(m, "S_res",    valor_inicial = S0$S_res)

  # --- 3) Parámetros escalares (como strings numéricas para el parser) ---
  m <- agregar_variable(m, "alpha_ext", as.character(P$alpha_ext))
  m <- agregar_variable(m, "alpha_out", as.character(P$alpha_out))
  m <- agregar_variable(m, "tau_life",  as.character(P$tau_life))
  m <- agregar_variable(m, "rho_rec",   as.character(P$rho_rec))
  m <- agregar_variable(m, "eps_sort",  as.character(P$eps_sort))
  m <- agregar_variable(m, "eps_rec",   as.character(P$eps_rec))
  m <- agregar_variable(m, "beta_feed", as.character(P$beta_feed))

  # --- 4) Placeholders de factores temporales (se inyectan vía variables_temporales) ---
  # Por defecto = 1; deben ser reemplazados por series: rec_factor, life_factor, sort_factor, recy_factor
  m <- agregar_variable(m, "rec_factor",  "1")
  m <- agregar_variable(m, "life_factor", "1")
  m <- agregar_variable(m, "sort_factor", "1")
  m <- agregar_variable(m, "recy_factor", "1")

  # --- 5) Flujos principales (sin flujos "neutros" en S_post) ---

  # Extracción de virgen hacia manufactura
  # Qext = alpha_ext
  m <- agregar_flujo(
    m, nombre = "Qext", desde = "S_virgen", hacia = "S_prod",
    expresion = "alpha_ext"
  )

  # Salida a uso
  # Qout = alpha_out * S_prod
  m <- agregar_flujo(
    m, nombre = "Qout", desde = "S_prod", hacia = "S_uso",
    expresion = "alpha_out * S_prod"
  )

  # Obsolescencia (vida media ajustada por política)
  # Qobs = S_uso / (tau_life * life_factor)
  m <- agregar_flujo(
    m, nombre = "Qobs", desde = "S_uso", hacia = "S_post",
    expresion = "S_uso / (tau_life * life_factor)"
  )

  # --- Reparto del postconsumo (todo sale de S_post por cuatro ramas) ---

  # 1) Pérdida por NO recolección
  # Qloss_post = Qobs * (1 - rho_rec * rec_factor)
  m <- agregar_flujo(
    m, nombre = "Qloss_post", desde = "S_post", hacia = "S_res",
    expresion = "Qobs * (1 - rho_rec * rec_factor)"
  )

  # 2) Pérdidas en clasificación
  # Qloss_sort = Qobs * (rho_rec * rec_factor) * (1 - eps_sort * sort_factor)
  m <- agregar_flujo(
    m, nombre = "Qloss_sort", desde = "S_post", hacia = "S_res",
    expresion = "Qobs * (rho_rec * rec_factor) * (1 - eps_sort * sort_factor)"
  )

  # 3) Reciclaje efectivo (clasificado y reciclado)
  # Qrecy = Qobs * (rho_rec * rec_factor) * (eps_sort * sort_factor) * (eps_rec * recy_factor)
  m <- agregar_flujo(
    m, nombre = "Qrecy", desde = "S_post", hacia = "S_rec",
    expresion = "Qobs * (rho_rec * rec_factor) * (eps_sort * sort_factor) * (eps_rec * recy_factor)"
  )

  # 4) Pérdidas en reciclaje (como residuo)
  # Qloss_rec = Qobs * (rho_rec * rec_factor) * (eps_sort * sort_factor) * (1 - eps_rec * recy_factor)
  m <- agregar_flujo(
    m, nombre = "Qloss_rec", desde = "S_post", hacia = "S_res",
    expresion = "Qobs * (rho_rec * rec_factor) * (eps_sort * sort_factor) * (1 - eps_rec * recy_factor)"
  )

  # Reintegro de material reciclado a producción (cierra el lazo)
  # Qfeed = beta_feed * S_rec
  m <- agregar_flujo(
    m, nombre = "Qfeed", desde = "S_rec", hacia = "S_prod",
    expresion = "beta_feed * S_rec"
  )

  # --- 6) (Opcional) Variables derivadas útiles para KPIs (solo si se desean en salida) ---
  # Nota: si tu framework soporta variables derivadas evaluadas en cada paso y las expone en salida,
  # puedes agregarlas aquí para facilitar KPIs. Si no, calcula KPIs en post-proceso.
  # m <- agregar_variable(m, "Qloss_total",
  #   "Qloss_post + Qloss_sort + Qloss_rec")
  # m <- agregar_variable(m, "Qrecy_total",
  #   "Qrecy")

  return(m)
}

# ================================
# Ejemplo mínimo de uso (en consola):
# --------------------------------
# source("R/build_model_ec.R")
# S0 <- list(S_virgen=1e7, S_prod=0, S_uso=2e4, S_post=0, S_rec=0, S_res=0)
# P  <- list(alpha_ext=5e3, alpha_out=0.15, tau_life=36,
#            rho_rec=0.45, eps_sort=0.75, eps_rec=0.80, beta_feed=0.10)
# m <- build_model_ec(S0, P)
# time <- seq(0, 120, by=1)
# variables_temporales <- list(
#   rec_factor  = data.frame(valor=rep(1, length(time))),
#   life_factor = data.frame(valor=rep(1, length(time))),
#   sort_factor = data.frame(valor=rep(1, length(time))),
#   recy_factor = data.frame(valor=rep(1, length(time)))
# )
# res_det <- simular_modelo_ode_estocastico(
#   m, variables_temporales, tiempo=time, rtol=1e-6, atol=1e-6
# )
# tol <- 1e-9
# res_det$S_post <- pmax(res_det$S_post, -tol)  # o pmax(res_det$S_post, 0) si prefieres “recortar”
# head(res_det)
# ================================

