
#' Fonction demi perimètre
#' @noRd
#' @param a longueur du premier coté
#' @param b longueur du deuxieme coté
#' @param c longueur du troisième coté
#'
#' @return Renvoie le demi-périmétre d'un triangle
#' @examples
#' demi_perimetre(1,2,3): égale à 3
#'
demi_perimetre <- function(a, b, c) {
  return((a + b + c) / 2)
}

  #' La fonction heron
  #'j'ai l'erreur suivante : Attachement du package : ‘heron’
  #'Les objets suivants sont masqués _par_ ‘.GlobalEnv’:
  #'demi_perimetre, heron
  #'
  #' @description La fonction permet de calculer l'aire d'un triangle
  #' à partir de  la longueur de ses côtés.
  #'
  #' @param a longueur du premier coté
  #' @param b longueur du deuxieme coté
#' @param c longueur du troisième coté
#' @return Retourne l'aire du triangle.
#' @export
#' @examples
#' heron(3,4,5) égal à 6

heron <- function(a, b, c) {
  if (!is.numeric(c(a, b, c))) {
    stop("Valeur(s) non numerique(s).")
  }
  else if (a < 0 | b < 0 | c < 0) {
    stop("Longueur(s) négative(s).")
  }
  else if (a == 0 | b == 0 | c == 0) {
    warning("Longueur(s) nulle(s).")

  }
  p <- demi_perimetre(a, b, c)
  return(sqrt(p * ((p - a) * (p - b) * (p - c))))
}


