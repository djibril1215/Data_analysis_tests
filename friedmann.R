# --- TEST DE FRIEDMAN INTERACTIF 

test_friedman_interactif <- function() {
  
  cat("=== 1. CONFIGURATION DU TEST ===\n")
  alpha <- as.numeric(readline("Entrez le seuil alpha (ex: 0.05) : "))
  n_conditions <- as.integer(readline("Entrez le nombre de conditions (ex: 3) : "))
  
  # 2. RÉCUPÉRATION DES DONNÉES
  cat("\nNote : Pour Friedman, chaque groupe doit avoir le même nombre de valeurs.\n")
  
  donnees_list <- lapply(1:n_conditions, function(i) {
    saisie <- readline(paste0("Valeurs de la Condition ", i, " : "))
    as.numeric(unlist(strsplit(saisie, "\\s+")))
  })
  
  # Conversion en matrice (obligatoire pour friedman.test)
  matrice_donnees <- do.call(cbind, donnees_list)
  
  # 3. CALCULS STATISTIQUES
  ddl <- n_conditions - 1
  res_f <- friedman.test(matrice_donnees)
  
  valeur_test <- as.numeric(res_f$statistic)
  point_critique <- qchisq(1 - alpha, ddl)
  p_val <- res_f$p.value
  
  # 4. ANALYSE DE L'HYPOTHÈSE
  cat("\n=== 2. ANALYSE DE L'HYPOTHÈSE ===\n")
  cat("H0 : Les conditions n'ont pas d'effet (distributions identiques).\n")
  cat("H1 : Au moins une condition diffère des autres.\n")
  cat("Loi de probabilité : Chi-deux (", ddl, "ddl )\n")
  
  cat("\n=== 3. VALEURS CALCULÉES ===\n")
  cat("Valeur du test (Chi-deux) :", valeur_test, "\n")
  cat("Point critique            :", point_critique, "\n")
  cat("P-value                   :", p_val, "\n")
  
  # 5. DÉCISION ET CONCLUSION
  cat("\n=== 4. DÉCISION ET CONCLUSION ===\n")
  
  if (is.na(valeur_test)) {
    cat("Erreur : Les groupes n'ont pas la même taille (obligatoire pour Friedman).\n")
  } else if (valeur_test > point_critique) {
    cat("DÉCISION   : REJET de l'hypothèse nulle (H0)\n")
    cat("CONCLUSION : Il existe une différence significative entre les conditions.\n")
  } else {
    cat("DÉCISION   : NON-REJET de l'hypothèse nulle (H0)\n")
    cat("CONCLUSION : Aucune différence significative n'est démontrée.\n")
  }
}

# Lancer le test
test_friedman_interactif()
