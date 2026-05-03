# --- TEST DE KRUSKAL-WALLIS INTERACTIF COMPLET ---

test_kruskal_interactif <- function() {
  
  cat("=== 1. CONFIGURATION DU TEST ===\n")
  # Demande du seuil alpha
  alpha <- as.numeric(readline("Entrez le seuil de signification alpha (ex: 0.05) : "))
  
  # Demande du nombre de groupes
  n_groupes <- as.integer(readline("Entrez le nombre de groupes à comparer : "))
  
  # Vérification de sécurité pour alpha
  if (is.na(alpha) || alpha <= 0 || alpha >= 1) {
    alpha <- 0.05
    cat("(!) Seuil invalide, utilisation de la valeur par défaut : 0.05\n")
  }
  
  # 2. RÉCUPÉRATION DES DONNÉES SANS BOUCLE FOR (via lapply)
  cat("\n=== 2. SAISIE DES DONNÉES ===\n")
  cat("Saisissez les valeurs pour chaque groupe (séparées par des espaces) :\n")
  donnees_list <- lapply(1:n_groupes, function(i) {
    saisie <- readline(paste0("Valeurs du Groupe ", i, " : "))
    # Nettoyage et conversion en numérique
    as.numeric(unlist(strsplit(saisie, "\\s+")))
  })
  
  # 3. CALCULS STATISTIQUES
  ddl <- n_groupes - 1
  res_kw <- kruskal.test(donnees_list)
  
  # Extraction propre pour éviter les erreurs de comparaison
  valeur_test <- as.numeric(res_kw$statistic)
  point_critique <- qchisq(1 - alpha, ddl)
  p_val <- res_kw$p.value
  
  # 4. ANALYSE ET LOI DE PROBABILITÉ
  cat("\n=== 3. ANALYSE DE L'HYPOTHÈSE ===\n")
  cat("H0 : Distributions identiques (médianes égales)\n")
  cat("H1 : Au moins une distribution est différente\n")
  cat("Loi de probabilité : Chi-deux (", ddl, "ddl )\n")
  
  cat("\n=== 4. VALEURS CALCULÉES ===\n")
  cat("Valeur du test (H) :", valeur_test, "\n")
  cat("Point critique     :", point_critique, "\n")
  cat("P-value            :", p_val, "\n")
  
  # 5. DÉCISION ET CONCLUSION
  cat("\n=== 5. DÉCISION ET CONCLUSION ===\n")
  
  if (is.na(valeur_test)) {
    cat("Erreur : Impossible de calculer le test. Vérifiez vos saisies.\n")
  } else if (valeur_test > point_critique) {
    cat("DÉCISION   : REJET de l'hypothèse nulle (H0)\n")
    cat("CONCLUSION : Au seuil", alpha, ", il existe une différence significative entre les groupes.\n")
  } else {
    cat("DÉCISION   : NON-REJET de l'hypothèse nulle (H0)\n")
    cat("CONCLUSION : Pas de différence significative démontrée au seuil", alpha, ".\n")
  }
}

# Lancer l'exécution
test_kruskal_interactif()


