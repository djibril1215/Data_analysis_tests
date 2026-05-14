# --- TEST DE FRIEDMAN INTERACTIF ---

test_friedman_interactif <- function() {
  
  cat("=== 1. CONFIGURATION DU TEST ===\n")
  
  # Seuil alpha
  alpha <- as.numeric(readline(
    "Entrez le seuil de signification alpha (ex: 0.05) : "
  ))
  
  # Nombre de traitements/groupes
  n_groupes <- as.integer(readline(
    "Entrez le nombre de groupes/traitements : "
  ))
  
  # Nombre de sujets/blocs
  n_blocs <- as.integer(readline(
    "Entrez le nombre de sujets/blocs : "
  ))
  
  # Vérification alpha
  if (is.na(alpha) || alpha <= 0 || alpha >= 1) {
    
    alpha <- 0.05
    
    cat("(!) Alpha invalide, utilisation de 0.05\n")
  }
  
  cat("\n=== 2. SAISIE DES DONNÉES ===\n")
  cat("Chaque ligne représente un sujet/bloc.\n")
  cat("Entrez les valeurs séparées par des espaces.\n")
  
  # Saisie des données
  matrice <- t(sapply(1:n_blocs, function(i) {
    
    saisie <- readline(
      paste0("Sujet/Bloc ", i, " : ")
    )
    
    as.numeric(unlist(strsplit(saisie, "\\s+")))
    
  }))
  
  # Vérification dimensions
  if (ncol(matrice) != n_groupes) {
    
    cat("Erreur : nombre de valeurs incorrect ENTRER UN NOMBRE DE VALEUR EGALE OU SUPERIEUR  AU NOMBRES DE GROUPES .\n")
    
    return()
  }
  
  # ===== TEST =====
  
  res_friedman <- friedman.test(matrice)
  
  ddl <- n_groupes - 1
  
  valeur_test <- as.numeric(res_friedman$statistic)
  
  p_val <- res_friedman$p.value
  
  point_critique <- qchisq(1 - alpha, ddl)
  
  # ===== AFFICHAGE =====
  
  cat("\n=== 3. HYPOTHÈSES ===\n")
  
  cat("H0 : Les traitements ont le même effet\n")
  cat("H1 : Au moins un traitement est différent\n")
  
  cat("\n=== 4. RÉSULTATS ===\n")
  
  cat("Statistique de Friedman :", valeur_test, "\n")
  cat("Point critique          :", point_critique, "\n")
  cat("P-value                 :", p_val, "\n")
  
  # ===== DÉCISION =====
  
  cat("\n=== 5. DÉCISION ===\n")
  
  if (valeur_test > point_critique) {
    
    cat("DÉCISION : REJET de H0\n")
    
    cat("CONCLUSION : différence significative entre les traitements.\n")
    
  } else {
    
    cat("DÉCISION : NON-REJET de H0\n")
    
    cat("CONCLUSION : pas de différence significative.\n")
  }
  
  # ===== REPRÉSENTATION GRAPHIQUE =====
  
  cat("\n=== 6. REPRÉSENTATION GRAPHIQUE ===\n")
  
  # Transformation en vecteur
  valeurs <- as.vector(matrice)
  
  # Noms des groupes
  groupes <- factor(rep(
    paste("Traitement", 1:n_groupes),
    each = n_blocs
  ))
  
  # Boxplot
  boxplot(valeurs ~ groupes,
          
          col = "lightgreen",
          
          main = "Test de Friedman",
          
          xlab = "Traitements",
          
          ylab = "Valeurs")
  
  # Ajout des médianes
  points(1:n_groupes,
         
         tapply(valeurs, groupes, median),
         
         col = "red",
         
         pch = 19)
}

# ===== EXÉCUTION =====

test_friedman_interactif()
