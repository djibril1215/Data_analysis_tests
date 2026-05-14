# --- TEST DE KRUSKAL-WALLIS INTERACTIF  ---

test_kruskal_interactif <- function() {
  
  cat("=== 1. CONFIGURATION DU TEST ===\n")
  
  alpha <- as.numeric(readline("Entrez le seuil de signification alpha (ex: 0.05) : "))
  
  n_groupes <- as.integer(readline("Entrez le nombre de groupes à comparer MINIMUN 3 : "))
  
  if (is.na(alpha) || alpha <= 0 || alpha >= 1) {
    alpha <- 0.05
    cat("(!) Seuil invalide, utilisation de la valeur par défaut : 0.05\n")
  }
  
  cat("\n=== 2. SAISIE DES DONNÉES ===\n")
  
  donnees_list <- lapply(1:n_groupes, function(i) {
    
    saisie <- readline(paste0("Valeurs du Groupe ", i, " : "))
    
    as.numeric(unlist(strsplit(saisie, "\\s+")))
  })
  
  ddl <- n_groupes - 1
  
  res_kw <- kruskal.test(donnees_list)
  
  valeur_test <- as.numeric(res_kw$statistic)
  
  point_critique <- qchisq(1 - alpha, ddl)
  
  p_val <- res_kw$p.value
  
  cat("\n=== 3. ANALYSE DE L'HYPOTHÈSE ===\n")
  
  cat("H0 : Distributions identiques\n")
  cat("H1 : Au moins une distribution est différente\n")
  
  cat("\n=== 4. VALEURS CALCULÉES ===\n")
  
  cat("Valeur du test :", valeur_test, "\n")
  cat("Point critique :", point_critique, "\n")
  cat("P-value :", p_val, "\n")
  
  cat("\n=== 5. DÉCISION ===\n")
  
  if (valeur_test > point_critique) {
    
    cat("REJET de H0\n")
    
  } else {
    
    cat("NON-REJET de H0\n")
  }
  
  # ===== GRAPHIQUE =====
  
  valeurs <- unlist(donnees_list)
  
  groupes <- factor(rep(paste("Groupe", 1:n_groupes),
                        times = sapply(donnees_list, length)))
  
  boxplot(valeurs ~ groupes,
          col = "lightblue",
          main = "Représentation graphique du test de Kruskal-Wallis",
          xlab = "Groupes",
          ylab = "Valeurs")
  
  points(1:n_groupes,
         tapply(valeurs, groupes, median),
         col = "red",
         pch = 19)
}

# Exécution
test_kruskal_interactif()
