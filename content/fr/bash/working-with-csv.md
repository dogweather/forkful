---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"

category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Les fichiers CSV (valeurs séparées par des virgules) stockent des données tabulaires sous une forme texte simple, ce qui en fait un format commun pour échanger des données entre applications. Les programmeurs utilisent souvent des scripts Bash pour manipuler ces fichiers car c'est rapide et efficace pour des tâches de manipulation de texte.

## How to: (Comment faire :) 
```Bash
# Lire un fichier CSV
while IFS=, read -r col1 col2 col3; do
  echo "Colonne 1: $col1 - Colonne 2: $col2 - Colonne 3: $col3"
done < fichier.csv

# Exemple de sortie:
# Colonne 1: valeur1 - Colonne 2: valeur2 - Colonne 3: valeur3

# Ajouter une ligne à un fichier CSV
echo "nouvelle_valeur1,nouvelle_valeur2,nouvelle_valeur3" >> fichier.csv

# Trier un fichier CSV par la deuxième colonne
sort -t, -k2 fichier.csv
```

## Deep Dive (Plongée Profonde)
Les fichiers CSV ont été largement adoptés dans les années 1970 comme moyen facile de transférer des tables de données entre différents programmes. Des alternatives, comme le format JSON ou XML, permettent de représenter des données plus complexes, mais le CSV reste populaire en raison de sa simplicité. La manipulation de CSV en Bash est généralement facilitée par les commandes UNIX comme `cut`, `sort`, `awk`, qui offrent une grande flexibilité.

## See Also (Voir Aussi)
- [GNU Coreutils](https://www.gnu.org/software/coreutils/coreutils.html): Utilitaires de base pour la manipulation de texte.
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/): Manuel de référence pour Bash.
- [AWK Manual](https://www.gnu.org/software/gawk/manual/): Manuel pour AWK, un langage de traitement de texte.
