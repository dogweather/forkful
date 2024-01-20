---
title:                "Travailler avec les fichiers csv"
html_title:           "Bash: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Travailler avec des fichiers CSV est souvent une tâche courante pour les programmeurs. CSV (Comma-Separated Values) est un format de fichier largement utilisé pour stocker des données tabulaires, telles que des listes de produits ou des données de vente. Les programmeurs utilisent des outils comme Bash pour manipuler ces fichiers et extraire des informations utiles.

## Comment faire:

Pour démarrer, vous aurez besoin d'un fichier CSV à manipuler. Voici un exemple de fichier CSV avec des données sur les ventes de produits:

```Bash
Produit, Prix, Quantité vendue
Chemise, $20, 50
Pantalon, $35, 30
Chaussures, $50, 20
```

Pour extraire des informations spécifiques de ce fichier, vous pouvez utiliser des commandes Bash comme `cut` et `grep`. Par exemple, si vous voulez connaître le prix total des ventes de chaussures, vous pouvez utiliser la commande suivante:

```Bash
cut -d, -f2,3 produits.csv | grep "Chaussures" | awk '{total+=$1*$2} END {print total}'
```

Cette commande utilise `cut` pour sélectionner la deuxième et la troisième colonne (prix et quantité) et `grep` pour ne sélectionner que les lignes qui ont "Chaussures" dans la première colonne (produit). Ensuite, la commande `awk` est utilisée pour calculer le prix total des ventes de chaussures en multipliant le prix par la quantité et en ajoutant le résultat pour toutes les lignes sélectionnées.

La sortie devrait ressembler à ceci:

```
$1000
```

## Plongée en profondeur:

Le format CSV a été introduit pour la première fois en 1972 et est devenu populaire en tant que moyen simple et universel pour stocker et échanger des données tabulaires. Bien qu'il soit largement utilisé, il n'est pas le seul format de fichier pour les données tabulaires. Des alternatives telles que JSON et XML sont également couramment utilisées.

Pour travailler avec des fichiers CSV en Bash, vous pouvez également utiliser des outils tels que `awk` et `sed`, qui offrent une grande flexibilité pour manipuler des données. De plus, il existe des librairies de traitement de CSV disponibles pour des langages de programmation tels que Python et R.

## Voir aussi:

- [Documentation sur les outils Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Guide de référence des commandes Bash](https://www.tldp.org/LDP/abs/html/abs-guide.html)