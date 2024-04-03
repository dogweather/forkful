---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:42.615807-07:00
description: "Travailler avec des fichiers CSV (Valeurs S\xE9par\xE9es par des Virgules)\
  \ dans Bash consiste \xE0 traiter et manipuler des donn\xE9es tabulaires stock\xE9\
  es au format\u2026"
lastmod: '2024-03-13T22:44:58.021789-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV (Valeurs S\xE9par\xE9es par des Virgules)\
  \ dans Bash consiste \xE0 traiter et manipuler des donn\xE9es tabulaires stock\xE9\
  es au format texte brut."
title: Travailler avec CSV
weight: 37
---

## Quoi & Pourquoi ?
Travailler avec des fichiers CSV (Valeurs Séparées par des Virgules) dans Bash consiste à traiter et manipuler des données tabulaires stockées au format texte brut. C'est essentiel pour les programmeurs car cela permet l'automatisation des tâches de transformation, d'analyse et d'intégration des données directement depuis la ligne de commande, sans nécessité d'utiliser des outils plus lourds ou des environnements de programmation.

## Comment faire :

**Lire un fichier CSV ligne par ligne**

```bash
while IFS=, read -r colonne1 colonne2 colonne3
do
  echo "Colonne 1 : $colonne1, Colonne 2 : $colonne2, Colonne 3 : $colonne3"
done < exemple.csv
```

*Exemple de sortie :*

```
Colonne 1 : id, Colonne 2 : nom, Colonne 3 : email
...
```

**Filtrer les lignes CSV sur la base d'une condition**

Utilisant `awk`, vous pouvez facilement filtrer les lignes. Par exemple, pour trouver les lignes où la deuxième colonne est égale à "Alice" :

```bash
awk -F, '$2 == "Alice" { print $0 }' exemple.csv
```

**Modifier la valeur d'une colonne**

Pour changer la deuxième colonne en majuscules :

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' exemple.csv
```

**Trier un fichier CSV basé sur une colonne**

Vous pouvez trier un fichier CSV en se basant, disons, sur la troisième colonne (numériquement) :

```bash
sort -t, -k3,3n exemple.csv
```

**Utiliser `csvkit` pour des tâches plus complexes**

`csvkit` est un ensemble d'outils en ligne de commande pour convertir et travailler avec des CSV. Il peut être installé via pip.

Pour convertir un fichier JSON en CSV :

```bash
in2csv donnees.json > donnees.csv
```

Pour interroger un fichier CSV en utilisant SQL :

```bash
csvsql --query "SELECT nom FROM exemple WHERE id = 10" exemple.csv
```

*Note : L'installation de `csvkit` nécessite Python et peut être réalisée en utilisant `pip install csvkit`.*
