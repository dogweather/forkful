---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:40.600164-07:00
description: "Travailler avec des fichiers CSV (Comma Separated Values ou Valeurs\
  \ S\xE9par\xE9es par des Virgules) implique l'analyse, la manipulation et la g\xE9\
  n\xE9ration de\u2026"
lastmod: 2024-02-19 22:05:16.998466
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV (Comma Separated Values ou Valeurs S\xE9\
  par\xE9es par des Virgules) implique l'analyse, la manipulation et la g\xE9n\xE9\
  ration de\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec des fichiers CSV (Comma Separated Values ou Valeurs Séparées par des Virgules) implique l'analyse, la manipulation et la génération de données dans un format tabulaire largement utilisé pour l'échange de données entre applications. Les programmeurs effectuent ces opérations pour traiter et analyser efficacement les données, automatiser des tâches ou s'intégrer avec d'autres systèmes.

## Comment :

Fish Shell, en lui-même, n'a pas de fonctions intégrées spécifiquement conçues pour la manipulation de CSV. Cependant, vous pouvez tirer parti des utilitaires Unix comme `awk`, `sed` et `cut` pour les opérations de base ou utiliser des outils spécialisés comme `csvkit` pour des tâches plus avancées.

### Lire un fichier CSV et imprimer la première colonne :
Utilisation de `cut` pour extraire la première colonne :
```fish
cut -d ',' -f1 data.csv
```
Exemple de sortie :
```
Nom
Alice
Bob
```

### Filtrer les lignes CSV en fonction de la valeur d'une colonne :
Utilisation de `awk` pour trouver les lignes où la seconde colonne correspond à "42" :
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
Exemple de sortie :
```
Bob,42,Londres
```

### Modifier un fichier CSV (par exemple, ajouter une colonne) :
Utilisation de `awk` pour ajouter une colonne avec une valeur statique "NouvelleColonne" :
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NouvelleColonne"}' data.csv > modified.csv
```
Exemple de sortie dans `modified.csv` :
```
Nom,Âge,Ville,NouvelleColonne
Alice,30,New York,NouvelleColonne
Bob,42,Londres,NouvelleColonne
```

### Utiliser `csvkit` pour des opérations plus avancées :
Tout d'abord, assurez-vous d'avoir `csvkit` installé. Sinon, installez-le en utilisant pip: `pip install csvkit`.

**Convertir un fichier CSV en JSON :**
```fish
csvjson data.csv > data.json
```
Exemple de sortie `data.json` :
```json
[{"Nom":"Alice","Âge":"30","Ville":"New York"},{"Nom":"Bob","Âge":"42","Ville":"Londres"}]
```

**Filtrer avec `csvgrep` de `csvkit` :**
```fish
csvgrep -c 2 -m 42 data.csv
```
Cette commande réplique la tâche de filtrage mais en utilisant `csvkit`, en ciblant la colonne 2 pour la valeur "42".

En conclusion, bien que Fish Shell lui-même ne puisse pas offrir de capacités de manipulation de CSV directement, son intégration transparente avec les utilitaires Unix et la disponibilité d'outils comme `csvkit` fournissent des options puissantes pour travailler avec des fichiers CSV.
