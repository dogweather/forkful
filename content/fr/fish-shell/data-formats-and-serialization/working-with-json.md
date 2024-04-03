---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:41.473993-07:00
description: "Travailler avec le JSON dans le Fish Shell implique l'analyse et la\
  \ g\xE9n\xE9ration de donn\xE9es JSON, une t\xE2che courante pour configurer des\
  \ applications,\u2026"
lastmod: '2024-03-13T22:44:58.347772-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec le JSON dans le Fish Shell implique l'analyse et la g\xE9\
  n\xE9ration de donn\xE9es JSON, une t\xE2che courante pour configurer des applications,\
  \ interagir avec des API et rationaliser les flux de travail en ligne de commande."
title: Travailler avec JSON
weight: 38
---

## Comment faire :
Le Fish Shell, en lui-même, ne dispose pas d'utilitaires intégrés pour l'analyse et la génération de JSON. Cependant, il s'intègre sans problème avec des outils tiers comme `jq` pour le traitement du JSON. `jq` est un processeur JSON en ligne de commande puissant et polyvalent qui vous permet de découper, filtrer, mapper et transformer des données structurées avec un langage simple et expressif.

### Analyser du JSON avec jq
Pour analyser un fichier JSON et extraire des données à l'aide de `jq` :

```fish
# En supposant que vous avez un fichier JSON nommé 'data.json' avec le contenu : {"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# Exemple de sortie
"Fish Shell"
```

### Génération de JSON avec jq
Créer un contenu JSON à partir de variables de shell ou de sorties :

```fish
# Créer un objet JSON à partir de variables
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# Exemple de sortie
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### Filtrer les collections JSON
Supposons que nous avons un tableau JSON d'objets dans un fichier nommé `versions.json` :
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
Pour filtrer ce tableau pour seulement les versions stables :

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# Exemple de sortie
"3.1.2"
"3.4.0"
```

Les exemples fournis démontrent la puissance de l'intégration de `jq` avec Fish Shell pour les opérations JSON. Utiliser de tels outils enrichit l'expérience shell, en faisant un environnement redoutable pour la manipulation des formats de données modernes.
