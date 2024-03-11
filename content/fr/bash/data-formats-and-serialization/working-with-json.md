---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:31.280862-07:00
description: "Travailler avec JSON en programmation Bash implique d'analyser, d'extraire\
  \ et de manipuler des donn\xE9es JSON directement depuis la ligne de commande. Les\u2026"
lastmod: '2024-03-11T00:14:31.945687-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec JSON en programmation Bash implique d'analyser, d'extraire\
  \ et de manipuler des donn\xE9es JSON directement depuis la ligne de commande. Les\u2026"
title: Travailler avec JSON
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Travailler avec JSON en programmation Bash implique d'analyser, d'extraire et de manipuler des données JSON directement depuis la ligne de commande. Les programmeurs font souvent cela pour intégrer de manière transparente les scripts shell avec les API web et les formats modernes d'échange de données, rendant le script Bash plus puissant et pertinent dans un écosystème riche en JSON.

## Comment faire :
Bash lui-même manque de capacités intégrées d'analyse JSON, mais `jq` est un processeur JSON en ligne de commande puissant qui comble cette lacune. Voici comment l'utiliser :

**Lire un fichier JSON :**

Exemple de `data.json` :
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

Pour lire et extraire le nom du fichier JSON :
```bash
jq '.name' data.json
```
Sortie :
```
"Jane Doe"
```

**Modifier les données JSON :**

Pour mettre à jour la ville en "Los Angeles" et écrire à nouveau dans le fichier :
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**Analyser JSON à partir d'une variable :**

Si vous avez du JSON dans une variable Bash, `jq` peut également le traiter :
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
Sortie :
```
"John Doe"
```

**Travailler avec des tableaux :**

Étant donné un tableau d'éléments en JSON :
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

Pour extraire le deuxième élément (l'indexation commence à 0) :
```bash
jq '.items[1]' data.json
```
Sortie :
```
"banana"
```

Pour des opérations plus complexes et des filtrages, `jq` dispose d'un manuel complet et de tutoriels disponibles en ligne, en faisant un outil polyvalent pour tous vos besoins en Bash/JSON.
