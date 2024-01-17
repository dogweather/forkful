---
title:                "Travailler avec json"
html_title:           "Bash: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Travailler avec JSON (JavaScript Object Notation) signifie manipuler des données structurées sous forme de clés et de valeurs. Les programmeurs utilisent souvent le format JSON pour stocker, échanger et transférer des données entre différents systèmes, car il est léger et facile à lire et à écrire.

## Comment faire:

Voici quelques exemples de code en Bash pour travailler avec JSON:

```Bash
# Lire un fichier JSON
cat data.json | jq

# Ecrire dans un fichier JSON
echo '{"name": "John Doe", "age": 30}' > data.json

# Extraire des données spécifiques d'un fichier JSON
curl 'https://example.com/api/users' | jq '.[] | select(.age > 25) | .name'

# Convertir des données en JSON
printf '{"name": "%s", "age": %d}' "Jane Doe" 25 | jq .

```

## Plongée en profondeur:

JSON a été créé en 2001 pour remplacer le format XML en tant que moyen plus simple de représenter des données structurées. Bien qu'il soit principalement utilisé dans les applications Web et mobiles, il est également utilisé dans les environnements de développement et de test en raison de sa flexibilité. D'autres formats de données couramment utilisés sont CSV (Comma-Separated Values) et YAML (YAML Ain't Markup Language).

JSON est implémenté en utilisant des objets JavaScript, ce qui facilite son utilisation pour les programmeurs familiarisés avec le langage. Il prend également en charge un large éventail de types de données, y compris les chaînes, les nombres, les tableaux et les objets.

## Voir aussi:

Pour en savoir plus sur JSON en Bash et découvrir d'autres fonctionnalités utiles, consultez les sources suivantes:

- [Documentation officielle de Bash pour JSON](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [La manipulation de données avec jq](https://stedolan.github.io/jq/)
- [Introduction à JSON et son utilisation en développement](https://www.w3schools.com/js/js_json_intro.asp)