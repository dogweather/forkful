---
title:                "Manipulation de JSON"
date:                  2024-01-19
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? 
Travailler avec JSON, c'est jongler avec un format de données léger pour l'interchange d'infos. Les programmeurs s'y collent pour la simplicité, la portabilité et parce qu'Internet kiffe JSON pour les APIs.

## How to:
Avec `jq`, t'as le couteau suisse pour JSON. Installe-le (`sudo apt-get install jq`), puis plonge.

```Bash
echo '{"nom": "Dupont", "age": 33}' | jq '.'
```
Sortie: 
```
{
  "nom": "Dupont",
  "age": 33
}
```
Pour extraire un champ:
```Bash
echo '{"nom": "Dupont", "age": 33}' | jq '.nom'
```
Sortie:
```
"Dupont"
```
## Deep Dive
JSON, c’est JavaScript Object Notation. C'est né dans les années 2000. XML était là avant, mais plus lourd. `jq` est ton outil en ligne de commande. Une alternative? `jshon`. L'implémentation c'est une question de goûts, mais `jq` c'est plus populaire.

## See Also
- jq Manual: https://stedolan.github.io/jq/manual/
- JSON sur Wikipédia: https://fr.wikipedia.org/wiki/JavaScript_Object_Notation
- Comparaison de `jq` et `jshon`: https://www.slant.co/versus/21012/21022/~jq_vs_jshon
