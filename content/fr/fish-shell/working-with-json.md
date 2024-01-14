---
title:                "Fish Shell: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

JSON (JavaScript Object Notation) est un format de données couramment utilisé pour échanger des informations sur le Web. Il est également utilisé dans de nombreuses applications pour stocker et transférer des données. En apprendre davantage sur Fish Shell et comment il fonctionne avec JSON peut être utile pour les développeurs souhaitant intégrer des données JSON dans leurs scripts ou leurs projets.

## Comment faire

Pour travailler avec JSON dans Fish Shell, il existe quelques outils et commandes utiles à connaître. Voici quelques exemples pratiques :

- Pour obtenir du contenu JSON à partir d'une URL, on peut utiliser la commande `curl` suivie de l'URL :
```
Fish Shell
$ curl https://exemple.com/donnees.json
```
- Pour afficher proprement le contenu d'un fichier JSON, on peut utiliser l'outil `jq` :
```
Fish Shell
$ cat fichier.json | jq
```
- On peut également utiliser les commandes de base de Fish Shell pour parcourir et manipuler des données JSON. Par exemple, pour afficher toutes les valeurs d'un objet :
```
Fish Shell
$ set exemple "{ \"fruit\": \"pomme\", \"couleur\": \"rouge\" }"
$ echo $exemple | from_json | jq .[]
```
Cela affichera `pomme` et `rouge` sur des lignes séparées.

## Plongée en profondeur

Fish Shell possède des fonctions intégrées pour travailler avec JSON, telles que `to_json` et `from_json`. Ces fonctions convertissent respectivement des données en JSON et en Fish données. Il y a également des modules tiers tels que `fish-json-reformatter` qui permettent de reformater et de valider des fichiers JSON directement depuis Fish Shell.

Il est également important de comprendre la syntaxe du format JSON et comment naviguer au sein d'un fichier pour accéder à des valeurs spécifiques. Cette [documentation de Mozilla](https://developer.mozilla.org/fr/docs/Learn/JavaScript/Objects/JSON) peut être utile pour approfondir ses connaissances sur le sujet.

## Voir aussi

- La [documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html) pour en savoir plus sur les fonctions et les commandes intégrées.
- Le plugin `jq` pour Fish Shell, [disponible sur GitHub](https://github.com/omfugoid/fish-jq).
- Le module `fish-json-reformatter`, [disponible sur GitHub](https://github.com/oh-my-fish/plugin-json-reformatter).