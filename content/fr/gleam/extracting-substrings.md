---
title:                "Extraction de sous-chaines"
html_title:           "Gleam: Extraction de sous-chaines"
simple_title:         "Extraction de sous-chaines"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi
Avez-vous déjà eu besoin d'extraire une partie spécifique d'une chaîne de caractères en programmation ? Si oui, vous n'êtes pas seul ! La méthode d'extraction de sous-chaînes est un outil très utile pour manipuler des données et résoudre des problèmes complexes en utilisant Gleam.

## Comment Faire
Pour extraire une sous-chaîne en utilisant Gleam, vous devez d'abord connaître la syntaxe de base : 
```Gleam
str.slice(start, end) 
```
Cette fonction prend en paramètres l'index de début et de fin de la sous-chaîne que vous souhaitez extraire. Par exemple, si vous voulez extraire les trois premiers caractères d'une chaîne de caractères, vous pouvez utiliser : 
```Gleam 
str.slice(0, 3) 
```
Et voici le résultat : 
```Gleam 
"Bon" 
```

Vous pouvez également utiliser des variables à la place des valeurs numériques dans la fonction slice et même utiliser des boucles pour extraire plusieurs sous-chaînes à la fois.

## Plongée Profonde
Maintenant que vous savez comment extraire une sous-chaîne en utilisant Gleam, vous pouvez également profiter de ses autres fonctionnalités pour effectuer des opérations plus avancées. Par exemple, vous pouvez utiliser la fonction ```split``` pour diviser une chaîne en un tableau de sous-chaînes en utilisant un séparateur spécifique. Vous pouvez également utiliser des expressions régulières pour extraire des sous-chaînes en fonction d'un motif spécifique.

## Voir Aussi
- Documentation officielle sur la manipulation de chaînes de caractères en Gleam : https://gleam.run/docs/string
- Tutoriel sur les expressions régulières en Gleam : https://dev.to/katendesaumwe/guide-to-regular-expressions-in-gleam-3imh
- Exemples de code en Gleam : https://gist.github.com/happi/the-new-erlang-lang