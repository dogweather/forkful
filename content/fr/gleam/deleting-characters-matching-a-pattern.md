---
title:                "Gleam: Supprimer les caractères correspondant à un motif"
simple_title:         "Supprimer les caractères correspondant à un motif"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer tous les caractères correspondant à un modèle peut sembler une tâche simple, mais cela peut en fait être très utile dans de nombreuses situations de programmation. Il peut vous aider à nettoyer les données, à filtrer les entrées utilisateur ou à transformer des chaînes de caractères en un format spécifique. Dans cet article, nous allons vous montrer comment supprimer des caractères en utilisant le langage de programmation Gleam.

## Comment faire

Supprimer des caractères correspondant à un modèle en utilisant Gleam est très simple. Tout d'abord, nous devons utiliser la fonction `String.replace` pour remplacer le motif par une chaîne vide. Voici un exemple de code :

```Gleam
let str = "Bonjour le monde!"
let newStr = String.replace(str, Regex.from_string("le"), "")
```

Dans cet exemple, nous remplaçons "le" par une chaîne vide, ce qui aura pour effet de supprimer tous les "le" dans la chaîne originale. Le résultat sera donc "Bonour monde!".

Nous pouvons également utiliser des expressions régulières pour supprimer des caractères correspondant à un motif plus complexe. Par exemple, si nous voulons supprimer tous les chiffres d'une chaîne de caractères, nous pouvons utiliser l'expression régulière `[0-9]` comme ceci :

```Gleam
let str = "J'ai 25 ans"
let newStr = String.replace(str, Regex.from_string("[0-9]"), "")
```

Le résultat sera "J'ai ans". Comme vous pouvez le constater, tous les chiffres ont été supprimés.

## Plongée profonde

En plus de la fonction `String.replace`, Gleam dispose également de différentes fonctions qui peuvent vous aider à supprimer des caractères correspondant à un motif. Par exemple, la fonction `String.filter` vous permet de filtrer une chaîne de caractères en fonction d'un prédicat. Cela signifie que vous pouvez définir votre propre fonction qui décide si un caractère doit être conservé ou supprimé. Vous pouvez également utiliser la fonction `String.split` pour diviser une chaîne en fonction d'un motif et récupérer uniquement les parties qui vous intéressent.

## Voir aussi

 - [Documentation sur la suppression de caractères en Gleam](https://gleam.run/documentation/)
 - [Plus d'informations sur les expressions régulières en Gleam](https://gleam.run/documentation/regexes)
 - [Exemples de code pour supprimer des caractères en Gleam](https://github.com/gleam-lang/gleam/blob/main/examples/strings.string/regex/src/string/filter_example.gleam)