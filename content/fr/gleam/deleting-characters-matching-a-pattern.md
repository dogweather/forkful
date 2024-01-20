---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?
Supprimer les caractères correspondant à un "pattern" signifie enlever tous les éléments qui s'adaptent à une certaine règle dans une chaine de caractères. Les développeurs utilisent cette fonction pour nettoyer et structurer les données.

## Comment faire :
En utilisant Gleam, on peut utiliser `string.trim` pour supprimer les espaces et `string.replace` pour supprimer d'autres caractères.

```gleam
import gleam/string

fn main() {
  let str = "    salut le monde!   "

  str
  |> string.trim
  |> string.replace(" ", "")
  |> io.println 
}
```
Cela va imprimer `salutlemonde!`.

## Plongée en profondeur
Historiquement, la suppression de caractères correspondant à un "pattern" est une technique essentielle pour manipuler les chaînes de caractères en programmation. Alternativement, vous pouvez utiliser `string.slice` pour supprimer des caractères spécifiques. Les détails d'implémentation varient selon le langage de programmation, mais ils impliquent généralement une boucle itérative à travers chaque caractère.

```gleam
import gleam/string.{slice, length}

fn slice_string(str: String) {
  let len = length(str)
  slice(str, 1, len - 1)
}
```

## A voir aussi
Pour plus de fonctionnalités de chaîne de caractères dans Gleam, consultez le [document officiel](https://gleam.run/documentation/). Vous pouvez également lire cet article sur [les expressions régulières en Gleam](https://gleam.run/book/tour/regular-expressions.html) pour une manipulation avancée de la chaîne de caractères.