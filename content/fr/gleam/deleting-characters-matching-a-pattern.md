---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "Gleam: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif peut sembler être une tâche sans importance, mais en réalité, cela peut grandement améliorer l'efficacité et la lisibilité de votre code. En utilisant des expressions régulières ou des méthodes spécifiques, vous pouvez filtrer et supprimer facilement des caractères indésirables dans une chaîne de caractères.

## Comment faire

Pour supprimer des caractères correspondant à un motif en utilisant Gleam, vous pouvez utiliser la méthode `String.replace` en passant l'expression régulière correspondant au motif que vous souhaitez supprimer et la chaîne de caractères cible en tant que paramètres. Par exemple, si vous voulez supprimer tous les chiffres d'une chaîne de caractères, vous pouvez utiliser:

```Gleam
let string = "abc123";
let filtered_string = string.replace(~r/[0-9]/, "");
```

Cela remplacera tous les chiffres (représentés par le motif `[0-9]`) par une chaîne de caractères vide, laissant ainsi une chaîne de caractères sans chiffres.

## Plongée en profondeur

Il existe également d'autres méthodes utiles pour supprimer des caractères correspondant à un motif en utilisant Gleam. Par exemple, la méthode `String.filter` vous permet de filtrer une chaîne de caractères en utilisant une fonction prédicative qui évalue chaque caractère. Vous pouvez également utiliser des modules tels que `gleam/regex` pour une manipulation plus avancée des expressions régulières.

## Voir aussi

- La documentation officielle de Gleam sur la méthode `String.replace`: [https://gleam.run/docs/std.html#string-replace](https://gleam.run/docs/std.html#string-replace)
- La documentation officielle de Gleam sur la méthode `String.filter`: [https://gleam.run/docs/std.html#string-filter](https://gleam.run/docs/std.html#string-filter)
- L'article "Introduction to String Manipulation in Gleam" (en anglais): [https://medium.com/ascential-engineering/introduction-to-string-manipulation-in-gleam-9cbfc440c4a3](https://medium.com/ascential-engineering/introduction-to-string-manipulation-in-gleam-9cbfc440c4a3)