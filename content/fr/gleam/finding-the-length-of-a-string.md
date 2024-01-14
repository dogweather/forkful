---
title:                "Gleam: Trouver la longueur d'une chaîne de caractères"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur cherchant à écrire un code performant et fiable, il est important de comprendre comment trouver la longueur d'une chaîne de caractères. Cela peut sembler être une tâche banale, mais sachez que cela peut avoir un impact considérable sur les performances de votre code.

## Comment faire

Heureusement, Gleam dispose d'une fonction native pour trouver la longueur d'une chaîne de caractères: `String.length()`. Voyons un exemple concret pour mieux comprendre comment l'utiliser.

```Gleam
let my_string = "Bonjour le monde"
let length = String.length(my_string)
```

Après avoir défini une chaîne de caractères, nous utilisons simplement la fonction `String.length()` pour trouver sa longueur. En utilisant cet exemple, la variable `length` aura une valeur de 17 car la chaîne "Bonjour le monde" compte 17 caractères.

## Plongée profonde

Il peut sembler évident d'utiliser une fonction prédéfinie pour trouver la longueur d'une chaîne de caractères, mais il est important de comprendre comment cela fonctionne en interne. En utilisant `String.length()`, le compilateur Gleam compte en fait le nombre de bits nécessaires pour représenter la chaîne de caractères, et non le nombre de caractères individuels.

Cela signifie que si votre chaîne de caractères contient des caractères multibytes tels que des caractères unicode, la fonction comptera toujours le nombre de bits possédés par ces caractères. Si vous souhaitez compter le nombre de caractères plutôt que le nombre de bits, vous pouvez utiliser la fonction `Grapheme.length()`.

## Voir aussi

- [Documentation Gleam sur la gestion des chaînes de caractères](https://gleam.run/book/stdlib.html#strings)
- [Article du blog Gleam sur les opérations sur les chaînes de caractères](https://gleam.run/articles/strings.html)