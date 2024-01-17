---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "Gleam: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que la mise en majuscule d'une chaîne?

La mise en majuscule d'une chaîne est le processus de conversion de toutes les lettres d'une chaîne en majuscules. Cela peut être utile dans de nombreux cas, tels que lorsque vous avez besoin de comparer deux chaînes sans tenir compte de la casse ou lorsque vous souhaitez simplement que votre texte soit plus lisible.

## Comment faire:

La mise en majuscule d'une chaîne est très simple à réaliser en utilisant la fonction `String.to_uppercase()` de Gleam. Cette fonction prend une chaîne en entrée et retourne cette même chaîne en majuscules.

```Gleam
let my_string = "Bonjour le monde !" 
let upper_string = String.to_uppercase(my_string)

```

La valeur de `upper_string` sera alors "BONJOUR LE MONDE !". Vous pouvez également utiliser cette fonction directement sur une chaîne littérale.

```Gleam
let upper_string = String.to_uppercase("Bonjour le monde !")
```

## Zoom en profondeur:

La mise en majuscule existe depuis très longtemps et est également appelée "mise en capitales" ou "mise en majuscule". Avant l'utilisation généralisée des ordinateurs, les écrivains utilisaient des caractères manuels pour mettre en majuscule les mots importants dans leurs textes. Aujourd'hui, il existe plusieurs façons de mettre en majuscule une chaîne, telles que l'utilisation de l'alphabet ASCII ou Unicode.

Si vous utilisez Gleam, vous n'avez pas besoin de vous soucier des détails d'implémentation puisque la fonction `String.to_uppercase()` s'occupe de tout pour vous.

## Voir aussi:

Pour en savoir plus sur la mise en majuscule et d'autres opérations de manipulation de chaînes, consultez la documentation officielle de Gleam : https://gleam.run/documentation/standard-library/string.html