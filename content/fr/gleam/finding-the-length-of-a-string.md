---
title:                "Gleam: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez vouloir trouver la longueur d'une chaîne de caractères dans votre code Gleam. Cela peut être utile lorsque vous travaillez avec des chaînes de caractères dynamiques, ou lorsque vous avez besoin d'une vérification de validation pour un certain nombre de caractères.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en Gleam, vous pouvez utiliser la fonction `String.length` et passer votre chaîne en tant qu'argument. Voici un exemple de code avec une chaîne de caractères et une sortie de résultat correspondante :

```
Gleam
let my_string = "Bonjour, je m'appelle Sophie."
IO.print(String.length(my_string))```

Cette fonction renverra le nombre total de caractères dans votre chaîne, y compris les espaces. Si vous voulez exclure les espaces de la longueur, vous pouvez utiliser la méthode `String.trim` pour enlever ces caractères avant d'utiliser `String.length`.

## Plongée en profondeur

Il est important de noter que la fonction `String.length` compte également les caractères Unicode dans votre chaîne. Cela signifie que certains caractères peuvent compter pour plus d'un caractère dans la longueur totale de la chaîne. Si vous avez besoin de compter uniquement les caractères ASCII, vous pouvez utiliser la méthode `String.trim_to_graphemes` avant d'utiliser `String.length`.

De plus, si vous travaillez avec des chaînes de caractères dynamiques, vous pouvez utiliser la fonction `String.len_bytes` pour compter la longueur en octets de votre chaîne. Cela peut être utile si vous devez limiter la taille de votre chaîne pour éviter les problèmes de performance.

## Voir aussi

- Documentation de Gleam sur la fonction `String.length` : [lien](https://gleam.run/core/String.html#length)
- Documentation de Gleam sur la fonction `String.len_bytes` : [lien](https://gleam.run/core/String.html#len_bytes)
- Définition de l'UTF-8 pour mieux comprendre la comptabilisation des caractères Unicode : [lien](https://www.rapidtables.com/code/text/unicode-characters.html)