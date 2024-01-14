---
title:    "Gleam: Majuscule d'une chaîne de caractères"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La capitalisation d'une chaîne de caractères peut être utile dans de nombreuses situations. Par exemple, si vous souhaitez afficher un titre en majuscules ou pour plainifier un texte pour l'utiliser dans une URL.

## Comment faire

La capitalisation d'une chaîne de caractères en utilisant Gleam est simple grâce à la fonction `String.uppercase()`.

```Gleam
let title = "programmation en gleam"
let new_title = String.uppercase(title)

// Résultat: "PROGRAMMATION EN GLEAM"
```

Vous pouvez également utiliser `String.capitalize()` pour ne mettre en majuscule que la première lettre d'une chaîne de caractères.

```Gleam
let name = "pierre"
let new_name = String.capitalize(name)

// Résultat: "Pierre"
```

## Plongée en profondeur

En utilisant la fonction `String.uppercase()` ou `String.capitalize()`, il est important de noter que les caractères accentués seront également mis en majuscule ou capitalisés correctement selon la langue utilisée.

De plus, si vous souhaitez capitaliser une chaîne de caractères en suivant des règles de capitalisation spécifiques à votre langue, vous pouvez utiliser la bibliothèque `String/Transform` qui propose des fonctionnalités avancées pour la manipulation de chaînes de caractères.

## Voir aussi
- [Documentation de la bibliothèque String de Gleam](https://gleam.run/documentation/stdlib/string.html)
- [Documentation de la bibliothèque String/Transform de Gleam](https://gleam.run/documentation/stdlib/string/transform.html)