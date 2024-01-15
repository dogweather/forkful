---
title:                "Convertir une chaîne en minuscules"
html_title:           "Gleam: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des chaînes de caractères dans vos projets de programmation, il est probable que vous ayez rencontré la nécessité de convertir une chaîne en lettres minuscules. Cela peut être utile pour comparer des chaînes sans prendre en compte la casse, pour une meilleure lisibilité ou pour d'autres raisons. Dans cet article, nous allons vous montrer comment réaliser cette tâche en utilisant Gleam, un langage de programmation fonctionnel robuste et expressif.

## Comment procéder

Pour convertir une chaîne en lettres minuscules en utilisant Gleam, il existe deux méthodes principales: utiliser la fonction `String.to_lower/1` ou la fonction `String.map/1`. Voici un exemple de code pour chaque méthode:

```Gleam
let str = "Bonjour Tout Le Monde"
let lower_case_str = String.to_lower(str)
# Résultat: "bonjour tout le monde"

let mapped_str = String.map(str, fn(c) -> 
    String.to_lower(c) 
end)
# Résultat: "bonjour tout le monde"
```

Comme vous pouvez le voir, les deux méthodes aboutissent au même résultat: une chaîne avec toutes les lettres en minuscules. Cependant, la fonction `String.to_lower/1` est plus concise et intuitive puisqu'elle ne nécessite pas l'utilisation d'une fonction de mappage. Il est donc conseillé de l'utiliser dans la plupart des cas.

## Approfondissement

Maintenant que vous savez comment convertir une chaîne en lettres minuscules en utilisant Gleam, vous pouvez vous demander comment cela fonctionne réellement. En fait, il n'y a rien de magique à cela. La fonction `String.to_lower/1` utilise simplement la fonction `String.map/2` en interne pour appliquer la fonction `String.to_lower/1` à chaque caractère de la chaîne d'entrée.

Il est également important de noter que lors de la conversion en minuscules, certaines lettres peuvent être remplacées par plusieurs caractères. Par exemple, la lettre "ß" en majuscule devient "SS" en minuscule en allemand. Cela peut être surprenant, mais c'est le comportement attendu.

## Voir aussi

- Documentation de la fonction `String.to_lower/1`: https://hexdocs.pm/gleam/std-string.html#to_lower/1
- Documentation de la fonction `String.map/1`: https://hexdocs.pm/gleam/std-string.html#map/1
- Exemples de projets utilisant Gleam: https://github.com/search?q=language%3Agleam