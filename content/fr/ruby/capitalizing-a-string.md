---
title:                "Capitaliser une chaîne de caractères"
html_title:           "Ruby: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La capitalisation d'une chaîne de caractères est le fait de mettre en majuscule la première lettre de chaque mot dans une chaîne de texte. Ce qui signifie que "bonjour tout le monde" deviendra "Bonjour Tout Le Monde". Les programmeurs le font souvent pour faciliter la lecture et la compréhension du texte.

## Comment faire:
```Ruby
"salut tout le monde".capitalize #=> "Salut tout le monde"
"hello world".capitalize #=> "Hello world"
"bOnJoUr à tOuS".capitalize #=> "Bonjour à tous"
```

## Plongée en profondeur:
Historiquement, la capitalisation était utilisée pour distinguer les titres des noms propres dans les écritures manuscrites. Aujourd'hui, c'est principalement utilisé dans le domaine de la programmation pour rendre le code plus lisible. Alternativement, on peut utiliser la méthode `upcase`, qui convertit toute la chaîne en majuscules, ou la méthode `capitalize!`, qui modifie directement la chaîne d'origine.

## Voir aussi:
- [Ruby Doc: String.capitalize](https://ruby-doc.org/core-2.7.1/String.html#method-i-capitalize)
- [Ruby Doc: String.upcase](https://ruby-doc.org/core-2.7.1/String.html#method-i-upcase)
- [Ruby Doc: String.capitalize!](https://ruby-doc.org/core-2.7.1/String.html#method-i-capitalize-21)