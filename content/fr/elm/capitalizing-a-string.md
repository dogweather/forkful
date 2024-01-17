---
title:                "Capitaliser une chaîne de caractères"
html_title:           "Elm: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Capitaliser une chaîne de caractères, c'est simplement mettre la première lettre en majuscule. Les programmeurs le font souvent pour améliorer la lisibilité de leur code ou pour respecter des conventions de codage. 

## Comment faire:

```Elm
capitalize : String -> String 
capitalize str = 
    String.toUpper (String.left 1 str) ++ String.dropLeft 1 str
```

Exemple d'utilisation : 
```Elm
capitalize "bonjour" 
``` 
Résultat : "Bonjour"


## Plongée profonde:

Avant, en programmation, il n'était pas rare de voir le mot "Upper" pour "Majuscule" ou "Lower" pour "Minuscule". De nos jours, on utilise plutôt les termes "capitalize" ou "uncapitalize" (retirer les majuscules) pour être plus explicite sur les opérations effectuées.

Dans d'autres langages, comme JavaScript, la fonction de capitalisation est implémentée de manière différente, il faut donc être attentif aux particularités lors du changement de langage. 

Il est également possible d'utiliser des bibliothèques externes pour effectuer cette opération, comme la bibliothèque "elm-string-extra" qui propose la fonction "capitalize" directement. 

## Voir aussi:

Documentation Elm pour la fonction String : https://package.elm-lang.org/packages/elm/core/latest/String

Documentation Elm pour la bibliothèque "elm-string-extra" : https://package.elm-lang.org/packages/elm-community/string-extra/latest/ 

Besoin de plus d'informations sur les conventions de codage en Elm ? Consultez le guide officiel : https://guide.elm-lang.fr/code/