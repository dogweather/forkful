---
date: 2024-01-20 17:47:04.638320-07:00
description: "How to: Historiquement en Elm, `String.length` retourne la longueur\
  \ en utilisant les unit\xE9s de code UTF-16, un standard pour coder les caract\xE8\
  res. Cette\u2026"
lastmod: '2024-04-05T21:53:59.175015-06:00'
model: gpt-4-1106-preview
summary: "Historiquement en Elm, `String.length` retourne la longueur en utilisant\
  \ les unit\xE9s de code UTF-16, un standard pour coder les caract\xE8res."
title: "Trouver la longueur d'une cha\xEEne de caract\xE8res"
weight: 7
---

## How to:
```Elm
import String

-- Trouver la longueur d'une chaîne de caractères
longueur : String -> Int
longueur chaine = String.length chaine

-- Utilisation et exemple d'affichage
main =
  let
      exemple = "Bonjour le monde!"
  in
  text ("La longueur de la chaîne est: " ++ String.fromInt (longueur exemple))

-- Affichage: "La longueur de la chaîne est: 17"
```

## Deep Dive:
Historiquement en Elm, `String.length` retourne la longueur en utilisant les unités de code UTF-16, un standard pour coder les caractères. Cette fonction est directe et efficace, mais attention aux caractères spéciaux ou emojis qui peuvent être comptés différemment. C'est une implémentation similaire à celle de JavaScript. Il n'y a pas d'alternatives directes dans la bibliothèque standard d'Elm, mais des packages tiers pourraient offrir des fonctions plus adaptées pour des cas d'utilisation spécifiques.

## See Also:
- [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Unicode Standard sur UTF-16](https://unicode.org/faq/utf_bom.html)
- [Elm Community Discussion sur String.length](https://discourse.elm-lang.org/)
