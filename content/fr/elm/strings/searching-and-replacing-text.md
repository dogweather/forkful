---
aliases:
- /fr/elm/searching-and-replacing-text/
date: 2024-01-20 17:57:28.401275-07:00
description: "Chercher et remplacer du texte, c'est juste modifier une cha\xEEne par\
  \ une autre dans un bloc de texte. Les programmeurs font \xE7a souvent pour corriger\
  \ des\u2026"
lastmod: 2024-02-18 23:09:08.715611
model: gpt-4-1106-preview
summary: "Chercher et remplacer du texte, c'est juste modifier une cha\xEEne par une\
  \ autre dans un bloc de texte. Les programmeurs font \xE7a souvent pour corriger\
  \ des\u2026"
title: Recherche et remplacement de texte
---

{{< edit_this_page >}}

## What & Why? 
Chercher et remplacer du texte, c'est juste modifier une chaîne par une autre dans un bloc de texte. Les programmeurs font ça souvent pour corriger des erreurs, mettre à jour des informations ou manipuler des données.

## How to:
Elm utilise des fonctions comme `String.replace` pour chercher et remplacer du texte. Voici un petit exemple :

```Elm
import String exposing (replace)

main =
    let
        originalText = "Bonjour, je m'appelle Elm!"
        newText = 
            originalText
                |> replace "Elm" "Programming"
    in
    newText
```

À l'exécution, vous obtiendrez : "Bonjour, je m'appelle Programming!"

## Deep Dive
Chercher et remplacer du texte n'est pas unique à Elm; c’est une fonctionnalité commune dans beaucoup de langages de programmation. Implementé pour la première fois dans les éditeurs de texte des années 70, cette fonction est devenue cruciale pour le traitement de texte automatisé.

En Elm, `String.replace` est limité: il ne supporte pas les expressions régulières comme d'autres langages. Pour des recherches plus complexes, il faudrait utiliser `Regex.replace` du module `Regex`. C’est plus puissant mais aussi plus complexe.

L'implémentation dans Elm vise la simplicité et la fiabilité, s'intégrant bien dans l'écosystème fonctionnel du langage.

## See Also
- Documentation Elm pour `String`: https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Documentation Elm pour `Regex`: https://package.elm-lang.org/packages/elm/regex/latest/Regex#replace
- Tutoriel sur les expressions régulières en Elm: https://elmprogramming.com/regex.html
