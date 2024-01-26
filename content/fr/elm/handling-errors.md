---
title:                "Gestion des erreurs"
date:                  2024-01-26T00:51:45.810407-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestion des erreurs"
programming_language: "Elm"
category:             "Elm"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/handling-errors.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Gérer les erreurs signifie écrire du code qui peut anticiper et traiter les problèmes qui surviennent. Les programmeurs le font pour éviter les plantages, protéger l'intégrité des données et fournir aux utilisateurs des solutions de repli élégantes.

## Comment faire :
La philosophie centrale d'Elm est Pas d'Exceptions à l'Exécution. Donc, Elm tire parti de son système de types avec des types tels que `Maybe` et `Result` pour gérer les erreurs.

Pour le scénario `Maybe` :

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- Lorsque vous l'exécutez :

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

Pour le scénario `Result` :

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- Et en l'utilisant :

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## Plongée approfondie
Le système de types d'Elm est strict, ce qui aide à détecter les erreurs tôt. Historiquement, la plupart des langages se reposaient sur des exceptions et des vérifications à l'exécution, mais Elm a choisi des garanties au moment de la compilation. Des alternatives comme `Result` permettent une information détaillée sur les erreurs, tandis que `Maybe` est plus simple pour les scénarios oui-non. La gestion des erreurs par Elm encourage les développeurs à envisager toutes les voies dès le départ, évitant les pièges des cas d'erreurs oubliés.

## Voir aussi :
- La section du guide officiel d'Elm sur la gestion des erreurs : [Gestion des Erreurs – Une Introduction](https://guide.elm-lang.org/error_handling/)
- Documentation Elm `Maybe` : [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Documentation Elm `Result` : [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)