---
title:                "Extraction de sous-chaînes"
html_title:           "Elm: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

L'extraction de sous-chaînes est une technique couramment utilisée en programmation, qui consiste à extraire une partie spécifique d'une chaîne de caractères. Les programmeurs le font souvent pour traiter des données, effectuer des recherches ou pour faciliter la manipulation de chaînes plus longues.

## Comment le faire:

```Elm
-- Exemple 1: Extraction d'une sous-chaîne à partir d'une position spécifique
substring 2 5 "Bonjour!" -- Sortie: "njour"

-- Exemple 2: Extraction d'une sous-chaîne en utilisant une plage de positions
substring 0 3 "Elm est génial!" -- Sortie: "Elm"

-- Exemple 3: Extraction d'une sous-chaîne en utilisant un motif
substring "est" "Elm est génial!" -- Sortie: "est"
```

## Approfondissement
L'extraction de sous-chaînes a été introduite dans le langage de programmation Elm dans sa version 0.18. Il existe également d'autres méthodes pour extraire des sous-chaînes, telles que l'utilisation de la fonction `String.slice` ou le module `String.Extra` qui offre des fonctions supplémentaires pour manipuler les chaînes de caractères. 

Dans Elm, la méthode `substring` prend en compte la position de début et de fin d'extraction, ainsi que le motif à extraire. Si aucun motif n'est spécifié, il extraira simplement la sous-chaîne en fonction des positions données. Il est également possible d'utiliser des nombres négatifs pour compter à partir de la fin de la chaîne.

## Voir aussi:
- Documentation officielle sur la fonction `substring` en Elm : https://package.elm-lang.org/packages/elm/core/latest/String#substring
- Documentation sur la fonction `slice` en Elm : https://package.elm-lang.org/packages/elm/core/latest/String#slice
- Documentation sur le module `String.Extra` en Elm : https://package.elm-lang.org/packages/elm/core/latest/String-Extra