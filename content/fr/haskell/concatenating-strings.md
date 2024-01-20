---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concaténation de chaînes en Haskell : un guide décontracté

## Quoi & Pourquoi ?

La concaténation de chaînes est l'action de joindre deux chaînes ou plus en une seule. Les programmeurs le font pour manipuler et utiliser efficacement des données textuelles.

## Comment Faire:

La fonction `(++)` en Haskell est utilisée pour concaténer des chaînes. Voici un exemple simple:

```Haskell
let phrase1 = "Quelle"
let phrase2 = "belle journée!"
let phraseComplete = phrase1 ++ " " ++ phrase2
```

L'exécution de ce code donnera en sortie:

```Haskell
"Quelle belle journée!"
```

## Exploration Profonde

Historiquement, Haskell, sorti en 1990, a toujours porté un grand intérêt à la manipulation des chaînes. L’opérateur `(++)` provient donc de la première version de Haskell.

Alternativement, vous pouvez utiliser la fonction `concat` pour joindre une liste de chaînes en une seule chaîne.

```Haskell
let phrases = ["Quelle", "belle", "journée!"]
let phraseComplete = concat phrases
```

La sortie de ce code sera:

```Haskell
"Quellebellejournée!"
```

Notez que contrairement à `(++)`, `concat` ne place pas de séparateur entre les chaînes. L’opérateur `(++)` et la fonction `concat` font tous deux une copie complète de leurs arguments. Ainsi, leur utilisation peut être coûteuse en termes de mémoire pour les grandes chaînes.

## À Voir Aussi

- Documentation officielle sur la concaténation de chaînes en Haskell : http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:-43--43-

- Un guide détaillé sur les chaînes de caractères en Haskell : http://learnyouahaskell.com/starting-out#an-intro-to-lists.