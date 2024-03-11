---
date: 2024-01-20 17:45:56.302157-07:00
description: "Extraire des sous-cha\xEEnes, c'est comme prendre un biscuit dans la\
  \ bo\xEEte : vous choisissez une partie sp\xE9cifique d'une cha\xEEne de caract\xE8\
  res. Les\u2026"
lastmod: '2024-03-11T00:14:31.770997-06:00'
model: gpt-4-1106-preview
summary: "Extraire des sous-cha\xEEnes, c'est comme prendre un biscuit dans la bo\xEE\
  te : vous choisissez une partie sp\xE9cifique d'une cha\xEEne de caract\xE8res.\
  \ Les\u2026"
title: "Extraction de sous-cha\xEEnes"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Extraire des sous-chaînes, c'est comme prendre un biscuit dans la boîte : vous choisissez une partie spécifique d'une chaîne de caractères. Les programmeurs font ça pour manipuler des données, valider des entrées, ou simplement afficher ce qu'ils veulent.

## Comment faire :
```Haskell
import Data.Text (Text)
import qualified Data.Text as T

-- Pour commencer, installons `text` via Cabal ou Stack.
-- cabal install text
-- ou
-- stack install text

-- Voici comment extraire une sous-chaîne avec Data.Text :

-- Disons que l'on a un Texte :
let texteComplet = "Bonjour, je suis un exemple de texte."

-- On veut extraire "je suis" du texteComplet.
-- Utilisons la fonction `take` et `drop` :
let debut = 9           -- le début de la sous-chaîne
let longueur = 7        -- la longueur de la sous-chaîne
let sousChaine = T.take longueur . T.drop debut $ texteComplet

-- Affichons la sous-chaîne :
sousChaine
```
Sortie attendue:
```
"je suis"
```

## Plongeon Profond
Historiquement, Haskell manipule des chaînes avec des listes, mais `Data.Text` est devenu standard, pour l'efficacité. L'alternative, `Data.ByteString`, est idéale pour les données binaires. Extraire des sous-chaînes est une opération fondamentale en programmation, faisant partie de l'analyse de texte, le parsing, et plus. Haskell gère l'unicode proprement avec `Text`, donc pas de soucis avec différentes langues.

## Voir Aussi:
- [Hackage - package text](https://hackage.haskell.org/package/text)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
