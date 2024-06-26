---
date: 2024-01-26 00:54:00.536647-07:00
description: "Comment faire : Haskell g\xE8re les erreurs de mani\xE8re robuste gr\xE2\
  ce \xE0 des types comme `Maybe` et `Either`. Voici un aper\xE7u rapide ."
lastmod: '2024-03-13T22:44:57.841616-06:00'
model: gpt-4-1106-preview
summary: "Haskell g\xE8re les erreurs de mani\xE8re robuste gr\xE2ce \xE0 des types\
  \ comme `Maybe` et `Either`."
title: Gestion des erreurs
weight: 16
---

## Comment faire :
Haskell gère les erreurs de manière robuste grâce à des types comme `Maybe` et `Either`. Voici un aperçu rapide :

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Diviser par zéro est interdit, donc on retourne Nothing.
safeDivide x y = Just (x `div` y)  -- Autrement, tout va bien, on retourne le résultat dans un Just.

-- Voyons-le en action :
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

Pour une gestion d'erreurs plus complexe, `Either` entre en jeu :

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Erreur de division par zéro."  -- Cette fois, l'erreur comporte un message.
safeDivideEither x y = Right (x `div` y)

-- Et à l'utilisation :
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Erreur de division par zéro."
```

## Plongée Profonde
Dans le monde de Haskell, la gestion des erreurs a une longue histoire. Autrefois, des erreurs pouvaient faire s'écrouler tout votre programme—pas amusant. Le système de types de Haskell offre des moyens de rendre cela beaucoup moins probable. Nous avons `Maybe` et `Either`, mais il existe d'autres solutions comme `Exceptions` et `IO` pour différents scénarios.

`Maybe` est simple : on obtient `Just` quelque chose si tout va bien, ou `Nothing` si ce n'est pas le cas. `Either` monte d'un cran, vous permettant de retourner un message d'erreur (`Left`) ou un résultat réussi (`Right`).

Les deux sont purs, cela signifie qu'ils ne perturbent pas le monde extérieur – un gros avantage dans Haskell. On évite les pièges des exceptions non contrôlées qui affligent certains autres langages.

Pour ceux qui ne se contentent pas de `Maybe` et `Either`, des bibliothèques comme `Control.Exception` offrent une gestion des erreurs plus traditionnelle et impérative à travers des exceptions. Mais leur utilisation trop libérale peut compliquer les choses, donc la communauté s'en tient souvent aux types.

## Voir Aussi
Approfondissez avec :

- La documentation de Haskell elle-même : [Haskell](https://haskell.org/documentation)
- Idéal pour les débutants : ["Apprends toi un Haskell pour le Meilleur!"](http://learnyouahaskell.com/)
