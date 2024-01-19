---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Comparer deux dates, c'est déterminer si une date est antérieure, postérieure ou identique à une autre. Les programmeurs font cela pour répondre à de nombreuses questions, notamment ordonner des événements ou calculer des durées.

## Comment faire :

En Haskell, nous utilisons le type `UTCTime` du module `Data.Time.Clock` pour manipuler les dates. Voici un exemple de comparaison de deux dates :

```Haskell
import Data.Time

main :: IO ()
main = do
  let date1 = UTCTime (fromGregorian 2021 1 1) 0
  let date2 = UTCTime (fromGregorian 2022 1 1) 0

  print (date1 > date2)
  print (date1 <= date2)
```

Dans ce code, `date1` est plus ancienne que `date2`, alors `(date1 > date2)` sera `False` et `(date1 <= date2)` sera `True`.

## Plongeons 

Historiquement, en informatique, des dates ont souvent été comparées en les convertissant en secondes depuis une certaine période (ex: l'Universel Coordonné depuis 1970). Mais cette méthode n'était pas idéale en raison des complications comme les années bissextiles, les fuseaux horaires, etc.

En Haskell, le type `UTCTime` résout ces problèmes. Pour des comparaisons simples, l'opérateur de comparaison (`>`, `<`, `==`, etc.) suffit. Pour les calculs de durée plus complexes, Haskell propose les types `DiffTime` et `NominalDiffTime`.

Des alternatives comme le type `Day` pourraient être utilisées pour des besoins spécifiques, mais elles ne tiennent pas compte de l'heure exacte.

## Voir Aussi :

Pour plus d'informations et des exemples, consultez les liens suivants :

- [Haskell Data.Time](https://hackage.haskell.org/package/time-1.5.0.1/docs/Data-Time.html) : Pour plus d'informations sur le package `time` et ses fonctions.

- [Learn You a Haskell](http://learnyouahaskell.com/) : Pour une introduction générale à Haskell.

- [Real World Haskell](http://book.realworldhaskell.org/) : Pour une vue plus appliquée de Haskell, y compris sur comment manipuler les dates et les heures.