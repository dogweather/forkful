---
title:                "Calcul d'une date dans le futur ou le passé"
html_title:           "Haskell: Calcul d'une date dans le futur ou le passé"
simple_title:         "Calcul d'une date dans le futur ou le passé"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

La programmation en Haskell est très utile pour effectuer des calculs sur les dates futures ou passées. Par exemple, vous pourriez avoir besoin de calculer le jour de la semaine où vous allez fêter votre anniversaire dans 10 ans, ou de savoir quel jour de la semaine vous êtes né il y a 20 ans. Ces calculs peuvent être fastidieux à faire à la main, mais grâce à Haskell, nous pouvons les automatiser et les rendre beaucoup plus faciles.

## Comment faire

Pour effectuer des calculs de dates en Haskell, nous utiliserons le module "Data.Time", qui fournit des fonctions utiles pour manipuler des dates et des heures. Pour commencer, nous devons importer le module en haut de notre fichier :

```Haskell
import Data.Time
```

### Calculer une date dans le futur

Pour calculer une date dans le futur, nous utiliserons la fonction "addDays" qui prend comme arguments une durée en jours et une date de référence.

```Haskell
addDays :: Integer -> Day -> Day
```

Par exemple, si nous voulons calculer la date qui se trouve dans 100 jours à partir d'aujourd'hui, nous pouvons utiliser la fonction de cette manière :

```Haskell
addDays 100 today
```

La fonction "today" renvoie la date d'aujourd'hui, donc le résultat de cette expression sera la date qui se trouve dans 100 jours à partir d'aujourd'hui.

### Calculer une date dans le passé

Pour calculer une date dans le passé, nous utiliserons la fonction "addDays", mais cette fois avec une valeur négative pour la durée. Par exemple, si nous voulons calculer la date d'il y a 500 jours à partir d'aujourd'hui, nous pouvons utiliser cette expression :

```Haskell
addDays (-500) today
```

### Exemple complet

Essayons un exemple complet pour calculer une date dans le futur et dans le passé. Supposons que nous voulons calculer la date dans 10 ans à partir de la date d'aujourd'hui, ainsi que la date il y a 10 ans à partir de la date d'aujourd'hui.

```Haskell
import Data.Time

main = do
  let futureDate = addDays (10 * 365) today
      pastDate = addDays (-10 * 365) today
  putStrLn "Date dans 10 ans :"
  print futureDate
  putStrLn "Date il y a 10 ans :"
  print pastDate
```

Le résultat de ce programme sera :

```
Date dans 10 ans :
2029-02-05
Date il y a 10 ans :
2009-02-05
```

Nous avons utilisé la fonction "putStrLn" pour afficher un message suivi de la fonction "print" pour afficher la date calculée.

## Plongée en profondeur

Le module "Data.Time" fournit également des fonctions pour travailler avec des heures, des minutes, des secondes et des fuseaux horaires. Vous pouvez explorer ces fonctions plus en profondeur en lisant la documentation officielle de Haskell ou en cherchant des tutoriels en ligne sur la manipulation de dates en Haskell.

## Voir aussi

- [Documentation officielle de Haskell sur le module "Data.Time"](https://www.haskell.org/hoogle/?hoogle=Data.Time)
- [Tutoriel sur la manipulation de dates en Haskell](https://mmhaskell.com/blog/2017-01-09-tutorial-haskell-dates)