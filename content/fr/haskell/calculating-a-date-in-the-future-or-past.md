---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Haskell: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Calculer une date dans le futur ou le passé consiste à manipuler les dates pour obtenir une date spécifique. Nous, les programmeurs, faisons ça tout le temps pour de nombreuses raisons, comme pour planifier des événements ou pour suivre le temps écoulé.

## Comment faire :

En Haskell, nous pouvons utiliser la bibliothèque Time pour manipuler les dates. Voici comment vous pouvez le faire:

```Haskell
import Data.Time

laDateDansNDays :: IO Day -> Int -> IO Day
laDateDansNDays date n = do
  date' <- date
  return $ addDays n date'
```

Puis exécutez ceci dans GHCi :

```Haskell
date <- getCurrentDay
laDateDansNDays (return date) 3
```

La fonction `laDateDansNDays` prend une date et un nombre de jours, puis renvoie la date après d'ajouter ce nombre de jours.

## Plus de connaissances :

Historiquement, ce genre de manipulation de date se faisait à la main, avec beaucoup de heurts et de malheurs à cause des erreurs de calcul. En Haskell, le paquet `time` facilite grandement la tâche. Cependant, il existent des alternatives comme `thyme` ou `data-dates`.

L'implémentation de ces bibliothèques est basée sur des calculs calendriques prenants en compte les complexités de notre système, y compris les années bissextiles et les fuseaux horaires. C'est un détail important à garder à l'esprit lors du choix d'une bibliothèque.

## Aussi voir :

1. [Paquet `time` sur Hackage](https://hackage.haskell.org/package/time)
2. [Paquet `thyme` sur Hackage](https://hackage.haskell.org/package/thyme)
3. [Paquet `data-dates` sur Hackage](https://hackage.haskell.org/package/data-dates)
4. [Tutoriel sur la manipulation de la date et heure en Haskell](https://www.schoolofhaskell.com/user/commercial/content/time)

Prenez le temps d'explorer ces liens pour avoir une meilleure compréhension de la manipulation des dates et horaires dans Haskell.