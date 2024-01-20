---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La récupération de la date actuelle est une tâche courante en programmation qui consiste à obtenir et à stocker la date du moment. C'est essentiel pour le suivi des événements, la journalisation et la gestion du temps dans les applications.

## Comment faire :

Pour obtenir la date actuelle en Haskell, le module Data.Time est votre ami. Voici un simple morceau de code pour obtenir la date actuelle :

```Haskell
import Data.Time

main :: IO ()
main = do
    currentDay <- fmap utctDay getCurrentTime
    putStrLn ("La date du jour est: " ++ show currentDay)
```

Lorsque vous exécutez ce code, il affiche la date actuelle sous la forme `aaaa-mm-jj`.

## Plongée en profondeur :

Historiquement, obtenir la date et l'heure actuelles est une tâche basique mais indispensable en programmation, quel que soit le langage que vous utilisez.

En Haskell, on peut aussi utiliser le module System.Time, mais il est obsolète. Data.Time est le module de choix pour gérer le temps et les dates en Haskell.

Il existe d'autres fonctions dans le module Data.Time pour gérer le format de la date et l'heure, comme formatTime, parseTimeM etc.

## A voir aussi :

- Pour un aperçu complet du module Data.Time, consultez la [documentation officielle](http://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time.html)
- Pour une discussion plus approfondie sur le temps et les dates en Haskell, vous pouvez lire l'[article sur School of Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/time).