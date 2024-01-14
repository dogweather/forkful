---
title:                "Haskell: Transformer une date en chaîne de caractères"
simple_title:         "Transformer une date en chaîne de caractères"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Convertissez vos dates en chaînes de caractères en Haskell pour une meilleure gestion de vos données et une manipulation plus facile des dates.

## Comment faire

```Haskell
-- Importer le module Data.Time
import Data.Time

-- Définir la date à convertir
let date = fromGregorian 2020 9 30

-- Utiliser la fonction "show" pour convertir la date en chaîne de caractères
show date

-- Exemple de sortie : "2020-09-30"
```

## Plongée en profondeur

Lorsque vous convertissez une date enchaîne de caractères en Haskell, vous utilisez en fait le type de données "UTCTime". Ce type de données représente un point dans le temps, avec une précision d'une picoseconde. L'utilisation de la fonction "show" permet de convertir ce type de données en chaîne de caractères selon le format ISO 8601.

Cependant, il est également possible de personnaliser le format de sortie en utilisant la fonction "formatTime" du module Data.Time. Par exemple, pour obtenir une sortie sous la forme "30 septembre 2020", on peut utiliser le code suivant :

```Haskell
formatTime defaultTimeLocale "%d %B %Y" date
-- Sortie : "30 septembre 2020"
```

## Voir aussi

- [Documentation officielle de Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Article sur la gestion des dates en Haskell](https://blog.pusher.com/handling-time-and-timezones-in-haskell/)
- [Tutoriel sur l'utilisation de la librairie time en Haskell](https://www.datahaskell.org/new-library-preview-time/)