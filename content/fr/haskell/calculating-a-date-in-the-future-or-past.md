---
title:                "Haskell: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Haskell, vous savez déjà à quel point il peut être utile de calculer une date dans le futur ou le passé. Que ce soit pour effectuer des tâches planifiées ou pour créer des fonctions de gestion du temps, cette compétence est essentielle pour tout projet Haskell. Dans cet article, nous allons voir comment le faire efficacement grâce à des exemples concrets en Haskell.

## Comment faire

Nous allons utiliser la fonction `addDays` de la librairie `Data.Time` pour réaliser nos calculs. Cette fonction prend en argument un nombre de jours à ajouter ou à soustraire et une date de référence. Ensuite, elle nous retourne la date correspondant à ce calcul.

```
import Data.Time

-- Calculer une date dans le passé
let datePassee = addDays (-30) (fromGregorian 2020 1 1) -- résulat: 1er décembre 2019

-- Calculer une date dans le futur
let dateFutur = addDays 15 (fromGregorian 2021 5 6) -- résulat: 21 mai 2021

-- Calculer une date à partir d'une date de référence inhabituelle
let dateParticuliere = addDays (3*2+7) (fromGregorian 1999 12 31) -- résultat: 13 février 2000
```

Dans ces exemples, nous avons utilisé la fonction `fromGregorian` pour créer nos dates de référence. Mais vous pouvez également utiliser `fromGregorianValid` qui renvoie `Maybe UTCTime` si la date est valide, ou `Nothing` si elle ne l'est pas. De plus, la fonction `addDays` peut être utilisée avec n'importe quelle instance de `Day` ou `UniversalTime`.

## Plongée en profondeur

Si vous souhaitez aller plus loin, vous pouvez également utiliser la fonction `addDays` pour réaliser des calculs plus complexes en utilisant des `Int` ou des `Integer` avec des `Day` ou des `NominalDiffTime`. De plus, il est possible de créer vos propres instances pour les types de données qui n'ont pas encore de fonctions `addDays` prédéfinies en utilisant `instance (Num a) => Num (Day)` ou `instance (Num a) => Num (UTCTime)`.

Enfin, vous pouvez également utiliser la librairie `Data.Time.Calendar` pour accéder à d'autres fonctions utiles pour le calcul de dates, telles que `gregorianMonthLength` pour vérifier le nombre de jours dans un mois.

## Voir aussi

- [Documentation de la librairie Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Documentation de la librairie Data.Time.Calendar](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html)
- [Tutoriel sur la gestion du temps en Haskell](https://wiki.haskell.org/Learning_Haskell/Time_Libraries)