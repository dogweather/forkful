---
title:    "Haskell: Calculer une date dans le futur ou le passé."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a de nombreuses raisons pour lesquelles vous pourriez trouver utile de calculer une date dans le futur ou dans le passé en utilisant Haskell. Peut-être que vous travaillez sur un projet où vous avez besoin de planifier des tâches ou des événements à venir, ou peut-être que vous voulez simplement tester vos compétences de programmation en résolvant un défi de calcul de date. Quelle que soit la raison, il est toujours bon de savoir comment le faire en utilisant Haskell.

## Comment faire

Le calcul de dates en utilisant Haskell est assez facile grâce à quelques fonctions de manipulation de temps intégrées. Pour commencer, importez le module "Data.Time" et utilisez les fonctions "addDays" et "addGregorianYearsClip" pour ajouter ou soustraire des jours ou des années à une date spécifique. Voici un exemple de code :

```Haskell
import Data.Time

date = fromGregorian 2021 10 15
futureDate = addDays 30 date
pastDate = addGregorianYearsClip (-2) date

main = do
    putStrLn $ "La date actuelle est : " ++ show date
    putStrLn $ "La date dans 30 jours sera : " ++ show futureDate
    putStrLn $ "La date il y a 2 ans était : " ++ show pastDate
```

Et voici la sortie du code :

```
La date actuelle est : 2021-10-15
La date dans 30 jours sera : 2021-11-14
La date il y a 2 ans était : 2019-10-15
```

Vous pouvez également utiliser d'autres fonctions telles que "addGregorianMonthsClip" ou "addGregorianHours" en utilisant des valeurs négatives pour calculer des dates dans le passé. N'hésitez pas à jouer avec ces fonctions et à créer votre propre logique de calcul de date en fonction de vos besoins.

## Plongée en profondeur

Si vous voulez aller encore plus loin dans votre exploration de la manipulation de dates en Haskell, il est utile de connaître les types de données utilisés pour représenter les dates et les heures. Haskell utilise les types "UTCTime" et "Day" pour les dates et "TimeOfDay" pour les heures. Vous pouvez également utiliser des types abstraits tels que "UTCTime" et "LocalTime" pour une représentation plus précise des dates et heures.

En outre, vous pouvez également trouver utile d'utiliser le type "DiffTime" pour représenter la différence entre deux heures ou "NominalDiffTime" pour représenter la différence entre deux dates. Ces types peuvent être utilisés avec les fonctions de manipulation de temps pour effectuer des calculs plus complexes.

## Voir aussi

- [Documentation sur la manipulation de dates et de temps en Haskell](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Exemples de calcul de date en Haskell](https://wiki.haskell.org/Date_and_time)
- [Défis de programmation en utilisant la manipulation de dates en Haskell](https://exercism.io/my/tracks/haskell)