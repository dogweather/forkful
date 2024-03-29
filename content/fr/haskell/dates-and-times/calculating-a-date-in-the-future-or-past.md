---
date: 2024-01-20 17:31:04.597615-07:00
description: "Calculer une date dans le futur ou le pass\xE9 permet de d\xE9terminer\
  \ un jour sp\xE9cifique \xE0 partir d'une date donn\xE9e. Les programmeurs le font\
  \ souvent pour des\u2026"
lastmod: '2024-03-13T22:44:57.847814-06:00'
model: gpt-4-1106-preview
summary: "Calculer une date dans le futur ou le pass\xE9 permet de d\xE9terminer un\
  \ jour sp\xE9cifique \xE0 partir d'une date donn\xE9e. Les programmeurs le font\
  \ souvent pour des\u2026"
title: "Calcul d'une date future ou pass\xE9e"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Calculer une date dans le futur ou le passé permet de déterminer un jour spécifique à partir d'une date donnée. Les programmeurs le font souvent pour des tâches de planification, d'archivage, ou quand les applications dépendent de la logique temporelle.

## Comment faire :
```haskell
import Data.Time

-- Ajouter ou soustraire des jours à une date donnée
ajouterJours :: Integer -> IO Day
ajouterJours nbJours = do
  aujourd'hui <- utctDay <$> getCurrentTime
  return $ addDays nbJours aujourd'hui

-- Exemple d'utilisation
main :: IO ()
main = do
  dansUneSemaine <- ajouterJours 7
  ilYaUneSemaine <- ajouterJours (-7)
  putStrLn $ "Dans une semaine, ce sera le " ++ show dansUneSemaine
  putStrLn $ "Il y a une semaine, c'était le " ++ show ilYaUneSemaine

-- Sortie d'exemple :
-- Dans une semaine, ce sera le 2023-04-14
-- Il y a une semaine, c'était le 2023-03-31
```

## Immersion :
Historiquement, la manipulation des dates en programmation a été complexe à cause des formats variés et des fuseaux horaires. Haskell utilise le package `time` pour faciliter ces opérations. Il y a d'autres bibliothèques comme `old-time`, mais `time` est recommandé aujourd'hui. L'implémentation Haskell se distingue par sa robustesse et son approche fonctionnelle, gérant convenablement les abstractions de temps.

Pour calculer une date dans le futur ou le passé, on utilise généralement `addDays`, qui prend un nombre de jours et une date. Cela retourne une nouvelle date. C'est simple et évite beaucoup d'erreurs communes comme oublier les années bissextiles ou les détails des mois variés. Haskell traite tout cela sous le capot.

## Voir Aussi :

- Documentation de `Data.Time` sur Hackage: [http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
