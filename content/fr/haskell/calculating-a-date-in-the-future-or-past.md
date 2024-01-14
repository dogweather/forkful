---
title:                "Haskell: Calcul d'une date dans le futur ou le passé."
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

Dans la programmation, il est souvent nécessaire de calculer une date dans le futur ou dans le passé afin d'effectuer des tâches spécifiques, telles que la planification d'événements ou la gestion de rappels. Heureusement, en utilisant Haskell, cela peut être réalisé facilement et de manière élégante.

# Comment faire

Pour calculer une date dans le futur ou dans le passé en Haskell, il faut d'abord utiliser le type de données `UTCTime` du module `Data.Time`. Ce type de données représente un point dans le temps sous forme de nombre de secondes écoulées depuis le 1er janvier 1970. En utilisant ce type de données, il est possible de manipuler facilement les dates en utilisant les fonctions du module `Data.Time.Calendar`.

Voici un exemple de code qui calcule la date dans deux semaines à partir d'aujourd'hui :

```Haskell
import Data.Time

main = do
  let startDate = getCurrentTime
  let futureDate = addUTCTime (7*24*60*60*2) startDate
  print futureDate
```

Ce code utilise la fonction `getCurrentTime` pour obtenir la date actuelle et utilise la fonction `addUTCTime` pour ajouter 2 semaines (14 jours) à la date actuelle. Le résultat sera une nouvelle date représentée par le type `UTCTime`.

Un autre exemple pourrait consister à calculer la date d'hier en utilisant la fonction `addDays` :

```Haskell
import Data.Time

main = do
  let startDate = getCurrentTime
  let pastDate = addDays (-1) startDate
  print pastDate
```

Dans ce cas, nous utilisons la fonction `addDays` avec un argument négatif pour soustraire un jour à la date actuelle et obtenir la date d'hier.

# Plongée en profondeur

Il est également possible de calculer une date dans le futur ou dans le passé en utilisant des unités de temps plus petites, telles que les heures ou les minutes. Pour ce faire, il suffit d'utiliser une combinaison de plusieurs fonctions telles que `addUTCTime`, `addSeconds`, `addMinutes`, etc.

Il est également important de noter que toutes les fonctions de manipulation de temps en Haskell utilisent le type de données `UTCTime`, qui représente le temps universel coordonné (UTC). Cela signifie qu'il n'y a pas de prise en compte automatique des fuseaux horaires, il est donc important de prendre cela en compte lors du calcul d'une date pour un fuseau horaire spécifique.

# Voir aussi

- [Documentation Haskell sur les dates et le temps](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Article sur la manipulation de dates en Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Safe-Concurrent-Time-and-Data-Time)
- [Exemple de code pour calculer la différence entre deux dates en Haskell](https://gist.github.com/khandelwal/master/dbb689f9545d33b1f0837bd9f2d58271)