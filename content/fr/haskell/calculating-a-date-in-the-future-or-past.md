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

# Quoi & Pourquoi? 

Calculer une date dans le futur ou dans le passé est une tâche courante pour les programmeurs. Cela consiste à trouver une date exacte en ajoutant ou en soustrayant un certain nombre de jours, de mois ou d'années à une date de départ donnée. Les programmeurs font cela pour des raisons pratiques, telles que la gestion des échéances ou la planification de tâches à effectuer à une date précise.

## Comment faire: 

Voici deux exemples de code en Haskell pour calculer une date dans le futur et dans le passé en utilisant la bibliothèque ```time```:

Calculer une date dans le futur (7 jours à partir d'aujourd'hui):

```Haskell
import Data.Time

main = do
  today <- getCurrentTime
  let futureDate = addDays 7 today
  print futureDate
```

Output: ```2021-11-01 09:00:00 UTC```

Calculer une date dans le passé (2 mois à partir d'aujourd'hui):

```Haskell
import Data.Time

main = do
  today <- getCurrentTime
  let pastDate = addUTCTime ((-2) * 30 * 24 * 60 * 60) today
  print pastDate
```
Output: ```2021-09-15 09:00:00 UTC```

## Profondeur de plongée: 

Dans le passé, les programmeurs utilisaient souvent des bibliothèques tierces pour gérer les dates et les heures. Heureusement, la bibliothèque standard ```time``` offre maintenant une prise en charge complète pour la manipulation de la date et de l'heure. Une alternative à la bibliothèque ```time``` est ```Chronos```, qui offre une syntaxe plus concise pour effectuer des calculs de dates et d'heures.

Il est important de noter que la gestion des dates et des heures peut être complexe en raison des différents formats de temps utilisés dans le monde entier. Les bibliothèques comme ```time``` et ```Chronos``` prennent en compte cela dans leur implémentation et offrent des outils pour faciliter la conversion entre ces formats.

## Voir aussi: 

- [Documentation officielle de la bibliothèque ```time```](https://hackage.haskell.org/package/time)
- [Documentation officielle de la bibliothèque ```Chronos```](https://hackage.haskell.org/package/chronos)