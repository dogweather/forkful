---
title:    "Haskell: Comparaison de deux dates"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Pourquoi

La comparaison de deux dates est une tâche courante dans la programmation, que ce soit pour vérifier la validité d'une entrée utilisateur ou pour trier une liste d'événements. Il est donc important de comprendre comment comparer deux dates en Haskell afin de rendre nos programmes plus efficaces et précis.

# Comment faire

Pour comparer deux dates en Haskell, nous utiliserons la fonction `compare`. Cette fonction prend deux arguments de type `DateTime` et renvoie une valeur de type `Ordering`, qui peut être `GT` (plus grand), `LT` (plus petit) ou `EQ` (égal).

```
haskell
import Data.Time

date1 = fromGregorian 2021 10 20
date2 = fromGregorian 2021 10 25

compare date1 date2  -- renvoie LT
```

Pour une comparaison plus précise, nous pouvons utiliser la fonction `diffDays`, qui prend deux arguments de type `DateTime` et renvoie un `Integer` représentant le nombre de jours entre les deux dates. Si nous voulons comparer des dates avec des heures, minutes et secondes, nous pouvons utiliser la fonction `diffUTCTime`.

```
haskell
import Data.Time

dateTime1 = UTCTime (fromGregorian 2021 10 20) (secondsToDiffTime 5000)
dateTime2 = UTCTime (fromGregorian 2021 10 21) (secondsToDiffTime 10000)

diffUTCTime dateTime1 dateTime2  -- renvoie -86400 (une journée en secondes)
```

# Plongée en profondeur

En Haskell, les dates sont représentées par le type `DateTime` dans le module `Data.Time`. Cette représentation utilise une structure de données appelée "arbre balanceur de jour". Cela permet une manipulation efficace des dates, même pour des calculs complexes ou une gestion de fuseau horaire.

De plus, le module `Data.Default` fournit une fonction `def` qui renvoie la date courante selon le fuseau horaire de l'environnement système. Nous pouvons également utiliser la fonction `getCurrentTime` du module `Data.Time.Format` pour obtenir la date et l'heure actuelles et les convertir en `DateTime`.

# Voir aussi

- [Documentation officielle de Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Guide de référence rapide de Data.Time pour Haskell](https://wiki.haskell.org/Data.Time/QuickReference)
- [Exemples pratiques de manipulation de dates et de temps en Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/date-and-time)