---
title:                "Haskell: Obtenir la date actuelle"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

La manipulation de dates et d'heures est une tâche courante en programmation et peut servir à de nombreuses fins, telles que l'affichage de la date actuelle sur une application ou le calcul du temps écoulé depuis une certaine date. Dans cet article, nous allons explorer comment obtenir la date actuelle en Haskell.

## Comment faire

Pour obtenir la date actuelle en Haskell, nous allons utiliser la bibliothèque `time`, qui contient des fonctions pour travailler avec des dates et des heures. Tout d'abord, nous devons l'importer dans notre programme :

```Haskell
import Data.Time
```

Ensuite, nous pouvons utiliser la fonction `getCurrentTime` pour récupérer la date et l'heure actuelles. Cette fonction renvoie une valeur de type `IO UTCTime`, nous devons donc utiliser l'opérateur `>>=` pour extraire la valeur de la monade `IO` :

```Haskell
getCurrentTime >>= \currentTime -> print currentTime
```

Ce code imprimera la date et l'heure actuelles au format `UTCTime` :

```
2020-05-20 15:30:00 UTC
```

Nous pouvons également utiliser la fonction `utcToLocalTime` pour convertir cette date en un fuseau horaire local :

```Haskell
let utcTime = getCurrentTime
utcToLocalTime utc utcTime >>= \localTime -> print localTime
```

Ce code imprimera la date et l'heure actuelles au format `LocalTime` :

```
2020-05-20 08:30:00
```

## Plongée en profondeur

La fonction `getCurrentTime` utilise le temps système du système d'exploitation pour récupérer la date et l'heure actuelles. Cependant, cela peut causer des problèmes lorsque plusieurs threads tentent d'utiliser cette fonction en même temps, car le temps peut avancer entre le moment où la fonction est appelée et le moment où elle retourne sa valeur.

Pour éviter cela, la bibliothèque `time` fournit également la fonction `getCurrentTime` qui utilise une horloge monotone pour garantir un résultat cohérent, même en cas de concurrence. Cependant, cette fonction n'est pas disponible sur toutes les plateformes, donc il est important de faire attention à la fonction à utiliser en fonction de votre cas d'utilisation.

## Voir aussi

- [Documentation de la bibliothèque `time`](https://hackage.haskell.org/package/time)
- [Guide sur la manipulation de dates en Haskell](https://www.haskell.org/tutorial/datetime.htm)