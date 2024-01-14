---
title:                "Haskell: Conversion d'une date en chaîne de caractères"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous travaillez sur un projet de programmation en Haskell, il est possible que vous rencontriez un jour le besoin de convertir une date en une chaîne de caractères compréhensible par l'utilisateur. Cela peut sembler être une tâche simple, mais cela peut en réalité nécessiter un peu de réflexion et de connaissances spécifiques. Dans cet article, nous allons explorer pourquoi et comment convertir une date en chaîne de caractères en utilisant Haskell.

## Comment faire

Tout d'abord, nous avons besoin d'une bibliothèque appelée "time" pour travailler avec les dates en Haskell. Si vous utilisez Stack, vous pouvez l'ajouter à votre fichier cabal en tapant :

```Haskell
build-depends: time
```

Ensuite, nous allons définir une date en utilisant les types de données fournis par la bibliothèque. Par exemple, nous allons définir une date au format yyyy / mm / dd avec le code suivant :

```Haskell
import Data.Time

date :: Day
date = fromGregorian 2020 12 31
```

Maintenant que notre date est définie, nous pouvons utiliser la fonction "formatTime" pour convertir notre date en une chaîne de caractères. Nous pouvons spécifier le format souhaité en utilisant des balises de formatage telles que "%Y" pour l'année, "%m" pour le mois et "%d" pour le jour. Un exemple de code serait :

```Haskell
dateString :: String
dateString = formatTime defaultTimeLocale "%Y/%m/%d" date
```

Nous pouvons maintenant imprimer notre chaîne de caractères en utilisant une simple commande "print" :

```Haskell
>> print dateString
"2020/12/31"
```

Vous pouvez également expérimenter avec différents formats et ajouter des fonctions telles que "show" pour convertir le résultat en une chaîne de caractères plus lisible.

## Approfondissement

La bibliothèque "time" contient plusieurs autres fonctions et types de données pour travailler avec des dates en Haskell. Par exemple, les types "TimeOfDay" et "TimeZone" peuvent être utilisés pour ajouter des informations supplémentaires, telles que l'heure et le fuseau horaire, à notre date.

Il existe également des fonctions de conversion entre différents types de dates, telles que "unixEpochTime" pour convertir une date en temps UNIX et "toGregorian" pour convertir une date en une représentation de l'ère grégorienne.

Il est important de noter que les dates en Haskell sont immuables et que les fonctions de manipulation de date renvoient toujours une nouvelle date au lieu de modifier la date existante.

# See Also

Voici quelques liens utiles pour continuer à explorer les dates en Haskell :

- [Documentation de la bibliothèque "time"](https://hackage.haskell.org/package/time)
- [Guide de référence de l'utilisation des dates en Haskell](https://wiki.haskell.org/U