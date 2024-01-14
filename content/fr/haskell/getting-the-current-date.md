---
title:    "Haskell: Obtenir la date actuelle"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes dans la programmation est de récupérer la date actuelle. Que ce soit pour afficher la date dans une application ou pour effectuer des opérations sur la date, il est important de savoir comment récupérer la date actuelle dans votre code Haskell.

## Comment faire

Heureusement, Haskell dispose d'une fonction intégrée pour récupérer la date actuelle. Il s'agit de la fonction "getCurrentTime" du module "Data.Time". Voici un exemple de code pour obtenir la date actuelle :

```Haskell
import Data.Time

main = do 
    dateActuelle <- getCurrentTime
    print dateActuelle
```

Le code ci-dessus importe le module "Data.Time" pour accéder à la fonction "getCurrentTime". Ensuite, dans la fonction principale "main", nous utilisons la notation "do" pour exécuter une séquence d'instructions. Nous utilisons la fonction "getCurrentTime" et stockons le résultat dans la variable "dateActuelle". Enfin, nous utilisons la fonction "print" pour afficher la date actuelle à l'écran.

Lorsque vous exécutez ce code, vous devriez obtenir un résultat similaire à ceci :

```Haskell
2021-10-05 19:32:47.392445 UTC
```

La date est au format ISO 8601 et inclut le fuseau horaire UTC. Si vous souhaitez formater la date différemment, vous pouvez utiliser la fonction "formatTime" du même module. Par exemple, si vous souhaitez afficher la date dans le format "DD/MM/YYYY", vous pouvez utiliser le code suivant :

```Haskell
import Data.Time
import System.Locale (defaultTimeLocale)

main = do 
    dateActuelle <- getCurrentTime
    let dateFormatee = formatTime defaultTimeLocale "%d/%m/%Y" dateActuelle
    print dateFormatee
```

Le résultat sera alors le suivant :

```Haskell
05/10/2021
```

## Deep Dive

La fonction "getCurrentTime" utilise le type de données "UTCTime" pour représenter la date et l'heure. Il s'agit d'un type complexe qui se compose de plusieurs composants tels que l'année, le mois, le jour, l'heure, les minutes, etc. Vous pouvez plonger plus profondément dans ce type de données en consultant la documentation officielle de Haskell.

## Voir aussi

- [Documentation officielle de Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskell Date and Time Tutorial](https://www.tutorialspoint.com/haskell/haskell_date_time.htm)
- [Type complexe UTCTime](https://hoogle.haskell.org/package/time-1.9/docs/Data-Time-Format.html#t:UTCTime)