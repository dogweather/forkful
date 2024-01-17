---
title:                "Obtenir la date actuelle"
html_title:           "Haskell: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Obtenir la date actuelle peut sembler être une tâche simple, mais pour les programmeurs, cela peut être un défi intéressant. En termes simples, obtenir la date actuelle signifie récupérer la date et l'heure actuelles du système de l'ordinateur. Les programmeurs le font pour une variété de raisons, notamment pour suivre le temps, enregistrer l'heure à laquelle une tâche a été effectuée ou pour afficher la date dans une interface utilisateur.

## Comment faire:

Voici un exemple simple de code Haskell pour obtenir la date actuelle et l'afficher dans le format "AAAA-MM-JJ HH:MM:SS":

```
import Data.Time.Clock
import Data.Time.Format

main = do
    time <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
    putStrLn formattedTime
```

Voici la sortie que vous devriez obtenir en exécutant le code ci-dessus:

```
2020-06-14 11:05:23
```

## Plongez en profondeur:

Historiquement, la façon de récupérer la date actuelle variait selon le système d'exploitation et le langage de programmation utilisés. Mais avec l'avènement de Haskell, il existe désormais une méthode standard pour le faire grâce au module "Data.Time". Cependant, il existe toujours d'autres alternatives, telles que l'utilisation de bibliothèques tierces ou l'accès aux programmes système via des appels de système.

En termes d'implémentation, le module "Data.Time" utilise une structure de données appelée UTCTime, qui contient les informations sur la date et l'heure, ainsi qu'une fonction getCurrentTime qui renvoie cet objet. Ensuite, la fonction formatTime est utilisée pour convertir cette structure en une chaîne de caractères dans le format souhaité.

## Voir aussi:

Pour en savoir plus sur la manière d'obtenir la date actuelle en Haskell, voici quelques liens utiles:

- [Documentation du module Data.Time de Haskell](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Exemples de code pour obtenir la date actuelle en Haskell](https://wiki.haskell.org/Date_and_time)
- [Appels système en Haskell](https://hackage.haskell.org/package/unix/docs/System-Posix-Syscall.html)