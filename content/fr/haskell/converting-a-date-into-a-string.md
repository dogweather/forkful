---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Haskell: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertir une date en chaîne de caractères en Haskell

## Quoi & pourquoi ?
La conversion d'une date en chaîne de caractères est le processus de transformation d'une date (sous la forme d'un type de données) en une séquence de caractères. Les programmeurs le font généralement pour afficher la date dans un format lisible pour les utilisateurs ou pour traiter des dates avec d'autres fonctions.

## Comment faire :
Pour convertir une date en chaîne de caractères en Haskell, vous pouvez utiliser la fonction `show` intégrée. Voici un exemple de code pour convertir une date dans le format "JJ/MM/AAAA":
```Haskell
import Data.Time.Format

dateEnChaine :: Day -> String
dateEnChaine date = formatTime defaultTimeLocale "%d/%m/%Y" date
```
Lorsque vous exécutez ce code avec une date donnée, vous obtiendrez une chaîne de caractères représentant cette date. Par exemple, la date `18/09/2021` sera convertie en `"18/09/2021"`.

## Plongée en profondeur :
Dans le passé, la conversion de dates en chaînes de caractères était souvent réalisée à l'aide de fonctions personnalisées. Mais avec l'ajout de la fonction `show` dans les bibliothèques standard de Haskell, le processus est devenu beaucoup plus facile et plus fiable.

Il existe également d'autres alternatives pour convertir des dates en chaînes de caractères en Haskell, comme la bibliothèque `time-format`, qui offre plus de flexibilité pour formater les dates.

L'implémentation de la fonction `show` utilise une structure de données appelée `TimeLocale` pour déterminer le format de la date en fonction de la localisation de l'utilisateur. Cela permet à la fonction de fonctionner dans différentes régions du monde sans avoir besoin de modifier le code.

## Voir aussi :
- [Documentation de la fonction `show` de Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:show)
- [Documentation de la bibliothèque `time-format`](https://hackage.haskell.org/package/time-format)
- [Tutoriel sur les dates en Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple-date-manipulation-in-Haskell)