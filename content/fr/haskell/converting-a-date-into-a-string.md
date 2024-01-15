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

## Pourquoi
Si vous travaillez avec des dates dans votre programme Haskell, il peut être utile de les convertir en chaînes de caractères. Gardez à l'esprit que le type de données de date peut varier d'un système à l'autre, donc la conversion en chaîne peut vous aider à obtenir un format standardisé.

## Comment faire
Pour convertir une date en chaîne de caractères en Haskell, vous pouvez utiliser la fonction ```show```. Voici un exemple de code :

```Haskell
import Data.Time.Calendar
import Data.Time.Format
import System.Locale

date = fromGregorian 2020 05 01
stringDate = formatTime defaultTimeLocale "%B %e, %Y" date

main = putStrLn stringDate
```

Dans cet exemple, nous importons les modules ```Data.Time.Calendar``` et ```Data.Time.Format``` pour avoir accès aux fonctions liées aux dates, ainsi que le module ```System.Locale``` pour spécifier le format de date que nous voulons utiliser. Nous créons ensuite une date avec la fonction ```fromGregorian``` en spécifiant l'année, le mois et le jour, puis nous utilisons la fonction ```formatTime``` pour convertir la date en chaîne de caractères avec le format ```"%B %e, %Y"```. Enfin, nous utilisons la fonction ```putStrLn``` pour afficher la chaîne de caractères dans la console.

La sortie de ce code sera "May 1, 2020", mais vous pouvez personnaliser le format en fonction de vos besoins. N'hésitez pas à explorer d'autres fonctions et formats disponibles dans les modules que nous avons importés.

## Plongée en profondeur
En utilisant la fonction ```formatTime```, vous pouvez également spécifier une timezone pour votre date et même fusionner plusieurs composants de la date (comme le jour de la semaine et le jour du mois) dans une seule chaîne de caractères. Continuez à explorer et à expérimenter pour trouver la meilleure façon de convertir vos dates en chaînes de caractères en Haskell.

## Voir aussi
- [Documentation officielle de GHC - Gestion des dates et heures en Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/time-1.9.3/index.html)
- [Guide complet sur les dates en Haskell](https://dev.to/simonh1000/haskell-dates-57lp)