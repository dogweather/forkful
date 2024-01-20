---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Nous allons parler de "parsing" d'une date à partir d'une chaîne de caractères - c'est la conversion d'une représentation textuelle d'une date en une structure de données propre à Haskell. Pourquoi devrions-nous le faire? Facile. Parce que c'est une façon fondamentale de gérer des données de date souvent stockées ou fournies en format texte.

## Comment Faire:
Nous pouvons analyser une date avec le module `Data.Time`. Voici un petit échantillon :

```Haskell
import Data.Time
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)

prendreDate :: String -> Day
prendreDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

main = print $ prendreDate "2021-11-25"
```
La sortie sera : `2021-11-25`

## En Profondeur:
1. Historique: À l'origine, Haskell avait une gestion des dates très limitée. Avec l'introduction du module `Data.Time` dans GHC 6.8.2 en 2007, une approche plus moderne et flexible a été introduite.
2. Alternatives: Il existe d'autres options pour l'analyse de dates, comme l'utilisation de bibliothèques supplémentaires (ex: `date-parser`).
3. Détails de mise en œuvre: `parseTimeOrError` prend un Booléen pour le respect strict du format, l'objet de format de la date, le format de date lui-même (ici en ISO 8601), et enfin la chaîne de la date à analyser. Si le ‘parsing’ échoue, une exception est lancée!

## Voir Aussi :
1. Documentation de `Data.Time`: https://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time.html
2. GHC 6.8.2 Notes de version: https://downloads.haskell.org/~ghc/6.8.2/docs/html/libraries/base-3.0.0.0/Control-Applicative.html
3. Bibliothèque `date-parser`: https://hackage.haskell.org/package/date-parser
4. Format de date ISO 8601: https://www.iso.org/iso-8601-date-and-time-format.html