---
title:                "Analyser une date depuis une chaîne de caractères"
aliases: - /fr/elm/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:59.988722-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyser une date depuis une chaîne de caractères"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'analyse d'une date à partir d'une chaîne de caractères en Elm consiste à convertir des informations textuelles représentant des dates et des heures en un format qu'Elm peut comprendre et manipuler, spécifiquement en le type `Date`. Ce processus est essentiel pour gérer l'entrée utilisateur, afficher correctement les dates localisées et effectuer des calculs liés aux dates, garantissant que vos applications Elm peuvent traiter intelligemment les données temporelles.

## Comment faire :
Elm n'a pas de capacités intégrées aussi robustes que certains autres langages pour l'analyse des dates, s'appuyant principalement sur l'interop Javascript ou les bibliothèques pour des opérations plus complexes. Cependant, vous pouvez utiliser le paquet `elm/time` pour une analyse basique, et pour des besoins plus complexes, la bibliothèque tierce `justinmimbs/date` est largement recommandée.

### Analyse en utilisant `elm/time` :
`elm/time` fournit le module `Time`, qui vous permet de travailler avec des horodatages au lieu de dates lisibles par l'homme. Bien qu'il ne parse pas directement les dates à partir de chaînes de caractères, vous pouvez convertir une chaîne au format ISO 8601 en un horodatage POSIX, avec lequel vous pouvez ensuite travailler.

```elm
import Time exposing (Posix)

-- En supposant que vous avez une chaîne de date ISO 8601
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- La convertir en un horodatage POSIX (cette fonction retourne un `Result`)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- Exemple de sortie : Ok <valeur du temps posix>
```

### Analyse en utilisant `justinmimbs/date` :
Pour une analyse plus complexe, comme traiter avec des formats non-ISO, la bibliothèque `justinmimbs/date` est un excellent choix. Voici comment vous pouvez l'utiliser pour parser une chaîne de date personnalisée :

1. Assurez-vous d'avoir installé la bibliothèque :

```shell
elm install justinmimbs/date
```

2. Utilisez la fonction `Date.fromString` pour parser des formats de date personnalisés :

```elm
import Date
import Result exposing (Result(..))

-- Disons que vous avez un format de chaîne de date personnalisé `dd-MM-yyyy`
customDateStr : String
customDateStr = "01-01-2023"

-- Fonction pour parser le format personnalisé
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- Exemple d'utilisation
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- Exemple de sortie : Ok (Date.fromCalendarDate 2023 Jan 1)
```

Dans ces exemples, le type `Result` encapsule soit un parsing réussi qui produit une date (`Ok`), soit une erreur (`Err`), permettant une gestion robuste des erreurs dans vos applications Elm.
