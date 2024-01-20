---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & Pourquoi ?

La conversion d'une date en une chaîne de caractères consiste à changer la forme d'une date pour une représentation sous forme de texte. Les programmeurs le font pour des raisons de clarté visuelle et de facilité de manipulation.

## Comment faire :
Pour convertir une date en chaîne de caractères en Elm, vous utilisez la bibliothèque `elm/time` et `elm/regex` comme suit:

```Elm
import Time exposing (Posix)
import Regex exposing (Regex, contains, fromString)

dateToString : Posix -> String
dateToString date =
    let
        regex : Regex
        regex =
            fromString "(\\d{4})-(\\d{2})-(\\d{2})"
                |> Maybe.withDefault Regex.never

        dateString : String
        dateString =
            Time.toIsoString date
    in
    if contains regex dateString then
        dateString
    else
        "Invalid date format"
```

Les dates sont converties en ISO String et la nouvelle chaîne de caractères est retournée.

## Plongée en Profondeur 

Historiquement, Elm utilisait la bibliothèque `elm-lang/core` pour la gestion du temps, mais à partir de Elm 0.19, la bibliothèque `elm/time` a été introduite pour une meilleure précision et une prise en charge de la norme ISO 8601.

En alternative, vous pouvez utiliser la bibliothèque `justinmimbs/date` pour gérer les dates en Elm.

Concernant l'implémentation, la fonction `toIsoString` convertit une date Posix en chaîne ISO 8601, et le regex est utilisé pour valider cette chaîne. Si elle correspond, elle est retournée, sinon un message d'erreur est renvoyé.

## Voir Aussi 
Voici des liens vers des ressources associées pour mieux comprendre la manipulation des dates en Elm :

1. Documentation Elm Time : https://package.elm-lang.org/packages/elm/time/latest/
2. Documentation Elm Regex : https://package.elm-lang.org/packages/elm/regex/latest/
3. ISO 8601 Date and Time Format : https://www.w3.org/TR/NOTE-datetime
4. Bibliothèque Github justinmimbs/date : https://github.com/justinmimbs/date