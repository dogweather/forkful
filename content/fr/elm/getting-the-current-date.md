---
title:                "Obtenir la date actuelle"
html_title:           "Elm: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Avez-vous déjà eu besoin de connaître la date et l'heure actuelle dans vos applications Elm ? Il existe plusieurs raisons pour lesquelles vous pourriez avoir besoin de récupérer la date actuelle. Peut-être que vous voulez afficher la date sur une page de votre site ou peut-être que vous voulez utiliser la date pour effectuer des calculs dans votre application. Quelle que soit la raison, récupérer la date actuelle peut être un moyen utile pour améliorer vos applications !

## Comment faire

Pour obtenir la date actuelle dans Elm, vous pouvez utiliser la fonction `Time.now` dans le module `Time`. Cette fonction retourne un `Task` avec la date et l'heure actuelles. Voici un exemple de code pour récupérer la date actuelle et l'afficher dans votre navigateur :

```Elm
import Time exposing (Time)
import Task exposing (Task)
import Html exposing (text)

dateActuelle : Task x Time
dateActuelle = Time.now

view : Time -> Html msg
view time =
    text (Time.toString time)

main =
    dateActuelle
        |> Task.perform (\_ -> task (\_ -> "Erreur lors de la récupération de la date")) (\t -> view t)
```

Lorsque vous exécutez ce code, vous devriez voir la date actuelle s'afficher dans votre navigateur ! Vous pouvez également utiliser la fonction `Time.toIsoString` pour formater la date dans le format ISO standard.

## Plongée en profondeur

Si vous voulez en savoir plus sur la récupération de la date actuelle dans Elm, il existe d'autres fonctions utiles dans le module `Time`. Par exemple, la fonction `Time.hoursSinceEpoch` retourne le nombre d'heures depuis le premier janvier 1970. Vous pouvez également utiliser des fonctions de conversion pour transformer la date en d'autres unités de temps, telles que les secondes ou les jours.

## Voir aussi

- La documentation sur le module `Time` : https://package.elm-lang.org/packages/elm/time/latest/
- Un article utile sur l'utilisation de la date et de l'heure dans Elm : https://thoughtbot.com/blog/exploring-time-and-date-in-elm