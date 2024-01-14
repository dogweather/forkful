---
title:    "Elm: Obtenir la date actuelle"
keywords: ["Elm"]
---

{{< edit_this_page >}}

### Pourquoi 

Dans tout projet de programmation, il est souvent nécessaire d'obtenir la date actuelle. Que ce soit pour afficher la date à l'utilisateur, gérer des évènements en temps réel ou pour enregistrer la date de création d'un document, l'utilisation de la date est incontournable. Dans cet article, nous allons découvrir comment obtenir la date actuelle en utilisant le langage Elm.

### Comment faire

Pour obtenir la date actuelle en Elm, nous allons utiliser la fonction `Time.now`. Cette fonction renvoie un `Task` contenant un objet `Posix`. Voici un exemple de code pour afficher la date actuelle dans la console :

```
import Time exposing (now)

main =
    now
        |> Task.perform (\posix -> Date.fromPosix posix |> Debug.log "Date actuelle")
```

Voici la sortie que nous obtenons dans la console :

```
Date actuelle:
    2020-09-25T14:17:48Z
```

Comme vous pouvez le constater, la date est au format ISO 8601. Si vous voulez modifier le format de la date, vous pouvez utiliser la fonction `Date.fromPosix`. Voici un exemple pour afficher la date au format DD-MM-YYYY :

```
import Time exposing (now)
import Date exposing (fromPosix, Date)
import Html exposing (div, text)

main =
    now
        |> Task.perform (\posix -> fromPosix posix |> dateToString |> div [] >> text)

dateToString : Date -> String
dateToString date =
    let
        day =
            toString date.day

        month =
            String.padLeft 2 '0' (toString date.month)

        year =
            toString date.year
    in
    String.concat "-" [ day, month, year ]
```

Et voici la sortie correspondante :

```
25-09-2020
```

### Plongée en profondeur

La fonction `Time.now` utilise l'API JavaScript `Date.now()` pour obtenir la date actuelle en millisecondes. Ensuite, elle utilise la fonction `Time.fromPosix` pour convertir ces millisecondes en un objet `Posix`, qui représente un instant précis dans le temps en utilisant l'échelle temps Unix (nombre de secondes écoulées depuis le 1er janvier 1970 à minuit UTC).

Si vous avez besoin de manipuler la date de manière plus complexe, vous pouvez utiliser le module `Date` qui expose de nombreuses fonctions pour travailler avec les dates en Elm.

### Voir aussi 

- [Documentation officielle Elm pour la gestion des dates et heures](https://package.elm-lang.org/packages/elm/time/latest)
- [Documentation officielle JavaScript pour la fonction `Date.now()`](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date/now)