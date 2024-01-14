---
title:                "Elm: Transformation d'une date en une chaîne de caractères"
simple_title:         "Transformation d'une date en une chaîne de caractères"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Connaître la manière de convertir une date en chaîne de caractères peut être un outil très utile dans la programmation Elm. Cela peut vous aider à afficher des dates dans un format spécifique ou à les manipuler pour des calculs complexes.

# Comment faire

Pour convertir une date en chaîne de caractères en Elm, nous devrons utiliser la fonction `Date.toString` et lui passer la date que nous souhaitons convertir en argument. Voici un exemple de code avec une date spécifiée en tant qu'objet `Date` :

```Elm
import Date exposing (Date, Day, Month, Year)
import String

date = Date.fromCalendarDate 2021 10 28 -- crée une date avec l'année, le mois et le jour spécifiés 
stringDate = Date.toString "dd MMMM yyyy" date -- convertit la date en une chaîne de caractères au format jour/mois/année en lettres 

String.uppercase stringDate -- affiche la date en majuscules : "28 OCTOBRE 2021"
```

Nous pouvons spécifier différents formats de date en utilisant des codes de format spécifiques :

- `"dd"` : jour avec 2 chiffres
- `"dd"` : jour avec 2 chiffres (01, 02, 03...)
- `"d"` : jour avec 1 chiffre (1, 2, 3...)
- `"DDD"` : jour avec 3 lettres (Mon, Tue, Wed...)
- `"DDDD"` : jour avec 1 lettre (M, T, W...)
- `"MMM"` : mois avec 3 lettres (Jan, Feb, Mar...)
- `"MMMM"` : mois entier en lettres (January, February, March...)
- `"yy"` : année avec 2 chiffres (21, 22, 23...)
- `"yyyy"` : année avec 4 chiffres (2021, 2022, 2023...)

Vous pouvez également combiner ces codes de format pour obtenir le format souhaité. Par exemple, `"dd MMM yyyy"` affichera la date au format "28 Oct 2021".

# Découverte en profondeur

En utilisant la fonction `Date.toString`, nous pouvons également spécifier une timezone pour notre date, en utilisant `Date.withZone` en tant qu'argument. Cela peut être utile lorsque vous travaillez avec des données provenant de différentes régions ou fuseaux horaires.

Nous pouvons également utiliser la fonction `Date.fromTime` pour convertir une date à une certaine heure, en utilisant l'objet `Time` en tant qu'argument. Cela peut être utile pour créer des fonctionnalités telles qu'un calendrier avec des rappels.

# Voir aussi

- [Documentation officielle Elm sur Date](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Convertir une chaîne de caractères en date en Elm](https://elmprogramming.com/string-to-date-in-elm.html)
- [Manipuler des dates en Elm : un tutoriel](https://medium.com/@artisteviet/elm-manipuler-les-dates-dans-elm-90e2d9e220ef)