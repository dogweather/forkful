---
title:    "Elm: Convertir une date en chaîne de caractères"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous programmez en Elm, il est fort probable que vous ayez besoin à un moment donné de convertir une date en une chaîne de caractères. Cela peut sembler un peu intimidant, mais en réalité, c'est une tâche assez simple à accomplir. Dans cet article, nous allons explorer pourquoi vous pourriez avoir besoin de convertir des dates en chaînes et comment le faire efficacement en Elm.

## Comment faire

Il existe plusieurs façons de convertir une date en une chaîne en Elm, mais la méthode la plus simple consiste à utiliser la fonction `Date.toString`. Cette fonction prend une date en tant que premier argument et un format de chaîne en tant que second argument. Voici un exemple de comment utiliser cette fonction :

```Elm
import Date exposing (Day, Month, Year, fromCalendarDate)
import Date.Extra as Date
import Time exposing (Posix)

-- On crée une date avec les valeurs Year, Month et Day
date = fromCalendarDate 2021 10 21

-- On utilise la fonction Date.toString avec un format de chaîne spécifique
stringDate = Date.toString "yyyy-MM-dd" date

-- On affiche la sortie
main = stringDate -- Résultat : "2021-10-21"
```

Comme vous pouvez le voir, nous avons utilisé le format "yyyy-MM-dd", qui donne une chaîne de caractères dans le format année-mois-jour. Mais vous pouvez également utiliser d'autres formats tels que "dd/MM/yyyy" ou "M/d/yyyy". La liste complète des formats disponibles peut être trouvée dans la documentation officielle d'Elm.

## Plongée profonde

Maintenant que vous savez comment convertir une date en une chaîne, il est important de comprendre comment fonctionne cette conversion. En Elm, toutes les dates sont représentées en tant que nombres entiers. Plus précisément, elles sont représentées en tant que nombres de secondes écoulées depuis le 1er janvier 1970 à minuit, heure UTC.

Lorsque vous utilisez la fonction `Date.toString`, le format de chaîne que vous fournissez est utilisé pour formater la date en utilisant un objet `Date.Format`, qui contient les informations nécessaires telles que le séparateur entre jour, mois et année, ainsi que l'ordre dans lequel ils doivent apparaître dans la chaîne finale.

Il est également important de noter qu'en raison de la nature immuable de la programmation en Elm, la fonction `Date.toString` ne modifie pas la date originale et renvoie plutôt une nouvelle chaîne.

## Voir aussi

Pour en savoir plus sur les dates et les formats de chaîne en Elm, n'hésitez pas à consulter ces ressources :

- Documentation officielle d'Elm : <https://elm-lang.org/docs>
- Exemples de formats de chaîne : <https://package.elm-lang.org/packages/elm/time/latest/Time#toString>
- Tutoriel sur les dates en Elm : <https://becoming-functional.com/working-with-dates-in-elm-79cf3a6475ee>