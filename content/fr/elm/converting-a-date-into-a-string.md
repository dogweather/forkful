---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Elm: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Les dates sont un élément essentiel de nombreuses applications et programmes, et pouvoir les convertir en chaînes de caractères est souvent nécessaire. Cela permet de les afficher de manière lisible pour l'utilisateur ou de les utiliser dans des requêtes HTTP. Dans cet article, nous allons voir comment convertir une date en chaîne de caractères en utilisant Elm.

## Comment faire

La fonction pour convertir une date en chaîne de caractères en Elm est appelée `Date.toString`. Voici un exemple de code pour l'utiliser :

```
import Date exposing (Date, toString)

-- Créer une date avec un objet Date contenant l'année, le mois et le jour
date = Date.fromCalendarDate 2021 11 18

-- Convertir la date en chaîne de caractères
str = toString date

-- Afficher la chaîne de caractères
main = text str
```

Lorsque vous exécutez ce code, vous obtiendrez le résultat suivant :

![Output Example](https://i.imgur.com/qniGfCQ.png)

Comme vous pouvez le voir, la fonction `toString` a converti la date en une chaîne de caractères facilement lisible pour un utilisateur. De plus, vous pouvez spécifier le format de la chaîne de caractères en utilisant le paramètre optionnel `locale` de la fonction. Par exemple, si vous voulez le format "mois/jour/année", vous pouvez utiliser `toString date { locale = "en-US" }`.

## Plongée en profondeur

La raison pour laquelle la conversion en chaîne de caractères peut sembler simple en Elm est que la bibliothèque standard fournit la fonction `Date.toString`. Cette fonction utilise en fait une bibliothèque externe appelée `elm/time` et utilise la fonction `toIsoStringWithOffset` pour effectuer la conversion.

En utilisant `toIsoStringWithOffset`, vous pouvez également spécifier un fuseau horaire pour la date, ce qui peut être utile si vous travaillez avec des dates dans différents fuseaux horaires.

## Voir aussi

- Documentation officielle d'Elm sur la date et le temps (https://package.elm-lang.org/packages/elm/time/latest)
- Exemples de code pour convertir une date en chaîne de caractères en Elm (https://elmprogramming.com/convert-date-to-string-in-elm.html)