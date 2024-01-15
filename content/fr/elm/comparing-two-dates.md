---
title:                "Comparer deux dates"
html_title:           "Elm: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez déjà dû comparer deux dates en programmation, vous savez peut-être à quel point cela peut être fastidieux et source d'erreurs. Heureusement, avec Elm, il existe une méthode simple et efficace pour comparer facilement deux dates.

## Comment faire

La première étape pour comparer deux dates en Elm est de les convertir en un format compréhensible pour le langage de programmation. Pour cela, nous pouvons utiliser la fonction `Time.fromString` qui prend une chaîne de caractères en tant que paramètre et renvoie une `Maybe Time` (une valeur pouvant être soit `Just Time`, soit `Nothing` en cas d'erreur de conversion).

```
Elm Time.fromString "2021-07-27"
```

Si nous voulons comparer des dates avec une précision plus fine, comme l'heure ou les millisecondes, nous pouvons également utiliser la fonction `Time.fromStringIso8601` qui prend un paramètre supplémentaire spécifiant le niveau de précision souhaité.

```
Elm Time.fromStringIso8601 "2021-07-27T12:30:00.000Z"
```

Une fois que nous avons nos deux dates dans un format compréhensible pour Elm, nous pouvons utiliser la fonction `Time.compare` pour les comparer. Cette fonction prend deux dates en tant que paramètres et renvoie `LT` (plus petite), `EQ` (égale) ou `GT` (plus grande), en fonction de leur ordre chronologique.

```
let date1 = Time.fromString "2021-07-27"
let date2 = Time.fromString "2021-07-28"
Time.compare date1 date2  -- renverra LT car date1 est plus petite que date2
```

Enfin, si nous voulons vérifier si une date est comprise entre deux autres dates, nous pouvons utiliser la fonction `Time.isBetween`. Cette fonction prend trois dates en tant que paramètres et renvoie `True` si la première date est comprise entre les deux autres dates et `False` sinon.

```
let date1 = Time.fromString "2021-07-27"
let date2 = Time.fromString "2021-07-28"
let date3 = Time.fromString "2021-07-29"
Time.isBetween date2 date1 date3  -- renverra True car date2 est comprise entre date1 et date3
```

## Deep Dive

Lorsque nous comparons des dates en Elm, il est important de noter que les dates sont comparées en utilisant le fuseau horaire UTC par défaut. Si nous voulons utiliser un fuseau horaire différent, nous devons d'abord convertir notre date en utilisant la fonction `Time.toUtc`.

De plus, la fonction `Time.compare` prend également en compte la précision jusqu'à la milliseconde lorsqu'elle compare les dates. Si nous ne voulons pas prendre en compte la précision des millisecondes, nous pouvons utiliser la fonction `Time.compareWithoutZone` qui ignore cette précision.

## Voir aussi

Pour en savoir plus sur la manipulation des dates en Elm, vous pouvez consulter ces ressources :

- [Documentation officielle Elm pour la manipulation des dates](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Article sur la comparaison de dates en Elm de dev.to](https://dev.to/benansell/comparing-dates-in-elm-1915)