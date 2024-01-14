---
title:    "Elm: Comparaison de deux dates"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de comparer deux dates en programmation pour effectuer différentes opérations telles que trier des données ou vérifier si une date est avant ou après une autre. Dans cet article, nous allons découvrir comment le langage Elm nous permet de comparer facilement deux dates.

## Comment faire

Pour comparer deux dates en Elm, nous pouvons utiliser la fonction `Date` intégrée au module `Time`. Voici un exemple de code qui compare deux dates :

```Elm
date1 = Date.everyDayAt 12 30
date2 = Date.fromCalendarDate 2021 Jul 10
result = Date.compare date1 date2
```

Dans ce code, nous créons deux dates différentes `date1` et `date2` en utilisant les constructeurs de date fournis par le module `Date`. Ensuite, nous utilisons la fonction `compare` pour comparer ces deux dates et stockons le résultat dans la variable `result`. Le résultat sera un `Order` qui peut être `LT`, `GT` ou `EQ` (pour "inférieur", "supérieur" ou "égal").

Voici une liste de toutes les fonctions de comparaison de dates disponibles dans le module `Date` :

- `Date.compare` : compare deux dates et retourne un `Order`.
- `Date.equals` : retourne `True` si deux dates sont égales, `False` sinon.
- `Date.lessThan` : retourne `True` si la première date est antérieure à la deuxième, `False` sinon.
- `Date.greaterThan` : retourne `True` si la première date est postérieure à la deuxième, `False` sinon.
- `Date.min` : retourne la date la plus ancienne parmi une liste de dates.
- `Date.max` : retourne la date la plus récente parmi une liste de dates.

## Plongée en profondeur

En plus de ces fonctions de comparaison, le module `Date` propose également des fonctions pour effectuer des opérations mathématiques sur des dates telles que `Date.add`, `Date.sub`, `Date.since` et `Date.until`. De plus, les dates peuvent être facilement converties en chaînes de caractères grâce aux fonctions `Date.toCivilString` et `Date.toIsoString`.

Il est également important de noter qu'en Elm, les dates sont immuables, ce qui signifie qu'une fois qu'une date est créée, elle ne peut pas être modifiée. Cela garantit une manipulation sûre des dates et évite les erreurs courantes telles que le décalage horaire.

Enfin, le module `Date` est fortement typé, ce qui signifie que toutes les opérations sur les dates ont des types spécifiques et sont vérifiées par le compilateur. Cela permet de détecter les erreurs de logique de comparaison de dates dès la phase de compilation plutôt qu'à l'exécution.

## Voir aussi

- La documentation complète du module `Date` : https://package.elm-lang.org/packages/elm/time/latest/Date
- Un tutoriel sur les dates en Elm : https://www.elm-tutorial.org/fr/07-dates.html
- Une introduction au langage Elm : https://www.elm-tutorial.org/fr/01-introduction.html