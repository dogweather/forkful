---
title:    "Elm: Comparaison de deux dates"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Pourquoi: La comparaison de deux dates en Elm

La comparaison de dates est un outil utile qui peut être utilisé dans divers projets de programmation. En utilisant le langage fonctionnel Elm, vous pouvez facilement comparer les dates pour déterminer si elles sont égales, après ou avant une autre date. Dans cet article, nous explorerons comment utiliser Elm pour comparer deux dates.

## Comment Faire

Pour commencer, nous devons d'abord définir nos deux dates à comparer en tant que variables. Dans l'exemple ci-dessous, nous utiliserons les dates 10 Octobre 2021 et 10 Octobre 2022:

```
Elm
import Time exposing (posixToMillis)

date1 : Int
date1 = posixToMillis 1633814400000

date2 : Int
date2 = posixToMillis 1665350400000
```

En utilisant la fonction `posixToMillis`, nous convertissons nos dates en millisecondes pour faciliter la comparaison. Ensuite, nous pouvons utiliser les opérateurs de comparaison tels que `==` pour vérifier si les dates sont égales, `<` pour vérifier si la première date est avant la seconde, ou `>` pour vérifier si la première date est après la seconde.

```
Elm
date1 == date2 -- false
date1 < date2 -- true
date1 > date2 -- false
```

Dans l'exemple ci-dessus, nous voyons que la première date est avant la seconde et donc `date1 < date2` renvoie `true`.

## Plongée en Profondeur

En comparant les dates en Elm, il est important de comprendre que chaque date est en fait un nombre de millisecondes depuis le 1er Janvier 1970. Cela signifie que si vous avez besoin de comparer des dates avec précision, vous devez également prendre en compte les fuseaux horaires et les années bissextiles.

De plus, il est important de noter que la fonction `posixToMillis` utilisée dans notre exemple suppose que les dates sont dans le fuseau horaire UTC. Si vous avez des dates dans d'autres fuseaux horaires, vous devrez peut-être utiliser une autre fonction de conversion.

## Voir Aussi

- La documentation officielle sur la comparaison de dates en Elm: https://package.elm-lang.org/packages/elm/time/latest/Time#posixToMillis
- Un tutoriel sur les opérateurs de comparaison en Elm: https://elmprogramming.com/conditionals.html
- Un article sur la conversion de dates en Elm: https://medium.com/@jacobworrel/parse-format-and-manipulate-dates-in-elm-71a86d72b640

---

# Voir Aussi